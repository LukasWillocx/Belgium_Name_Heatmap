# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  Belgium Name Heatmap — A/B Comparison                                    ║
# ║  Modernised with luwitemplate theming & dark mode                         ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── Libraries ────────────────────────────────────────────────────────────────
library(luwitemplate)
library(bslib)
library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
library(leaflet)
library(markdown)
library(RPostgres)
library(sf)
library(DBI)
library(dplyr)

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  DATABASE FUNCTIONS                                                        ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── Color palettes (kept for map polygon coloring) ───────────────────────────
GROUP_A_COLORS <- c("#ffb3b3", "#ff8080", "#e74c3c", "#c0392b", "#8b0000")
GROUP_B_COLORS <- c("#aed6f1", "#5dade2", "#3498db", "#2874a6", "#1a5490")

# ── Connection ───────────────────────────────────────────────────────────────
get_db_connection <- function() {
  source("credentials.R", local = TRUE)
  tryCatch({
    dbConnect(
      Postgres(),
      dbname   = DB_CONFIG$dbname,
      host     = DB_CONFIG$host,
      port     = DB_CONFIG$port,
      user     = DB_CONFIG$user,
      password = DB_CONFIG$password
    )
  }, error = function(e) stop(paste("Database connection failed:", e$message)))
}

# ── Name queries ─────────────────────────────────────────────────────────────
get_all_names <- function(conn, gender = NULL) {
  if (is.null(gender) || gender == "") {
    query <- "SELECT DISTINCT name FROM name_frequencies WHERE year = 2025 ORDER BY name"
  } else {
    query <- sprintf(
      "SELECT DISTINCT name FROM name_frequencies WHERE year = 2025 AND gender = '%s' ORDER BY name",
      gender
    )
  }
  result <- dbGetQuery(conn, query)
  setNames(result$name, result$name)
}

get_name_heatmap_data_detailed <- function(conn, names, group_id = "a") {
  if (length(names) == 0) return(NULL)
  names_sql <- paste0("'", gsub("'", "''", names), "'", collapse = ", ")
  
  query <- sprintf(
    "SELECT
       m.id as municipality_id,
       m.name as municipality_name,
       COALESCE(ST_AsText(m.geometry_simplified), ST_AsText(m.geometry)) as wkt,
       SUM(nf.frequency) as frequency,
       STRING_AGG(nf.name || ': ' || nf.frequency, '; ' ORDER BY nf.frequency DESC) as name_breakdown
     FROM municipalities m
     LEFT JOIN name_frequencies nf ON nf.municipality_id = m.id
     WHERE nf.year = 2025
       AND nf.name IN (%s)
     GROUP BY m.id, m.name, m.geometry, m.geometry_simplified
     HAVING SUM(nf.frequency) > 0
     ORDER BY m.name", names_sql)
  
  result <- dbGetQuery(conn, query)
  if (nrow(result) == 0) return(NULL)
  
  geom <- st_as_sfc(result$wkt, crs = 4326)
  st_sf(
    municipality_id   = result$municipality_id,
    municipality_name = result$municipality_name,
    frequency         = result$frequency,
    name_breakdown    = result$name_breakdown,
    geometry          = geom
  )
}

get_name_statistics <- function(conn, names) {
  if (length(names) == 0) return(NULL)
  names_sql <- paste0("'", gsub("'", "''", names), "'", collapse = ", ")
  
  query <- sprintf(
    "SELECT
       nf.name, nf.gender,
       SUM(nf.frequency) as total_count,
       COUNT(DISTINCT nf.municipality_id) as municipality_count,
       (SUM(nf.frequency)::numeric / 11500000.0) * 100 as national_percentage
     FROM name_frequencies nf
     WHERE nf.year = 2025 AND nf.name IN (%s)
     GROUP BY nf.name, nf.gender
     ORDER BY nf.name", names_sql)
  
  stats <- dbGetQuery(conn, query)
  if (nrow(stats) == 0) return(NULL)
  
  regional_query <- sprintf(
    "SELECT
       nf.name, r.name as region_name,
       SUM(nf.frequency) as count
     FROM name_frequencies nf
     JOIN municipalities m ON nf.municipality_id = m.id
     JOIN provinces p ON m.province_id = p.id
     JOIN regions r ON p.region_id = r.id
     WHERE nf.year = 2025 AND nf.name IN (%s)
     GROUP BY nf.name, r.name
     ORDER BY nf.name, r.name", names_sql)
  
  regional <- dbGetQuery(conn, regional_query)
  
  if (nrow(regional) > 0) {
    regional_list <- split(regional, regional$name)
    stats$top_regions <- sapply(stats$name, function(n) {
      if (n %in% names(regional_list)) {
        reg_data <- regional_list[[n]]
        paste(paste0(reg_data$region_name, ": ", reg_data$count), collapse = "; ")
      } else ""
    })
  } else {
    stats$top_regions <- ""
  }
  stats
}

# ── Data processing ──────────────────────────────────────────────────────────
blend_colors <- function(freq_a, freq_b, intensity) {
  red  <- col2rgb("#e74c3c")
  blue <- col2rgb("#3498db")
  total <- freq_a + freq_b
  ratio <- freq_b / total
  blended <- red * (1 - ratio) + blue * ratio
  blended <- blended * intensity
  rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
}

create_label_text <- function(muni_name, freq_a, freq_b, breakdown_a, breakdown_b, total_freq) {
  parts <- paste0("<strong style='font-size: 14px;'>", muni_name, "</strong>")
  
  if (freq_a > 0) {
    parts <- c(parts, paste0(
      "<span style='color: #e74c3c; font-weight: 600;'>Group A (", freq_a, " total):</span>"))
    if (!is.na(breakdown_a) && breakdown_a != "") {
      for (item in strsplit(breakdown_a, "; ")[[1]])
        parts <- c(parts, paste0(
          "<span style='color: #e74c3c; font-size: 12px; padding-left: 8px;'>\u2022 ", item, "</span>"))
    }
  }
  
  if (freq_b > 0) {
    if (freq_a > 0) parts <- c(parts, "<span style='font-size: 4px;'> </span>")
    parts <- c(parts, paste0(
      "<span style='color: #3498db; font-weight: 600;'>Group B (", freq_b, " total):</span>"))
    if (!is.na(breakdown_b) && breakdown_b != "") {
      for (item in strsplit(breakdown_b, "; ")[[1]])
        parts <- c(parts, paste0(
          "<span style='color: #3498db; font-size: 12px; padding-left: 8px;'>\u2022 ", item, "</span>"))
    }
  }
  
  if (freq_a > 0 && freq_b > 0) {
    pct_a <- round(100 * freq_a / total_freq, 1)
    pct_b <- round(100 * freq_b / total_freq, 1)
    parts <- c(parts, "<span style='font-size: 4px;'> </span>")
    parts <- c(parts, paste0(
      "<span style='color: #666; font-size: 11px; font-style: italic;'>",
      "Overall Ratio: ", pct_a, "% / ", pct_b, "%</span>"))
  }
  paste(parts, collapse = "<br/>")
}

merge_group_data <- function(data_a, data_b) {
  all_munis <- c()
  if (!is.null(data_a) && nrow(data_a) > 0) all_munis <- c(all_munis, data_a$municipality_id)
  if (!is.null(data_b) && nrow(data_b) > 0) all_munis <- c(all_munis, data_b$municipality_id)
  all_munis <- unique(all_munis)
  if (length(all_munis) == 0) return(NULL)
  
  combined_data <- data.frame(municipality_id = all_munis)
  
  if (!is.null(data_a) && nrow(data_a) > 0) {
    data_a_df <- st_drop_geometry(data_a)
    combined_data <- merge(combined_data,
                           data_a_df[, c("municipality_id", "municipality_name", "frequency", "name_breakdown")],
                           by = "municipality_id", all.x = TRUE, suffixes = c("", "_a"))
    names(combined_data)[names(combined_data) == "frequency"]      <- "freq_a"
    names(combined_data)[names(combined_data) == "name_breakdown"] <- "breakdown_a"
    names(combined_data)[names(combined_data) == "municipality_name"] <- "muni_name"
  } else {
    combined_data$freq_a      <- 0
    combined_data$breakdown_a <- ""
  }
  
  if (!is.null(data_b) && nrow(data_b) > 0) {
    data_b_df <- st_drop_geometry(data_b)
    combined_data <- merge(combined_data,
                           data_b_df[, c("municipality_id", "municipality_name", "frequency", "name_breakdown")],
                           by = "municipality_id", all.x = TRUE, suffixes = c("", "_b"))
    names(combined_data)[names(combined_data) == "frequency"]      <- "freq_b"
    names(combined_data)[names(combined_data) == "name_breakdown"] <- "breakdown_b"
    if (!"muni_name" %in% names(combined_data)) {
      names(combined_data)[names(combined_data) == "municipality_name"] <- "muni_name"
    } else {
      combined_data$muni_name <- ifelse(
        is.na(combined_data$muni_name), combined_data$municipality_name, combined_data$muni_name)
      combined_data$municipality_name <- NULL
    }
  } else {
    combined_data$freq_b      <- 0
    combined_data$breakdown_b <- ""
  }
  
  # Attach geometries
  if (!is.null(data_a) && nrow(data_a) > 0) {
    geom_data <- data_a[match(combined_data$municipality_id, data_a$municipality_id), ]
    combined_data$geometry <- st_geometry(geom_data)
    if (!is.null(data_b) && nrow(data_b) > 0) {
      missing_geom <- is.na(st_dimension(combined_data$geometry))
      if (any(missing_geom)) {
        geom_b <- data_b[match(combined_data$municipality_id[missing_geom], data_b$municipality_id), ]
        combined_data$geometry[missing_geom] <- st_geometry(geom_b)
      }
    }
  } else if (!is.null(data_b) && nrow(data_b) > 0) {
    geom_data <- data_b[match(combined_data$municipality_id, data_b$municipality_id), ]
    combined_data$geometry <- st_geometry(geom_data)
  }
  
  combined_data$freq_a[is.na(combined_data$freq_a)] <- 0
  combined_data$freq_b[is.na(combined_data$freq_b)] <- 0
  combined_data$total_freq <- combined_data$freq_a + combined_data$freq_b
  max_total <- max(combined_data$total_freq, na.rm = TRUE)
  
  combined_data$color <- sapply(1:nrow(combined_data), function(i) {
    fa <- combined_data$freq_a[i]; fb <- combined_data$freq_b[i]
    total <- combined_data$total_freq[i]
    intensity <- 0.4 + (0.6 * (total / max_total))
    if (fa > 0 && fb > 0) {
      blend_colors(fa, fb, intensity)
    } else if (fa > 0) {
      b <- col2rgb("#e74c3c") * intensity
      rgb(b[1], b[2], b[3], maxColorValue = 255)
    } else {
      b <- col2rgb("#3498db") * intensity
      rgb(b[1], b[2], b[3], maxColorValue = 255)
    }
  })
  
  combined_data$label_text <- sapply(1:nrow(combined_data), function(i) {
    create_label_text(
      combined_data$muni_name[i], combined_data$freq_a[i], combined_data$freq_b[i],
      combined_data$breakdown_a[i], combined_data$breakdown_b[i], combined_data$total_freq[i])
  })
  
  st_sf(combined_data, crs = 4326)
}

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  CHART BUILDERS — ggplot2 + luwi_ggplotly                                 ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

create_comparison_chart <- function(stats_a, stats_b, theme) {
  plot_data <- data.frame()
  if (!is.null(stats_a)) {
    plot_data <- rbind(plot_data, data.frame(
      name = stats_a$name, count = stats_a$total_count, group = "Group A",
      stringsAsFactors = FALSE))
  }
  if (!is.null(stats_b)) {
    plot_data <- rbind(plot_data, data.frame(
      name = stats_b$name, count = stats_b$total_count, group = "Group B",
      stringsAsFactors = FALSE))
  }
  if (nrow(plot_data) == 0) return(NULL)
  
  # Build color mapping
  unique_a <- if (!is.null(stats_a)) stats_a$name else character(0)
  unique_b <- if (!is.null(stats_b)) stats_b$name else character(0)
  color_map <- c()
  if (length(unique_a) > 0) color_map <- c(color_map, setNames(GROUP_A_COLORS[seq_along(unique_a)], unique_a))
  if (length(unique_b) > 0) color_map <- c(color_map, setNames(GROUP_B_COLORS[seq_along(unique_b)], unique_b))
  
  p <- ggplot(plot_data, aes(x = group, y = count, fill = name,
                             text = paste0(name, "\nCount: ", format(count, big.mark = ",")))) +
    geom_col(position = "stack") +
    scale_fill_manual(values = color_map) +
    labs(x = NULL, y = NULL) +
    theme_luwi(theme = theme) +
    theme(legend.position = "none")
  
  luwi_ggplotly(p, theme = theme, tooltip = "text") %>%
    layout(showlegend = FALSE,
           yaxis = list(side = "right")) %>%
    config(displayModeBar = FALSE)
}

create_regional_chart <- function(stats_a, stats_b, theme) {
  regional_data <- data.frame()
  
  parse_regions <- function(stats, group_label) {
    rows <- data.frame()
    if (!is.null(stats)) {
      for (i in seq_len(nrow(stats))) {
        if (stats$top_regions[i] != "") {
          regions <- strsplit(stats$top_regions[i], "; ")[[1]]
          for (r in regions) {
            parts <- strsplit(r, ": ")[[1]]
            if (length(parts) == 2)
              rows <- rbind(rows, data.frame(
                region = parts[1], count = as.numeric(parts[2]),
                group = group_label, stringsAsFactors = FALSE))
          }
        }
      }
    }
    rows
  }
  
  regional_data <- rbind(parse_regions(stats_a, "Group A"), parse_regions(stats_b, "Group B"))
  if (nrow(regional_data) == 0) return(NULL)
  
  regional_agg <- aggregate(count ~ region + group, data = regional_data, FUN = sum)
  
  # Title-case for axis labels
  regional_agg$region_label <- sapply(regional_agg$region, function(name) {
    words <- strsplit(tolower(name), " ")[[1]]
    paste(sapply(words, function(w) paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))),
          collapse = "\n")
  })
  
  p <- ggplot(regional_agg, aes(x = region_label, y = count, fill = group,
                                text = paste0(region, "\n", group, ": ", format(count, big.mark = ",")))) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("Group A" = "#e74c3c", "Group B" = "#3498db")) +
    labs(x = NULL, y = NULL) +
    theme_luwi(theme = theme) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 8, lineheight = 0.9))
  
  luwi_ggplotly(p, theme = theme, tooltip = "text") %>%
    layout(showlegend = FALSE,
           yaxis = list(side = "right")) %>%
    config(displayModeBar = FALSE)
}

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  CUSTOM CSS — replaces www/styles.css                                      ║
# ║  Uses bslib CSS variables for automatic dark mode reactivity               ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

panel_css <- tags$style(HTML("
  /* ── Full-viewport map ─────────────────────────────────────────────── */
  .map-container { position: relative; width: 100%; height: 100vh; }
  .map-container .leaflet-container { width: 100%; height: 100%; }

  /* ── Floating hover panels — vertically centered ───────────────────── */
  .floating-panel {
    position: absolute;
    top: 50%;
    transform: translateY(-50%);
    width: 320px;
    max-height: calc(100vh - 40px);
    overflow-y: auto;
    z-index: 1000;
    background: var(--bs-body-bg);
    color: var(--bs-body-color);
    border: 1px solid var(--bs-border-color);
    border-radius: 12px;
    padding: 20px;
    box-shadow: 0 4px 24px rgba(0, 0, 0, 0.12);
    transition: transform 0.35s cubic-bezier(0.4, 0, 0.2, 1),
                opacity 0.35s cubic-bezier(0.4, 0, 0.2, 1);
  }
  .left-panel  { left: 12px; }
  .right-panel { right: 12px; width: 385px; }

  .floating-panel.closed-left  { transform: translate(-360px, -50%); opacity: 0; pointer-events: none; }
  .floating-panel.closed-right { transform: translate(420px, -50%);  opacity: 0; pointer-events: none; }

  /* ── Toggle buttons (show when panel hidden) — vertically centered ── */
  .panel-toggle-btn {
    position: absolute;
    top: 50%;
    transform: translateY(-50%);
    z-index: 1001;
    width: 40px; height: 40px;
    border: 1px solid var(--bs-border-color);
    border-radius: 10px;
    background: var(--bs-body-bg);
    color: var(--bs-body-color);
    font-size: 18px;
    cursor: pointer;
    display: flex; align-items: center; justify-content: center;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    transition: opacity 0.25s, transform 0.25s;
  }
  .panel-toggle-btn.hidden { opacity: 0; pointer-events: none; transform: translateY(-50%) scale(0.8); }
  .toggle-left  { left: 12px; }
  .toggle-right { right: 12px; }

  /* ── Panel header row — close + dark mode toggle ────────────────────── */
  .panel-header-row {
    display: flex; align-items: center; justify-content: space-between;
    margin-bottom: 4px;
  }
  .panel-header-actions {
    display: flex; align-items: center; gap: 6px;
    margin-left: auto;
  }
  .panel-header-actions .form-check.form-switch {
    margin: 0; padding-left: 2.5em;
  }

  /* ── Close button inside panels ────────────────────────────────────── */
  .close-panel-btn {
    background: none; border: none;
    font-size: 22px; line-height: 1; cursor: pointer;
    color: var(--bs-body-color);
    opacity: 0.6; padding: 0;
  }
  .close-panel-btn:hover { opacity: 1; }

  /* ── Panel typography ──────────────────────────────────────────────── */
  .panel-title {
    font-size: 1.15rem; font-weight: 700;
    margin-bottom: 16px; padding-bottom: 8px;
    border-bottom: 2px solid var(--bs-primary);
  }
  .filter-group { margin-bottom: 14px; }
  .filter-group label { font-size: 0.85rem; font-weight: 600; margin-bottom: 4px; }

  /* ── Group summary boxes ───────────────────────────────────────────── */
  .comparison-grid { display: flex; gap: 10px; }
  .group-box {
    flex: 1; padding: 12px; border-radius: 10px; text-align: center;
    background: var(--bs-tertiary-bg);
    border: 1px solid var(--bs-border-color);
  }
  .group-box-header { font-size: 0.8rem; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px; }
  .group-box-count  { font-size: 1.5rem; font-weight: 700; margin: 4px 0; }
  .group-box-names  { font-size: 0.78rem; opacity: 0.8; word-break: break-word; }
  .group-box-empty  { font-size: 0.82rem; opacity: 0.5; padding: 8px 0; }

  .group-a-box .group-box-header, .group-a-box .group-box-count { color: #e74c3c; }
  .group-b-box .group-box-header, .group-b-box .group-box-count { color: #3498db; }

  /* ── Plotly in floating panel — prevent overflow ────────────────────── */
  .floating-panel .plotly { width: 100% !important; }
  .floating-panel .js-plotly-plot { width: 100% !important; }

  /* ── Selectize inputs — inherit theme ──────────────────────────────── */
  .selectize-input, .selectize-dropdown {
    background: var(--bs-body-bg) !important;
    color: var(--bs-body-color) !important;
    border-color: var(--bs-border-color) !important;
  }
  .selectize-dropdown-content .option.active {
    background: var(--bs-primary) !important;
    color: #fff !important;
  }

  /* ── Custom scrollbar for panels ───────────────────────────────────── */
  .floating-panel::-webkit-scrollbar { width: 6px; }
  .floating-panel::-webkit-scrollbar-track { background: transparent; }
  .floating-panel::-webkit-scrollbar-thumb {
    background: var(--bs-border-color); border-radius: 3px;
  }

  /* ── No-navbar: body margin reset ────────────────────────────────────── */
  body { margin: 0; padding: 0; overflow: hidden; }

  /* ── About button — ensure visibility in dark mode ───────────────────── */
  [data-bs-theme='dark'] .btn-outline-primary {
    color: var(--bs-primary);
    border-color: var(--bs-primary);
  }

  /* ── About modal inherits theme ────────────────────────────────────── */
  .about-modal-content img { max-width: 100%; height: auto; display: block; margin: 15px auto; }
  .about-modal-content     { line-height: 1.6; }
  .about-modal-content h1  { margin-top: 0; }
  .about-modal-content h2  { margin-top: 20px; border-bottom: 1px solid var(--bs-border-color); padding-bottom: 5px; }
"))

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  UI                                                                        ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

ui <- bslib::page_fillable(
  theme = my_theme(),
  dark_mode_css(),
  panel_css,
  title = "Name Heatmap",
  padding = 0,
  fillable_mobile = TRUE,
  
  useShinyjs(),
  
  tags$div(
    class = "map-container",
    
    # ── Toggle buttons (visible when panel is closed) ──
    tags$button(class = "panel-toggle-btn toggle-left hidden",
                id = "sidebarToggle", "\u2630"),
    tags$button(class = "panel-toggle-btn toggle-right hidden",
                id = "metricsToggle", "\U0001F4CA"),
    
    # ── Left floating panel — name comparison controls ──
    tags$div(
      class = "floating-panel left-panel", id = "leftPanel",
      tags$div(class = "panel-header-row",
               tags$div(class = "panel-title", style = "margin-bottom: 0; padding-bottom: 0; border: none;",
                        "Name Comparison"),
               tags$div(class = "panel-header-actions",
                        tags$button(class = "close-panel-btn", id = "closeLeftPanel", "\u00D7"))
      ),
      tags$hr(style = "margin: 8px 0 14px; border-color: var(--bs-primary); border-width: 2px;"),
      
      tags$div(
        class = "filter-group",
        tags$label("Group A", style = "color: #e74c3c;"),
        selectizeInput("names_a", NULL, choices = NULL, multiple = TRUE,
                       options = list(placeholder = "Type to search...", maxItems = 5))
      ),
      
      tags$div(
        class = "filter-group",
        tags$label("Group B", style = "color: #3498db;"),
        selectizeInput("names_b", NULL, choices = NULL, multiple = TRUE,
                       options = list(placeholder = "Type to search...", maxItems = 5))
      ),
      
      tags$div(class = "filter-group",
               actionButton("reset", "Clear Selection", class = "btn-secondary btn-block w-100")),
      tags$div(class = "filter-group",
               actionButton("aboutBtn", "About", class = "btn-outline-primary btn-block w-100",
                            icon = icon("info-circle")))
    ),
    
    # ── Right floating panel — comparison statistics ──
    tags$div(
      class = "floating-panel right-panel", id = "rightPanel",
      tags$div(class = "panel-header-row",
               tags$div(class = "panel-title", style = "margin-bottom: 0; padding-bottom: 0; border: none;",
                        "Comparison Statistics"),
               tags$div(class = "panel-header-actions",
                        input_dark_mode(id = "dark_mode"),
                        tags$button(class = "close-panel-btn", id = "closeRightPanel", "\u00D7"))
      ),
      tags$hr(style = "margin: 8px 0 14px; border-color: var(--bs-primary); border-width: 2px;"),
      
      conditionalPanel(
        condition = "output.show_metrics",
        uiOutput("metrics_display")
      )
    ),
    
    # ── Map ──
    leafletOutput("map", width = "100%", height = "100%")
  ),
  
  # ── Panel toggle JS ──
  tags$script(HTML("
    $(document).ready(function() {
      $('#closeLeftPanel').click(function() {
        $('#leftPanel').addClass('closed-left');
        $('#sidebarToggle').removeClass('hidden');
      });
      $('#sidebarToggle').click(function() {
        $('#leftPanel').removeClass('closed-left');
        $('#sidebarToggle').addClass('hidden');
      });
      $('#closeRightPanel').click(function() {
        $('#rightPanel').addClass('closed-right');
        $('#metricsToggle').removeClass('hidden');
      });
      $('#metricsToggle').click(function() {
        $('#rightPanel').removeClass('closed-right');
        $('#metricsToggle').addClass('hidden');
      });
    });
  "))
)

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  SERVER                                                                    ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

server <- function(input, output, session) {
  
  # ── Database connection ──
  conn <- get_db_connection()
  onStop(function() dbDisconnect(conn))
  
  # ── Dark mode: luwitemplate reactive ──
  colors <- get_theme_colors()
  dm     <- use_dark_mode(input, session)
  
  # ══════════════════════════════════════════════════════════════════════════
  # ABOUT MODAL
  # ══════════════════════════════════════════════════════════════════════════
  
  observeEvent(input$aboutBtn, {
    about_content <- tryCatch({
      if (file.exists("about.md")) {
        markdownToHTML(file = "about.md", fragment.only = TRUE)
      } else {
        "<p>About content not found. Please create an <code>about.md</code> file.</p>"
      }
    }, error = function(e) {
      paste("<p>Error loading about content:</p><pre>", e$message, "</pre>")
    })
    
    showModal(modalDialog(
      title     = "About This Application",
      tags$div(class = "about-modal-content", HTML(about_content)),
      easyClose = TRUE,
      footer    = modalButton("Close"),
      size      = "l"
    ))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # NAME AUTOCOMPLETE
  # ══════════════════════════════════════════════════════════════════════════
  
  all_names <- reactive(get_all_names(conn, NULL))
  
  observe({
    names_list <- all_names()
    updateSelectizeInput(session, "names_a", choices = names_list, server = TRUE)
    updateSelectizeInput(session, "names_b", choices = names_list, server = TRUE)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # MAP
  # ══════════════════════════════════════════════════════════════════════════
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 4.5, lat = 50.5, zoom = 8)
  })
  
  # ── Swap tiles on dark mode toggle ──
  observe({
    tile <- if (dm$mode() == "dark") providers$CartoDB.DarkMatter else providers$CartoDB.Positron
    leafletProxy("map") %>% clearTiles() %>% addProviderTiles(tile)
  })
  
  # ── Debounced name inputs ──
  names_a_debounced <- debounce(reactive(input$names_a), 500)
  names_b_debounced <- debounce(reactive(input$names_b), 500)
  
  # ── Update polygons ──
  observe({
    na <- names_a_debounced()
    nb <- names_b_debounced()
    
    if (length(na) == 0 && length(nb) == 0) {
      leafletProxy("map") %>% clearShapes() %>% clearControls()
      return()
    }
    
    data_a <- NULL; data_b <- NULL
    tryCatch({ if (length(na) > 0) data_a <- get_name_heatmap_data_detailed(conn, na, "a") },
             error = function(e) showNotification(paste("Group A error:", e$message), type = "error"))
    tryCatch({ if (length(nb) > 0) data_b <- get_name_heatmap_data_detailed(conn, nb, "b") },
             error = function(e) showNotification(paste("Group B error:", e$message), type = "error"))
    
    map_proxy <- leafletProxy("map") %>% clearShapes() %>% clearControls()
    combined_sf <- merge_group_data(data_a, data_b)
    if (is.null(combined_sf)) return()
    
    map_proxy %>%
      addPolygons(
        data = combined_sf,
        fillColor = ~color, fillOpacity = 0.75,
        color = ~color, weight = 1.5,
        label = ~lapply(label_text, htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "3px 8px"),
          textsize = "12px", direction = "auto"),
        highlightOptions = highlightOptions(
          weight = 3, color = "#fff", fillOpacity = 0.85, bringToFront = TRUE)
      )
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # RESET
  # ══════════════════════════════════════════════════════════════════════════
  
  observeEvent(input$reset, {
    updateSelectizeInput(session, "names_a", selected = character(0))
    updateSelectizeInput(session, "names_b", selected = character(0))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # METRICS / STATISTICS PANEL
  # ══════════════════════════════════════════════════════════════════════════
  
  output$show_metrics <- reactive({
    length(input$names_a) > 0 || length(input$names_b) > 0
  })
  outputOptions(output, "show_metrics", suspendWhenHidden = FALSE)
  
  output$metrics_display <- renderUI({
    if (length(input$names_a) == 0 && length(input$names_b) == 0)
      return(tags$p("Select names to compare"))
    
    stats_a <- if (length(input$names_a) > 0) get_name_statistics(conn, input$names_a) else NULL
    stats_b <- if (length(input$names_b) > 0) get_name_statistics(conn, input$names_b) else NULL
    
    tagList(
      # ── Summary boxes ──
      tags$div(class = "comparison-grid",
               if (!is.null(stats_a)) {
                 tags$div(class = "group-box group-a-box",
                          tags$div(class = "group-box-header", "Group A"),
                          tags$div(class = "group-box-count", format(sum(stats_a$total_count), big.mark = ",")),
                          tags$div(class = "group-box-names", paste(input$names_a, collapse = ", "))
                 )
               } else {
                 tags$div(class = "group-box group-a-box empty",
                          tags$div(class = "group-box-header", "Group A"),
                          tags$div(class = "group-box-empty", "No names selected"))
               },
               if (!is.null(stats_b)) {
                 tags$div(class = "group-box group-b-box",
                          tags$div(class = "group-box-header", "Group B"),
                          tags$div(class = "group-box-count", format(sum(stats_b$total_count), big.mark = ",")),
                          tags$div(class = "group-box-names", paste(input$names_b, collapse = ", "))
                 )
               } else {
                 tags$div(class = "group-box group-b-box empty",
                          tags$div(class = "group-box-header", "Group B"),
                          tags$div(class = "group-box-empty", "No names selected"))
               }
      ),
      
      tags$hr(style = "margin: 16px 0; border-color: var(--bs-border-color);"),
      
      # ── Frequency comparison (ggplot2 + luwi_ggplotly) ──
      if (!is.null(stats_a) || !is.null(stats_b)) {
        tags$div(
          tags$h5("Frequency Comparison", style = "margin-bottom: 10px;"),
          plotlyOutput("comparison_chart", height = "280px")
        )
      },
      
      tags$hr(style = "margin: 16px 0; border-color: var(--bs-border-color);"),
      
      # ── Regional distribution (ggplot2 + luwi_ggplotly) ──
      if (!is.null(stats_a) || !is.null(stats_b)) {
        tags$div(
          tags$h5("Regional Distribution", style = "margin-bottom: 10px;"),
          plotlyOutput("regional_chart", height = "240px")
        )
      }
    )
  })
  
  # ╔════════════════════════════════════════════════════════════════════════╗
  # ║ PLOTLY OUTPUTS — ggplot2 + luwi_ggplotly                            ║
  # ║ theme = dm$theme() ensures reactivity to dark mode toggle           ║
  # ╚════════════════════════════════════════════════════════════════════════╝
  
  output$comparison_chart <- renderPlotly({
    stats_a <- if (length(input$names_a) > 0) get_name_statistics(conn, input$names_a) else NULL
    stats_b <- if (length(input$names_b) > 0) get_name_statistics(conn, input$names_b) else NULL
    create_comparison_chart(stats_a, stats_b, theme = dm$theme())
  })
  
  output$regional_chart <- renderPlotly({
    stats_a <- if (length(input$names_a) > 0) get_name_statistics(conn, input$names_a) else NULL
    stats_b <- if (length(input$names_b) > 0) get_name_statistics(conn, input$names_b) else NULL
    create_regional_chart(stats_a, stats_b, theme = dm$theme())
  })
}

# ── Run ──────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)