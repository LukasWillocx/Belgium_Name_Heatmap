# ==============================================================================
# NAME HEATMAP - DATABASE FUNCTIONS
# ==============================================================================

library(RPostgres)
library(sf)
library(DBI)
library(plotly)

# ==============================================================================
# COLOR PALETTES
# ==============================================================================

# Custom 5-color palette for Group A (light to dark reds)
GROUP_A_COLORS <- c(
  "#ffb3b3",  # Light red
  "#ff8080",  # Medium-light red
  "#e74c3c",  # Base red
  "#c0392b",  # Dark red
  "#8b0000"   # Very dark red
)

# Custom 5-color palette for Group B (light to dark blues)
GROUP_B_COLORS <- c(
  "#aed6f1",  # Light blue
  "#5dade2",  # Medium-light blue
  "#3498db",  # Base blue
  "#2874a6",  # Dark blue
  "#1a5490"   # Very dark blue
)

# ==============================================================================
# CONNECTION
# ==============================================================================

get_db_connection <- function() {
  source("credentials.R", local = TRUE)
  tryCatch({
    conn <- dbConnect(
      Postgres(),
      dbname = DB_CONFIG$dbname,
      host = DB_CONFIG$host,
      port = DB_CONFIG$port,
      user = DB_CONFIG$user,
      password = DB_CONFIG$password
    )
    return(conn)
  }, error = function(e) {
    stop(paste("Database connection failed:", e$message))
  })
}

# ==============================================================================
# NAME QUERIES
# ==============================================================================

# Get all unique names for autocomplete (filtered by gender if specified)
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

# Get heatmap data with individual name breakdown for hover labels
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
     ORDER BY m.name",
    names_sql
  )
  
  result <- dbGetQuery(conn, query)
  if (nrow(result) == 0) return(NULL)
  
  geom <- st_as_sfc(result$wkt, crs = 4326)
  st_sf(
    municipality_id = result$municipality_id,
    municipality_name = result$municipality_name,
    frequency = result$frequency,
    name_breakdown = result$name_breakdown,
    geometry = geom
  )
}

# Get statistics for selected name(s)
get_name_statistics <- function(conn, names) {
  if (length(names) == 0) return(NULL)
  
  names_sql <- paste0("'", gsub("'", "''", names), "'", collapse = ", ")
  
  query <- sprintf(
    "SELECT 
       nf.name,
       nf.gender,
       SUM(nf.frequency) as total_count,
       COUNT(DISTINCT nf.municipality_id) as municipality_count,
       (SUM(nf.frequency)::numeric / 11500000.0) * 100 as national_percentage
     FROM name_frequencies nf
     WHERE nf.year = 2025 AND nf.name IN (%s)
     GROUP BY nf.name, nf.gender
     ORDER BY nf.name",
    names_sql
  )
  
  stats <- dbGetQuery(conn, query)
  
  if (nrow(stats) == 0) return(NULL)
  
  # Get regional breakdown
  regional_query <- sprintf(
    "SELECT 
       nf.name,
       r.name as region_name,
       SUM(nf.frequency) as count
     FROM name_frequencies nf
     JOIN municipalities m ON nf.municipality_id = m.id
     JOIN provinces p ON m.province_id = p.id
     JOIN regions r ON p.region_id = r.id
     WHERE nf.year = 2025 AND nf.name IN (%s)
     GROUP BY nf.name, r.name
     ORDER BY nf.name, r.name",
    names_sql
  )
  
  regional <- dbGetQuery(conn, regional_query)
  
  # Build regional strings
  if (nrow(regional) > 0) {
    regional_list <- split(regional, regional$name)
    stats$top_regions <- sapply(stats$name, function(n) {
      if (n %in% names(regional_list)) {
        reg_data <- regional_list[[n]]
        paste(paste0(reg_data$region_name, ": ", reg_data$count), collapse = "; ")
      } else {
        ""
      }
    })
  } else {
    stats$top_regions <- ""
  }
  
  stats
}

# ==============================================================================
# DATA PROCESSING FUNCTIONS
# ==============================================================================

# Helper function to blend colors based on frequency ratio
blend_colors <- function(freq_a, freq_b, intensity) {
  red <- col2rgb("#e74c3c")
  blue <- col2rgb("#3498db")
  
  total <- freq_a + freq_b
  ratio <- freq_b / total
  
  blended <- red * (1 - ratio) + blue * ratio
  blended <- blended * intensity
  
  rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
}

# Create label text for map tooltips
create_label_text <- function(muni_name, freq_a, freq_b, breakdown_a, breakdown_b, total_freq) {
  parts <- c(paste0("<strong style='font-size: 14px;'>", muni_name, "</strong>"))
  
  if (freq_a > 0) {
    parts <- c(parts, paste0("<span style='color: #e74c3c; font-weight: 600;'>Group A (", 
                             freq_a, " total):</span>"))
    
    if (!is.na(breakdown_a) && breakdown_a != "") {
      breakdown <- strsplit(breakdown_a, "; ")[[1]]
      for (item in breakdown) {
        parts <- c(parts, paste0("<span style='color: #e74c3c; font-size: 12px; padding-left: 8px;'>• ", item, "</span>"))
      }
    }
  }
  
  if (freq_b > 0) {
    if (freq_a > 0) {
      parts <- c(parts, "<span style='font-size: 4px;'> </span>")
    }
    
    parts <- c(parts, paste0("<span style='color: #3498db; font-weight: 600;'>Group B (", 
                             freq_b, " total):</span>"))
    
    if (!is.na(breakdown_b) && breakdown_b != "") {
      breakdown <- strsplit(breakdown_b, "; ")[[1]]
      for (item in breakdown) {
        parts <- c(parts, paste0("<span style='color: #3498db; font-size: 12px; padding-left: 8px;'>• ", item, "</span>"))
      }
    }
  }
  
  if (freq_a > 0 && freq_b > 0) {
    pct_a <- round(100 * freq_a / total_freq, 1)
    pct_b <- round(100 * freq_b / total_freq, 1)
    parts <- c(parts, "<span style='font-size: 4px;'> </span>")
    parts <- c(parts, paste0("<span style='color: #666; font-size: 11px; font-style: italic;'>",
                             "Overall Ratio: ", pct_a, "% / ", pct_b, "%</span>"))
  }
  
  paste(parts, collapse = "<br/>")
}

# Merge and process data from both groups for map display
merge_group_data <- function(data_a, data_b) {
  # Get all unique municipalities
  all_munis <- c()
  if (!is.null(data_a) && nrow(data_a) > 0) all_munis <- c(all_munis, data_a$municipality_id)
  if (!is.null(data_b) && nrow(data_b) > 0) all_munis <- c(all_munis, data_b$municipality_id)
  all_munis <- unique(all_munis)
  
  if (length(all_munis) == 0) return(NULL)
  
  # Create combined dataset
  combined_data <- data.frame(municipality_id = all_munis)
  
  # Add Group A data
  if (!is.null(data_a) && nrow(data_a) > 0) {
    data_a_df <- st_drop_geometry(data_a)
    combined_data <- merge(combined_data, 
                           data_a_df[, c("municipality_id", "municipality_name", "frequency", "name_breakdown")],
                           by = "municipality_id", all.x = TRUE, suffixes = c("", "_a"))
    names(combined_data)[names(combined_data) == "frequency"] <- "freq_a"
    names(combined_data)[names(combined_data) == "name_breakdown"] <- "breakdown_a"
    names(combined_data)[names(combined_data) == "municipality_name"] <- "muni_name"
  } else {
    combined_data$freq_a <- 0
    combined_data$breakdown_a <- ""
  }
  
  # Add Group B data
  if (!is.null(data_b) && nrow(data_b) > 0) {
    data_b_df <- st_drop_geometry(data_b)
    combined_data <- merge(combined_data,
                           data_b_df[, c("municipality_id", "municipality_name", "frequency", "name_breakdown")],
                           by = "municipality_id", all.x = TRUE, suffixes = c("", "_b"))
    names(combined_data)[names(combined_data) == "frequency"] <- "freq_b"
    names(combined_data)[names(combined_data) == "name_breakdown"] <- "breakdown_b"
    
    if (!"muni_name" %in% names(combined_data)) {
      names(combined_data)[names(combined_data) == "municipality_name"] <- "muni_name"
    } else {
      combined_data$muni_name <- ifelse(
        is.na(combined_data$muni_name),
        combined_data$municipality_name,
        combined_data$muni_name
      )
      combined_data$municipality_name <- NULL
    }
  } else {
    combined_data$freq_b <- 0
    combined_data$breakdown_b <- ""
  }
  
  # Add geometries
  if (!is.null(data_a) && nrow(data_a) > 0) {
    geom_data <- data_a[match(combined_data$municipality_id, data_a$municipality_id), ]
    combined_data$geometry <- st_geometry(geom_data)
    
    if (!is.null(data_b) && nrow(data_b) > 0) {
      missing_geom <- is.na(st_dimension(combined_data$geometry))
      if (any(missing_geom)) {
        geom_data_b <- data_b[match(combined_data$municipality_id[missing_geom], data_b$municipality_id), ]
        combined_data$geometry[missing_geom] <- st_geometry(geom_data_b)
      }
    }
  } else if (!is.null(data_b) && nrow(data_b) > 0) {
    geom_data <- data_b[match(combined_data$municipality_id, data_b$municipality_id), ]
    combined_data$geometry <- st_geometry(geom_data)
  }
  
  # Fill NAs
  combined_data$freq_a[is.na(combined_data$freq_a)] <- 0
  combined_data$freq_b[is.na(combined_data$freq_b)] <- 0
  
  # Calculate total frequency and normalize
  combined_data$total_freq <- combined_data$freq_a + combined_data$freq_b
  max_total <- max(combined_data$total_freq, na.rm = TRUE)
  
  # Determine color and intensity
  combined_data$color <- sapply(1:nrow(combined_data), function(i) {
    freq_a <- combined_data$freq_a[i]
    freq_b <- combined_data$freq_b[i]
    total <- combined_data$total_freq[i]
    
    intensity <- 0.4 + (0.6 * (total / max_total))
    
    if (freq_a > 0 && freq_b > 0) {
      blend_colors(freq_a, freq_b, intensity)
    } else if (freq_a > 0) {
      base_red <- col2rgb("#e74c3c")
      blended <- base_red * intensity
      rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
    } else {
      base_blue <- col2rgb("#3498db")
      blended <- base_blue * intensity
      rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
    }
  })
  
  # Create labels
  combined_data$label_text <- sapply(1:nrow(combined_data), function(i) {
    create_label_text(
      combined_data$muni_name[i],
      combined_data$freq_a[i],
      combined_data$freq_b[i],
      combined_data$breakdown_a[i],
      combined_data$breakdown_b[i],
      combined_data$total_freq[i]
    )
  })
  
  # Convert to sf object
  st_sf(combined_data, crs = 4326)
}

# ==============================================================================
# CHART CREATION FUNCTIONS
# ==============================================================================

# Create comparison chart
create_comparison_chart <- function(stats_a, stats_b, dark_mode) {
  plot_data <- data.frame()
  
  if (!is.null(stats_a)) {
    plot_data <- rbind(plot_data, data.frame(
      name = stats_a$name,
      count = stats_a$total_count,
      group = "Group A",
      stringsAsFactors = FALSE
    ))
  }
  
  if (!is.null(stats_b)) {
    plot_data <- rbind(plot_data, data.frame(
      name = stats_b$name,
      count = stats_b$total_count,
      group = "Group B",
      stringsAsFactors = FALSE
    ))
  }
  
  if (nrow(plot_data) == 0) return(NULL)
  
  # Create color map
  unique_names_a <- if (!is.null(stats_a)) stats_a$name else character(0)
  unique_names_b <- if (!is.null(stats_b)) stats_b$name else character(0)
  
  color_map <- c()
  if (length(unique_names_a) > 0) {
    color_map <- c(color_map, setNames(GROUP_A_COLORS[1:length(unique_names_a)], unique_names_a))
  }
  if (length(unique_names_b) > 0) {
    color_map <- c(color_map, setNames(GROUP_B_COLORS[1:length(unique_names_b)], unique_names_b))
  }
  
  text_color <- if(dark_mode) "#e0e0e0" else "#333333"
  grid_color <- if(dark_mode) "#444444" else "#e0e0e0"
  
  plot_ly(plot_data, x = ~group, y = ~count, color = ~name, 
          colors = color_map,
          type = "bar",
          hovertemplate = paste(
            "<b>%{fullData.name}</b><br>",
            "Count: %{y:,}<br>",
            "<extra></extra>"
          )) %>%
    layout(
      barmode = "stack",
      xaxis = list(
        title = "",
        tickfont = list(color = text_color, size = 12),
        showgrid = FALSE
      ),
      yaxis = list(
        title = "",
        tickfont = list(color = text_color, size = 11),
        gridcolor = grid_color,
        showgrid = TRUE
      ),
      showlegend = FALSE,
      margin = list(l = 50, r = 20, t = 20, b = 40),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      font = list(color = text_color)
    ) %>%
    config(displayModeBar = FALSE)
}

# Create regional chart
create_regional_chart <- function(stats_a, stats_b, dark_mode) {
  regional_data <- data.frame()
  
  if (!is.null(stats_a)) {
    for (i in 1:nrow(stats_a)) {
      if (stats_a$top_regions[i] != "") {
        regions <- strsplit(stats_a$top_regions[i], "; ")[[1]]
        for (r in regions) {
          parts <- strsplit(r, ": ")[[1]]
          if (length(parts) == 2) {
            regional_data <- rbind(regional_data, data.frame(
              region = parts[1],
              count = as.numeric(parts[2]),
              group = "Group A",
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  if (!is.null(stats_b)) {
    for (i in 1:nrow(stats_b)) {
      if (stats_b$top_regions[i] != "") {
        regions <- strsplit(stats_b$top_regions[i], "; ")[[1]]
        for (r in regions) {
          parts <- strsplit(r, ": ")[[1]]
          if (length(parts) == 2) {
            regional_data <- rbind(regional_data, data.frame(
              region = parts[1],
              count = as.numeric(parts[2]),
              group = "Group B",
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  if (nrow(regional_data) == 0) return(NULL)
  
  # Aggregate by region and group
  regional_agg <- aggregate(count ~ region + group, data = regional_data, FUN = sum)
  
  # Format region names
  format_region_name <- function(name) {
    words <- strsplit(tolower(name), " ")[[1]]
    formatted <- sapply(words, function(w) {
      paste0(toupper(substring(w, 1, 1)), substring(w, 2))
    })
    paste(formatted, collapse = "<br>")
  }
  
  regional_agg$region_formatted <- sapply(regional_agg$region, format_region_name)
  regional_agg$region_original <- regional_agg$region
  
  text_color <- if(dark_mode) "#e0e0e0" else "#333333"
  grid_color <- if(dark_mode) "#444444" else "#e0e0e0"
  
  plot_ly(regional_agg, x = ~region_formatted, y = ~count, color = ~group,
          colors = c("Group A" = "#e74c3c", "Group B" = "#3498db"),
          type = "bar",
          hovertemplate = paste(
            "<b>%{customdata}</b><br>",
            "%{fullData.name}: %{y:,}<br>",
            "<extra></extra>"
          ),
          customdata = ~region_original) %>%
    layout(
      barmode = "group",
      xaxis = list(
        title = "",
        tickfont = list(color = text_color, size = 10),
        tickangle = 0,
        showgrid = FALSE
      ),
      yaxis = list(
        title = "",
        tickfont = list(color = text_color, size = 11),
        gridcolor = grid_color,
        showgrid = TRUE
      ),
      showlegend = FALSE,
      margin = list(l = 50, r = 20, t = 20, b = 100),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      font = list(color = text_color)
    ) %>%
    config(displayModeBar = FALSE)
}