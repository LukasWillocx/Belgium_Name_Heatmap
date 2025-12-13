# ==============================================================================
# BELGIUM NAME HEATMAP - MAIN APP WITH A/B COMPARISON
# ==============================================================================

library(shiny)
library(leaflet)
library(shinyjs)
library(plotly)

# Source external files
source("database_functions.R")

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=8")
  ),
  
  tags$button(class = "dark-mode-toggle", id = "darkModeToggle", "🌙"),
  
  tags$div(class = "sidebar-toggle hidden", id = "sidebarToggle", "☰"),
  tags$div(class = "metrics-toggle hidden", id = "metricsToggle", "📊"),
  
  # Left Panel
  tags$div(
    class = "left-panel",
    id = "leftPanel",
    tags$button(class = "close-sidebar", id = "closeLeftPanel", "×"),
    tags$div(class = "panel-title", "Name Comparison"),
    
    tags$div(
      class = "filter-group",
      tags$label("Group A", style = "color: #e74c3c; font-weight: bold;"),
      selectizeInput("names_a", NULL, 
                     choices = NULL, 
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Type to search...',
                       maxItems = 5
                     ))
    ),
    
    tags$div(
      class = "filter-group",
      tags$label("Group B", style = "color: #3498db; font-weight: bold;"),
      selectizeInput("names_b", NULL, 
                     choices = NULL, 
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Type to search...',
                       maxItems = 5
                     ))
    ),
    
    tags$div(
      class = "filter-group",
      actionButton("reset", "Clear Selection", class = "btn-secondary btn-block")
    )
  ),
  
  # Right Panel
  tags$div(
    class = "right-panel",
    id = "rightPanel",
    tags$button(class = "close-sidebar", id = "closeRightPanel", "×"),
    tags$div(class = "panel-title", "Comparison Statistics"),
    
    conditionalPanel(
      condition = "output.show_metrics",
      uiOutput("metrics_display")
    )
  ),
  
  # Map
  leafletOutput("map", width = "100%", height = "100vh"),
  
  # JavaScript
  tags$script(HTML("
    $(document).ready(function() {
      let darkMode = false;
      $('#darkModeToggle').click(function() {
        darkMode = !darkMode;
        $('body').toggleClass('dark-mode');
        Shiny.setInputValue('darkModeState', darkMode);
      });
      
      $('#closeLeftPanel').click(function() {
        $('#leftPanel').addClass('closed');
        $('#sidebarToggle').removeClass('hidden');
      });
      
      $('#sidebarToggle').click(function() {
        $('#leftPanel').removeClass('closed');
        $('#sidebarToggle').addClass('hidden');
      });
      
      $('#closeRightPanel').click(function() {
        $('#rightPanel').addClass('closed');
        $('#metricsToggle').removeClass('hidden');
      });
      
      $('#metricsToggle').click(function() {
        $('#rightPanel').removeClass('closed');
        $('#metricsToggle').addClass('hidden');
      });
    });
  "))
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  conn <- get_db_connection()
  onStop(function() { dbDisconnect(conn) })
  
  darkMode <- reactiveVal(FALSE)
  
  observeEvent(input$darkModeState, {
    darkMode(input$darkModeState)
  })
  
  # ===========================================================================
  # NAME AUTOCOMPLETE
  # ===========================================================================
  
  # Get all names once for both inputs
  all_names <- reactive({
    get_all_names(conn, NULL)
  })
  
  observe({
    names_list <- all_names()
    updateSelectizeInput(session, "names_a", choices = names_list, server = TRUE)
    updateSelectizeInput(session, "names_b", choices = names_list, server = TRUE)
  })
  
  # ===========================================================================
  # MAP RENDERING
  # ===========================================================================
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 4.5, lat = 50.5, zoom = 8)
  })
  
  # Dark mode tiles
  observeEvent(darkMode(), {
    if (darkMode()) {
      leafletProxy("map") %>%
        clearTiles() %>%
        addProviderTiles(providers$CartoDB.DarkMatter)
    } else {
      leafletProxy("map") %>%
        clearTiles() %>%
        addProviderTiles(providers$CartoDB.Positron)
    }
  })
  
  # Helper function to blend colors based on frequency ratio
  blend_colors <- function(freq_a, freq_b, intensity) {
    # Base colors
    red <- col2rgb("#e74c3c")
    blue <- col2rgb("#3498db")
    
    # Calculate ratio (0 = all A/red, 1 = all B/blue)
    total <- freq_a + freq_b
    ratio <- freq_b / total
    
    # Blend RGB values
    blended <- red * (1 - ratio) + blue * ratio
    
    # Apply intensity based on total frequency (darker = more frequent)
    # Intensity ranges from 0.3 (light) to 1.0 (dark)
    blended <- blended * intensity
    
    rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
  }
  
  # Debounced reactive values for names to prevent race conditions
  names_a_debounced <- debounce(reactive(input$names_a), 500)
  names_b_debounced <- debounce(reactive(input$names_b), 500)
  
  # Update map when names are selected - with debouncing
  observe({
    names_a <- names_a_debounced()
    names_b <- names_b_debounced()
    
    # Don't require both inputs - allow either one
    if (length(names_a) == 0 && length(names_b) == 0) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      return()
    }
    
    # Get data for both groups with error handling
    data_a <- NULL
    data_b <- NULL
    
    tryCatch({
      if (length(names_a) > 0) {
        data_a <- get_name_heatmap_data_detailed(conn, names_a, "a")
      }
    }, error = function(e) {
      showNotification(paste("Error loading Group A data:", e$message), type = "error")
    })
    
    tryCatch({
      if (length(names_b) > 0) {
        data_b <- get_name_heatmap_data_detailed(conn, names_b, "b")
      }
    }, error = function(e) {
      showNotification(paste("Error loading Group B data:", e$message), type = "error")
    })
    
    # Clear existing layers
    map_proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearControls()
    
    # Get all unique municipalities
    all_munis <- c()
    if (!is.null(data_a) && nrow(data_a) > 0) all_munis <- c(all_munis, data_a$municipality_id)
    if (!is.null(data_b) && nrow(data_b) > 0) all_munis <- c(all_munis, data_b$municipality_id)
    all_munis <- unique(all_munis)
    
    if (length(all_munis) == 0) return()
    
    # Create combined dataset with ALL municipalities
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
      
      # Update municipality name if we didn't have it from Group A
      if (!"muni_name" %in% names(combined_data)) {
        names(combined_data)[names(combined_data) == "municipality_name"] <- "muni_name"
      } else {
        # Fill in missing names from Group B
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
    
    # Add geometries - prioritize Group A, fall back to Group B
    if (!is.null(data_a) && nrow(data_a) > 0) {
      geom_data <- data_a[match(combined_data$municipality_id, data_a$municipality_id), ]
      combined_data$geometry <- st_geometry(geom_data)
      
      # For municipalities only in Group B, get their geometry
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
    
    # Fill NAs with 0
    combined_data$freq_a[is.na(combined_data$freq_a)] <- 0
    combined_data$freq_b[is.na(combined_data$freq_b)] <- 0
    
    # Calculate total frequency and normalize for intensity
    combined_data$total_freq <- combined_data$freq_a + combined_data$freq_b
    max_total <- max(combined_data$total_freq, na.rm = TRUE)
    
    # Determine color and intensity for each municipality
    combined_data$color <- sapply(1:nrow(combined_data), function(i) {
      freq_a <- combined_data$freq_a[i]
      freq_b <- combined_data$freq_b[i]
      total <- combined_data$total_freq[i]
      
      # Intensity from 0.4 to 1.0 based on frequency
      intensity <- 0.4 + (0.6 * (total / max_total))
      
      if (freq_a > 0 && freq_b > 0) {
        # Both groups present - blend colors
        blend_colors(freq_a, freq_b, intensity)
      } else if (freq_a > 0) {
        # Only Group A - red scale
        base_red <- col2rgb("#e74c3c")
        blended <- base_red * intensity
        rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
      } else {
        # Only Group B - blue scale
        base_blue <- col2rgb("#3498db")
        blended <- base_blue * intensity
        rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
      }
    })
    
    # Create labels
    combined_data$label_text <- sapply(1:nrow(combined_data), function(i) {
      parts <- c(paste0("<strong style='font-size: 14px;'>", combined_data$muni_name[i], "</strong>"))
      
      if (combined_data$freq_a[i] > 0) {
        parts <- c(parts, paste0("<span style='color: #e74c3c; font-weight: 600;'>Group A (", 
                                 combined_data$freq_a[i], " total):</span>"))
        
        # Parse name breakdown
        if (!is.na(combined_data$breakdown_a[i]) && combined_data$breakdown_a[i] != "") {
          breakdown <- strsplit(combined_data$breakdown_a[i], "; ")[[1]]
          for (item in breakdown) {
            parts <- c(parts, paste0("<span style='color: #e74c3c; font-size: 12px; padding-left: 8px;'>• ", item, "</span>"))
          }
        }
      }
      
      if (combined_data$freq_b[i] > 0) {
        # Add spacing if both groups present
        if (combined_data$freq_a[i] > 0) {
          parts <- c(parts, "<span style='font-size: 4px;'> </span>")
        }
        
        parts <- c(parts, paste0("<span style='color: #3498db; font-weight: 600;'>Group B (", 
                                 combined_data$freq_b[i], " total):</span>"))
        
        # Parse name breakdown
        if (!is.na(combined_data$breakdown_b[i]) && combined_data$breakdown_b[i] != "") {
          breakdown <- strsplit(combined_data$breakdown_b[i], "; ")[[1]]
          for (item in breakdown) {
            parts <- c(parts, paste0("<span style='color: #3498db; font-size: 12px; padding-left: 8px;'>• ", item, "</span>"))
          }
        }
      }
      
      if (combined_data$freq_a[i] > 0 && combined_data$freq_b[i] > 0) {
        total <- combined_data$total_freq[i]
        pct_a <- round(100 * combined_data$freq_a[i] / total, 1)
        pct_b <- round(100 * combined_data$freq_b[i] / total, 1)
        parts <- c(parts, "<span style='font-size: 4px;'> </span>")
        parts <- c(parts, paste0("<span style='color: #666; font-size: 11px; font-style: italic;'>",
                                 "Overall Ratio: ", pct_a, "% / ", pct_b, "%</span>"))
      }
      
      paste(parts, collapse = "<br/>")
    })
    
    # Convert to sf object
    combined_sf <- st_sf(combined_data, crs = 4326)
    
    # Add to map
    map_proxy %>%
      addPolygons(
        data = combined_sf,
        fillColor = ~color,
        fillOpacity = 0.75,
        color = ~color,
        weight = 1.5,
        label = ~lapply(label_text, htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#fff",
          fillOpacity = 0.85,
          bringToFront = TRUE
        )
      )
  })
  
  # ===========================================================================
  # RESET
  # ===========================================================================
  
  observeEvent(input$reset, {
    updateSelectizeInput(session, "names_a", selected = character(0))
    updateSelectizeInput(session, "names_b", selected = character(0))
  })
  
  # ===========================================================================
  # METRICS PANEL
  # ===========================================================================
  
  output$show_metrics <- reactive({
    length(input$names_a) > 0 || length(input$names_b) > 0
  })
  outputOptions(output, "show_metrics", suspendWhenHidden = FALSE)
  
  output$metrics_display <- renderUI({
    if (length(input$names_a) == 0 && length(input$names_b) == 0) {
      return(tags$p("Select names to compare"))
    }
    
    # Get statistics for both groups
    stats_a <- if (length(input$names_a) > 0) {
      get_name_statistics(conn, input$names_a)
    } else NULL
    
    stats_b <- if (length(input$names_b) > 0) {
      get_name_statistics(conn, input$names_b)
    } else NULL
    
    tagList(
      # Summary comparison - side by side boxes
      tags$div(class = "comparison-grid",
               if (!is.null(stats_a)) {
                 tags$div(class = "group-box group-a-box",
                          tags$div(class = "group-box-header", "Group A"),
                          tags$div(class = "group-box-count", format(sum(stats_a$total_count), big.mark = ",")),
                          tags$div(class = "group-box-names", 
                                   paste(input$names_a, collapse = ", ")
                          )
                 )
               } else {
                 tags$div(class = "group-box group-a-box empty",
                          tags$div(class = "group-box-header", "Group A"),
                          tags$div(class = "group-box-empty", "No names selected")
                 )
               },
               if (!is.null(stats_b)) {
                 tags$div(class = "group-box group-b-box",
                          tags$div(class = "group-box-header", "Group B"),
                          tags$div(class = "group-box-count", format(sum(stats_b$total_count), big.mark = ",")),
                          tags$div(class = "group-box-names", 
                                   paste(input$names_b, collapse = ", ")
                          )
                 )
               } else {
                 tags$div(class = "group-box group-b-box empty",
                          tags$div(class = "group-box-header", "Group B"),
                          tags$div(class = "group-box-empty", "No names selected")
                 )
               }
      ),
      
      tags$hr(style = "margin: 20px 0;"),
      
      # Interactive frequency chart
      if (!is.null(stats_a) || !is.null(stats_b)) {
        tags$div(
          tags$h4("Frequency Comparison by Name", style = "margin-bottom: 15px;"),
          plotlyOutput("comparison_chart", height = "300px")
        )
      },
      
      tags$hr(style = "margin: 20px 0;"),
      
      # Interactive regional chart
      if (!is.null(stats_a) || !is.null(stats_b)) {
        tags$div(
          tags$h4("Regional Distribution", style = "margin-bottom: 15px;"),
          plotlyOutput("regional_chart", height = "250px")
        )
      }
    )
  })
  
  # Interactive comparison chart with Plotly - USING CUSTOM PALETTES
  output$comparison_chart <- renderPlotly({
    stats_a <- if (length(input$names_a) > 0) {
      get_name_statistics(conn, input$names_a)
    } else NULL
    
    stats_b <- if (length(input$names_b) > 0) {
      get_name_statistics(conn, input$names_b)
    } else NULL
    
    # Prepare data
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
    
    # Create color map using custom palettes
    unique_names_a <- if (!is.null(stats_a)) stats_a$name else character(0)
    unique_names_b <- if (!is.null(stats_b)) stats_b$name else character(0)
    
    color_map <- c()
    if (length(unique_names_a) > 0) {
      color_map <- c(color_map, setNames(GROUP_A_COLORS[1:length(unique_names_a)], unique_names_a))
    }
    if (length(unique_names_b) > 0) {
      color_map <- c(color_map, setNames(GROUP_B_COLORS[1:length(unique_names_b)], unique_names_b))
    }
    
    # Determine text color based on dark mode
    text_color <- if(darkMode()) "#e0e0e0" else "#333333"
    grid_color <- if(darkMode()) "#444444" else "#e0e0e0"
    
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
  })
  
  # Interactive regional chart with Plotly
  output$regional_chart <- renderPlotly({
    stats_a <- if (length(input$names_a) > 0) {
      get_name_statistics(conn, input$names_a)
    } else NULL
    
    stats_b <- if (length(input$names_b) > 0) {
      get_name_statistics(conn, input$names_b)
    } else NULL
    
    # Parse regional data
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
    
    # Format region names: capitalize first letter of each word and add line breaks
    format_region_name <- function(name) {
      # Convert to title case (first letter of each word capitalized)
      words <- strsplit(tolower(name), " ")[[1]]
      formatted <- sapply(words, function(w) {
        paste0(toupper(substring(w, 1, 1)), substring(w, 2))
      })
      # Join with <br> for line breaks
      paste(formatted, collapse = "<br>")
    }
    
    regional_agg$region_formatted <- sapply(regional_agg$region, format_region_name)
    regional_agg$region_original <- regional_agg$region  # Keep original for hover
    
    # Determine text color based on dark mode
    text_color <- if(darkMode()) "#e0e0e0" else "#333333"
    grid_color <- if(darkMode()) "#444444" else "#e0e0e0"
    
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
  })
}

shinyApp(ui, server)