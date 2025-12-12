# ==============================================================================
# BELGIUM NAME HEATMAP - MAIN APP WITH A/B COMPARISON
# ==============================================================================

library(shiny)
library(leaflet)
library(shinyjs)
library(ggplot2)

# Source external files
source("database_functions.R")

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=7")
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
  
  # Combined names for map display
  all_selected_names <- reactive({
    c(input$names_a, input$names_b)
  })
  
  # Update map when names are selected
  observe({
    selected <- all_selected_names()
    
    if (length(selected) == 0) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
    } else {
      # Get data for Group A
      data_a <- if (length(input$names_a) > 0) {
        get_name_heatmap_data(conn, input$names_a)
      } else NULL
      
      # Get data for Group B
      data_b <- if (length(input$names_b) > 0) {
        get_name_heatmap_data(conn, input$names_b)
      } else NULL
      
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      
      # Add Group A polygons (red scale)
      if (!is.null(data_a) && nrow(data_a) > 0) {
        min_freq_a <- min(data_a$frequency, na.rm = TRUE)
        max_freq_a <- max(data_a$frequency, na.rm = TRUE)
        
        pal_a <- colorNumeric(
          palette = "Reds",
          domain = c(min_freq_a, max_freq_a),
          na.color = "#cccccc"
        )
        
        leafletProxy("map") %>%
          addPolygons(
            data = data_a,
            fillColor = ~pal_a(frequency),
            fillOpacity = 0.6,
            color = "#e74c3c",
            weight = 2,
            group = "Group A",
            label = ~paste0("Group A - ", municipality_name, ": ", frequency, " occurrences")
          )
      }
      
      # Add Group B polygons (blue scale)
      if (!is.null(data_b) && nrow(data_b) > 0) {
        min_freq_b <- min(data_b$frequency, na.rm = TRUE)
        max_freq_b <- max(data_b$frequency, na.rm = TRUE)
        
        pal_b <- colorNumeric(
          palette = "Blues",
          domain = c(min_freq_b, max_freq_b),
          na.color = "#cccccc"
        )
        
        leafletProxy("map") %>%
          addPolygons(
            data = data_b,
            fillColor = ~pal_b(frequency),
            fillOpacity = 0.6,
            color = "#3498db",
            weight = 2,
            group = "Group B",
            label = ~paste0("Group B - ", municipality_name, ": ", frequency, " occurrences")
          )
      }
    }
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
      # Summary comparison
      tags$div(class = "comparison-summary",
               if (!is.null(stats_a)) {
                 tags$div(class = "group-summary group-a",
                          tags$h4("Group A", style = "color: #e74c3c;"),
                          tags$div(class = "stat-value", format(sum(stats_a$total_count), big.mark = ",")),
                          tags$div(class = "stat-label", "Total Count"),
                          tags$div(style = "margin-top: 8px; font-size: 0.9em;",
                                   paste(input$names_a, collapse = ", ")
                          )
                 )
               },
               if (!is.null(stats_b)) {
                 tags$div(class = "group-summary group-b",
                          tags$h4("Group B", style = "color: #3498db;"),
                          tags$div(class = "stat-value", format(sum(stats_b$total_count), big.mark = ",")),
                          tags$div(class = "stat-label", "Total Count"),
                          tags$div(style = "margin-top: 8px; font-size: 0.9em;",
                                   paste(input$names_b, collapse = ", ")
                          )
                 )
               }
      ),
      
      tags$hr(),
      
      # Stacked bar chart
      if (!is.null(stats_a) || !is.null(stats_b)) {
        tags$div(
          tags$h4("Frequency Comparison by Name"),
          plotOutput("comparison_chart", height = "300px")
        )
      },
      
      tags$hr(),
      
      # Regional breakdown comparison
      if (!is.null(stats_a) || !is.null(stats_b)) {
        tags$div(
          tags$h4("Regional Distribution"),
          plotOutput("regional_chart", height = "250px")
        )
      }
    )
  })
  
  # Comparison chart
  output$comparison_chart <- renderPlot({
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
        group = "Group A"
      ))
    }
    
    if (!is.null(stats_b)) {
      plot_data <- rbind(plot_data, data.frame(
        name = stats_b$name,
        count = stats_b$total_count,
        group = "Group B"
      ))
    }
    
    if (nrow(plot_data) == 0) return(NULL)
    
    ggplot(plot_data, aes(x = group, y = count, fill = name)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_brewer(palette = "Set3") +
      labs(x = NULL, y = "Total Count", fill = "Name") +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        axis.text.x = element_text(size = 12, face = "bold")
      )
  })
  
  # Regional comparison chart
  output$regional_chart <- renderPlot({
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
                group = "Group A"
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
                group = "Group B"
              ))
            }
          }
        }
      }
    }
    
    if (nrow(regional_data) == 0) return(NULL)
    
    # Aggregate by region and group
    regional_agg <- aggregate(count ~ region + group, data = regional_data, FUN = sum)
    
    ggplot(regional_agg, aes(x = region, y = count, fill = group)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Group A" = "#e74c3c", "Group B" = "#3498db")) +
      labs(x = NULL, y = "Total Count", fill = NULL) +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}

shinyApp(ui, server)