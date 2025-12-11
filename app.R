# ==============================================================================
# BELGIUM NAME HEATMAP - MAIN APP
# ==============================================================================

library(shiny)
library(leaflet)
library(shinyjs)

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
    tags$div(class = "panel-title", "Name Search"),
    
    tags$div(
      class = "filter-group",
      selectInput("gender", "Gender", 
                  choices = c("All" = "", "Male" = "M", "Female" = "F"),
                  selected = "")
    ),
    
    tags$div(
      class = "filter-group",
      selectizeInput("names", "Select Names", 
                     choices = NULL, 
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Type to search...',
                       maxItems = 3
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
    tags$div(class = "panel-title", "Statistics"),
    
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
  
  updateSelectizeInput(session, "names", 
                       choices = character(0), 
                       server = TRUE)
  
  observe({
    gender_filter <- input$gender
    
    # Server-side selectize for efficient searching
    updateSelectizeInput(
      session, "names",
      choices = get_all_names(conn, gender_filter),
      server = TRUE
    )
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
  
  # Update map when names are selected
  observeEvent(input$names, {
    if (length(input$names) == 0) {
      # Clear map
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
    } else {
      # Get name frequency data
      name_data <- get_name_heatmap_data(conn, input$names)
      
      if (!is.null(name_data) && nrow(name_data) > 0) {
        # Create color palette based on frequency
        max_freq <- max(name_data$frequency, na.rm = TRUE)
        pal <- colorNumeric(
          palette = "Blues",
          domain = c(0, max_freq),
          na.color = "#e0e0e0"
        )
        
        leafletProxy("map") %>%
          clearShapes() %>%
          clearControls() %>%
          addPolygons(
            data = name_data,
            fillColor = ~pal(frequency),
            fillOpacity = 0.7,
            color = "white",
            weight = 1,
            label = ~paste0(municipality_name, ": ", frequency, " (", 
                            sprintf("%.2f%%", percentage), ")")
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = name_data$frequency,
            title = if(length(input$names) == 1) input$names[1] else "Combined",
            opacity = 0.7
          )
      }
    }
  })
  
  # ===========================================================================
  # RESET
  # ===========================================================================
  
  observeEvent(input$reset, {
    updateSelectizeInput(session, "names", selected = character(0))
    updateSelectInput(session, "gender", selected = "")
  })
  
  # ===========================================================================
  # METRICS PANEL
  # ===========================================================================
  
  output$show_metrics <- reactive({
    length(input$names) > 0
  })
  outputOptions(output, "show_metrics", suspendWhenHidden = FALSE)
  
  output$metrics_display <- renderUI({
    req(input$names)
    
    stats <- get_name_statistics(conn, input$names)
    
    if (is.null(stats) || nrow(stats) == 0) {
      return(tags$p("No data available"))
    }
    
    tagList(
      lapply(1:nrow(stats), function(i) {
        tags$div(
          class = "name-stats-card",
          tags$h4(stats$name[i], class = paste0("gender-", tolower(stats$gender[i]))),
          tags$div(class = "stat-grid",
                   tags$div(class = "stat-box",
                            tags$div(class = "stat-value", format(stats$total_count[i], big.mark = ",")),
                            tags$div(class = "stat-label", "Total Count")
                   ),
                   tags$div(class = "stat-box",
                            tags$div(class = "stat-value", stats$municipality_count[i]),
                            tags$div(class = "stat-label", "Municipalities")
                   ),
                   tags$div(class = "stat-box",
                            tags$div(class = "stat-value", sprintf("%.3f%%", stats$national_percentage[i])),
                            tags$div(class = "stat-label", "National %")
                   )
          ),
          tags$div(class = "region-breakdown",
                   tags$strong("Regional Breakdown:"),
                   tags$div(style = "margin-top: 8px;",
                            lapply(strsplit(stats$top_regions[i], "; ")[[1]], function(r) {
                              tags$div(class = "region-item", r)
                            })
                   )
          ),
          tags$hr()
        )
      })
    )
  })
}

shinyApp(ui, server)