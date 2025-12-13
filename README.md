# Belgium Name Heatmap - A/B Comparison Tool

An interactive R Shiny application for comparing name distributions across Belgian municipalities through visual heatmaps and statistical analysis for the year 2025.

## Features

* **Dual-group comparison** - Compare up to 5 names in Group A vs. 5 names in Group B
* **Color-blended heatmap** - Municipalities are colored based on the relative frequency of names from each group:
  - Pure red indicates Group A dominance
  - Pure blue indicates Group B dominance
  - Purple/blended colors show mixed presence
  - Color intensity reflects total frequency
* **Interactive tooltips** - Hover over municipalities to see detailed breakdowns:
  - Individual name frequencies within each group
  - Percentage ratios when both groups are present
* **Comprehensive statistics panel**:
  - Total counts for each group
  - Frequency comparison by name (stacked bar chart)
  - Regional distribution comparison (grouped bar chart)
* **Smart name search** - Type-ahead autocomplete for all names in the database
* **Dark mode toggle** - Automatic map tile and chart color switching
* **Responsive design** - Collapsible panels to maximize map viewing area
* **Debounced updates** - Prevents race conditions during rapid name selection

## How It Works

The application connects to a PostgreSQL database containing:
* Administrative boundaries (municipalities) with PostGIS geometries
* Name frequency data across the entire population by municipality for 2025

Users can select names for Group A (red) and Group B (blue). The map dynamically updates to show where each group's names are most common, with color blending indicating the relative prevalence. The right panel provides detailed statistical comparisons including total counts, per-name breakdowns, and regional distributions.

## Use Cases

* **Name popularity comparison** - Compare traditional vs. modern names
* **Cultural analysis** - Examine Flemish vs. French name patterns
* **Regional preferences** - Identify geographic clustering of name types

## Tech Stack

* **R Shiny** - Web application framework
* **PostgreSQL + PostGIS** - Spatial database
* **Leaflet** - Interactive mapping
* **Plotly** - Interactive statistical charts
* **Simple Features (sf)** - Spatial data handling

## Required R Packages

```r
install.packages(c(
  "shiny",
  "leaflet",
  "shinyjs",
  "plotly",
  "RPostgres",
  "sf",
  "DBI"
))
```

## File Structure

```
belgium-name-heatmap/
│
├── app.R                      # Main application (UI + Server logic)
├── database_functions.R       # Database queries and data processing
├── credentials.R              # Database credentials (not in repo)
│
└── www/
    └── styles.css            # Custom CSS for panels, dark mode, and comparison UI
```

## File Descriptions

* **app.R**: Contains the Shiny UI definition and server logic. Handles user interactions, map rendering, reactive updates, and chart generation based on name selections.
* **database_functions.R**: Defines all database queries for fetching name data and statistics. Includes data processing functions for merging groups, color blending, label generation, and chart creation.
* **credentials.R**: Stores database connection parameters (dbname, host, port, user, password). This file should be created locally and added to `.gitignore`.
* **www/styles.css**: Custom styling for the comparison panels, dark mode, metric displays, and interactive charts.

## Database Schema

### Key tables:
* `municipalities` - 565 municipalities with MultiPolygon geometries (simplified for performance)
* `name_frequencies` - Name frequency data by municipality, name, gender, and year
* `provinces` - Ten provinces with region relationships
* `regions` - Three Belgian regions

### Key queries:
* Name autocomplete with distinct name lookup
* Detailed heatmap data with name breakdowns for tooltips
* Statistical aggregations for comparison metrics
* Regional distribution summaries

## Color System

### Group A (Red Palette)
* Light to dark reds (#ffb3b3 → #8b0000)
* Used for names selected in Group A

### Group B (Blue Palette)
* Light to dark blues (#aed6f1 → #1a5490)
* Used for names selected in Group B

### Blending Logic
* Colors blend proportionally based on frequency ratios
* Intensity scales with total frequency (0.4 to 1.0)
* Creates intuitive visual hierarchy

## Performance Optimizations

* **Geometry simplification** - Faster map rendering using pre-simplified geometries
* **Debounced inputs** - 500ms delay prevents excessive database queries
* **Efficient merging** - Single-pass data combination for both groups
* **Spatial indexes** - PostGIS indexes on geometry columns
* **Selective queries** - Only fetches municipalities with matching names
* **Client-side rendering** - Charts generated in browser via Plotly

## Hardware Requirements

* **Very low** - Application is lightweight and single-threaded
* Currently runs on a modern low-power quad-core mini computer (Minisforum UN100P)
* No parallelization required


* Additional statistical measures (correlation, clustering)
* Mobile-optimized responsive design
* Multi-language support for UI
