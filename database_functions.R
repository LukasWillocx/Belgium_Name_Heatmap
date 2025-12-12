# ==============================================================================
# NAME HEATMAP - DATABASE FUNCTIONS
# ==============================================================================

library(RPostgres)
library(sf)
library(DBI)

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

# Get heatmap data for selected name(s)
get_name_heatmap_data <- function(conn, names) {
  if (length(names) == 0) return(NULL)
  
  # Escape names for SQL
  names_sql <- paste0("'", gsub("'", "''", names), "'", collapse = ", ")
  
  query <- sprintf(
    "SELECT 
       m.id as municipality_id,
       m.name as municipality_name,
       COALESCE(ST_AsText(m.geometry_simplified), ST_AsText(m.geometry)) as wkt,
       SUM(nf.frequency) as frequency,
       CASE 
         WHEN mv.value > 0 AND m.area_km2 > 0 
         THEN (SUM(nf.frequency)::numeric / (mv.value * m.area_km2)) * 100
         ELSE 0
       END as percentage
     FROM municipalities m
     LEFT JOIN name_frequencies nf ON nf.municipality_id = m.id
     LEFT JOIN metric_values mv ON mv.municipality_id = m.id
     LEFT JOIN metric_definitions md ON mv.metric_id = md.id
     WHERE nf.year = 2025 
       AND nf.name IN (%s)
       AND (md.metric_key = 'population_density' OR md.metric_key IS NULL)
     GROUP BY m.id, m.name, m.geometry, m.geometry_simplified, m.area_km2, mv.value
     HAVING SUM(nf.frequency) > 0
     ORDER BY m.name",
    names_sql
  )
  
  result <- dbGetQuery(conn, query)
  if (nrow(result) == 0) return(NULL)
  
  # Handle very small percentages that cause color scale issues
  result$percentage[result$percentage < 0.001] <- 0.001
  
  geom <- st_as_sfc(result$wkt, crs = 4326)
  st_sf(
    municipality_id = result$municipality_id,
    municipality_name = result$municipality_name,
    frequency = result$frequency,
    percentage = result$percentage,
    geometry = geom
  )
}

# Get statistics for selected name(s)
get_name_statistics <- function(conn, names) {
  if (length(names) == 0) return(NULL)
  
  names_sql <- paste0("'", gsub("'", "''", names), "'", collapse = ", ")
  
  # Single simplified query
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
  
  # Get regional breakdown with a separate simple query
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
  
  # Build regional strings in R (simpler than SQL)
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