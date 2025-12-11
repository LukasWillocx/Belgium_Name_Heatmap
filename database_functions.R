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
       -- Calculate percentage of municipality population
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
  
  # Get total Belgian population for percentage calculation
  belgium_pop_query <- "SELECT SUM(value * area_km2) as total_pop
                        FROM metric_values mv
                        JOIN municipalities m ON mv.municipality_id = m.id
                        JOIN metric_definitions md ON mv.metric_id = md.id
                        WHERE md.metric_key = 'population_density'"
  belgium_pop <- dbGetQuery(conn, belgium_pop_query)$total_pop[1]
  
  query <- sprintf(
    "SELECT 
       nf.name,
       nf.gender,
       SUM(nf.frequency) as total_count,
       COUNT(DISTINCT nf.municipality_id) as municipality_count,
       (SUM(nf.frequency)::numeric / %f) * 100 as national_percentage,
       STRING_AGG(
         DISTINCT r.name || ': ' || regional_counts.count::text, 
         '; ' 
         ORDER BY r.name
       ) as top_regions
     FROM name_frequencies nf
     JOIN municipalities m ON nf.municipality_id = m.id
     JOIN provinces p ON m.province_id = p.id
     JOIN regions r ON p.region_id = r.id
     LEFT JOIN (
       SELECT 
         nf2.name,
         r2.name as region_name,
         SUM(nf2.frequency) as count
       FROM name_frequencies nf2
       JOIN municipalities m2 ON nf2.municipality_id = m2.id
       JOIN provinces p2 ON m2.province_id = p2.id
       JOIN regions r2 ON p2.region_id = r2.id
       WHERE nf2.year = 2025 AND nf2.name IN (%s)
       GROUP BY nf2.name, r2.name
     ) regional_counts ON regional_counts.name = nf.name AND regional_counts.region_name = r.name
     WHERE nf.year = 2025 AND nf.name IN (%s)
     GROUP BY nf.name, nf.gender
     ORDER BY nf.name",
    belgium_pop,
    names_sql,
    names_sql
  )
  
  dbGetQuery(conn, query)
}