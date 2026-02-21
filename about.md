## Functionality

This application performs a visual comparison of the popularity of name(s) between two user-definable groups. It is an adaptation of the application 'Belgium' that displays population density and top 10 most popular names at the municipal, provincial, regional and national level.
 
- **Group comparison**: each group consist of 1-5 names. This is an arbitrary upper limit to facilitate smooth performance. It provides the user with the flexibility to compare names that'd otherwise only be dissimilar in spelling and aggregate them as such. (e.g. Thomas & Tomas, Lukas & Lucas, etc.)
- **Comparison Statistics panel**: provides an aggregated overview between the two groups. It offers frequencies by name within a group. It also portrays regional distribution, which may particularly highlight cultural shifts as to which names might be common as it relates to location.
- **Name selection**: the selection box provides auto filled suggestions upon typing. Both male and female names are pooled together.
- **Choropleth map**: excellent visual overview as to how two groups of names might compare in frequency. It utilizes color blending (red for group A, blue for group B) to indicate varying ratios of the frequencies in either group across all Belgian municipalities in various shades of purple. Color intensity is determined by name frequency. The hover text provides a short summary at municipal level.

## Data Sources (2025)

### [Statbel](https://statbel.fgov.be/nl)

- [REFNIS codes](https://statbel.fgov.be/nl/over-statbel/methodologie/classificaties/geografie) in .CSV format for all 565 municipalities, 10 provinces and 3 regions
- [First name frequencies](https://statbel.fgov.be/nl/open-data/voornamen-van-de-totale-bevolking-gemeente-15) in .xlsx format for all municipalities, for men and women with a threshold of five. This means that if a name occurs less than five times in a municipality, it's not in the database. This constitutes the bulk of the raw data at a total of 423,647 rows.
- [Population density](https://statbel.fgov.be/sites/default/files/files/documents/bevolking/5.11%20Bevolkingsdichtheid/Pop_density_nl.xlsx) for all municipalities from 2019–2025. Only 2025 data was ingested to match municipalities based on their NIS code (with recent mergers).

### [Open Data City of Brussels](https://opendata.brussels.be/pages/home/)

- [Spatial data of municipalities](https://opendata.brussels.be/explore/dataset/codes-ins-nis-postaux-belgique/export/?disjunctive.postal_code&disjunctive.refnis_code&disjunctive.gemeentenaam&disjunctive.nom_commune&disjunctive.code_ins_region&disjunctive.region_fr&disjunctive.region_nl&disjunctive.region_en) in .GeoJSON format with the WGS84 coordinate system (EPSG:4326) for the administrative boundaries.

### Shortcomings

All data are linked based on their Belgian NIS codes for 2025. Originally there were 13 missing entries for geometry data for municipalities that were recently involved in the 2025 mergers. These have been appended on 17 February 2026, albeit with a slightly different resolution as it pertains to the administrative boudaries . The data has a 'detection limit' of 5 people for one particular name at municipal level. This implies a potentially skewed underestimation towards uncommon names.

## Data Processing

- Name ratio in a municipality if both groups are represented
- Name frequencies aggregated at municipal, regional, and national levels  
- Simplified geometries used for performance optimization. Full geometries constituted to severe delay and loading times, upwards of seconds. 

## Technical Details

Built with:

- **R Shiny** for the web application framework  
- **Leaflet** for interactive mapping  
- **PostgreSQL/PostGIS** for a responsive and robust data storage backbone, including storing spatial data  
- **sf** package for spatial operations  
- **luwitemplate** package for custom styling
- **Minisforum UN100P** mini computer with Ubuntu 24.04 LTS server as the hosting platform  
- **Shiny Server** for R application deployment  
- **Cloudflare Tunnel** to facilitate secure public hosting with this free service

## Database layout

![database layout](db.png)
