# Load packages -------------------------------------------------------------

library(tidyverse)   # data wrangling, mapping, string ops 
library(sf)          # simple features for spatial data
library(tigris)      # Census TIGER/Line and cartographic boundaries 
library(janitor)     # data cleaning helpers
library(tidycensus)  # decennial Census data 
library(mapgl)       # visualizing spatial data

# State selection -----------------------------------------------------------

walist <- c("WA")

# Historical town/city populations (CSV from GitHub) -----------------------

# Source: compiled from Wikipedia by a third party into CSV files hosted on GitHub. 
# This block reads the WA CSV, keeps the year columns, drops counties, and sorts.

hist_pops <- map_dfr(
  .x = walist,
  .f = function(x) {
    filen <- sprintf(
      "https://github.com/CreatingData/Historical-Populations/raw/refs/heads/master/wikipedia_state_data/%s.csv",
      x
    )
    read_csv(filen, show_col_types = FALSE) %>%
      select(title:y2010) %>%
      filter(str_detect(title, "County", negate = TRUE)) %>%
      arrange(title)
  }
)


# County geometries for WA --------------------------------------------------

# Download WA counties (cartographic boundary, 1:5m), project to EPSG:6350,
# and keep only county GEOID for later joins. 

wa_cty <- counties(walist, cb = TRUE, resolution = "5m") %>%
  st_transform(crs = 6596) %>%
  select(cty_fips = GEOID)


# 2020 decennial place populations and centroids ---------------------------

# Get 2020 place-level total population ("P1_001N") for WA with geometries,
# project, convert to centroids, then intersect with counties to attach cty_fips. 

wa_pops <- tidycensus::get_decennial(
  geography = "place",
  variables = "P1_001N",
  state = walist,
  year = 2020,
  geometry = TRUE
) %>%
  select(city_fips = 1, city_name = 2, pop20 = 4) %>%
  st_transform(crs = 6596) %>%
  st_centroid() %>%
  st_intersection(wa_cty) %>%
  st_drop_geometry()


# RUCC codes (county-level rural/urban classification) ---------------------

# Read USDA ERS 2023 Rural-Urban Continuum Codes, filter to the RUCC attribute,
# and keep county FIPS and RUCC code. 

rucc <- read_csv(
  "https://ers.usda.gov/sites/default/files/_laserfiche/DataFiles/53251/Ruralurbancontinuumcodes2023.csv",
  show_col_types = FALSE
) %>%
  filter(Attribute == "RUCC_2023") %>%
  select(cty_fips = 1, rucc = 5)


# Helper to normalize place/city names -------------------------------------

# Strip generic suffixes like "city", "town", "village" and trim whitespace.
# This helps align Census place names with the historical CSV naming.

normalize_city_name <- function(name) {
  name %>%
    str_replace_all(" city", "") %>%
    str_replace_all(" town", "") %>%
    str_replace_all(" village", "") %>%
    str_trim()
}


# Clean 2020 WA place names and filter out CDPs ----------------------------

wa_pops2 <- wa_pops %>%
  arrange(city_fips) %>%
  mutate(clean_name = normalize_city_name(city_name)) %>%
  filter(str_detect(city_name, "CDP", negate = TRUE))


# Clean historical names and normalize -------------------------------------

# Apply ad hoc fixes for known naming quirks in the source CSV,
# then normalize to align with Census names.

hist_pops2 <- hist_pops %>%
  mutate(
    # Keep only the WA-specific fix from the multi-state version
    title = ifelse(title == "Seattle", "Seattle, Washington", title),
    clean_name = normalize_city_name(title)
  )


# Join Census 2020 places with historical populations and RUCC ------------- 

# 1. Join place-based 2020 data to historical city/town series via clean_name.
# 2. Attach RUCC via cty_fips.
# 3. Keep FIPS, names, county FIPS, RUCC, historical years 1790–2010, and 2020 population.
# 4. Drop places with missing 2010 population.

joined <- left_join(wa_pops2, hist_pops2, by = "clean_name") %>%
  left_join(rucc, by = "cty_fips") %>%
  select(1, 2, cty_fips, rucc, y1790:y2010, y2020 = pop20) %>%
  arrange(city_fips) %>%
  filter(!is.na(y2010))


# Diagnostics: in which WA places did the join fail? -------------------------------

nojoin <- anti_join(wa_pops2, hist_pops2, by = "clean_name") %>%
  arrange(desc(pop20))

# Tiny populations (LaCrosse and Krupp, WA)...very unlikely to have walkable downtown

# Filter to pre-auto, small-to-mid-sized towns -----------------------------

# Criteria:
# - Did population in 1900–1940 (pre-automobile era) ever exceed 750,
# - Not in central metropolitan county (RUCC > 1),
# - 2020 population between 500 and 150,000.

pre_auto_towns <- joined %>%
  filter(
    y1900 >= 750 | y1910 >= 750 | y1920 >= 750 |
      y1930 >= 750 | y1940 >= 750,
    rucc > 1,
    y2020 > 500,
    y2020 < 150000
  )

# Goal: filter out 1) post-WWII suburbs, 2) tiny places, 3) huge places

# Attach place geometries (polygons) for WA --------------------------------

# Get WA place polygons, join back to selected pre_auto_towns by GEOID,
# sort, and project to EPSG:6350. 

pre_auto_sf <- places(walist, cb = TRUE) %>%
  select(city_fips = GEOID) %>%
  inner_join(pre_auto_towns, by = "city_fips") %>%
  arrange(city_fips) %>%
  st_transform(crs = 6596) %>% 
  select(-(y1790:y2010))

maplibre_view(pre_auto_sf) # visualize which towns/cities are in the "universe"
# Seattle, Tacoma, Spokane NOT on the list (apples ≠ oranges)

# Export to CSV ----------------------------------------------

# Note: CSV cannot directly store geometry; use st_as_text() for WKT if needed.

pre_auto_sf_out <- pre_auto_sf %>%
  mutate(geometry_wkt = st_as_text(geometry)) %>%
  st_drop_geometry()

# Save as CSV; adjust path/filename as desired.
write_csv(pre_auto_sf_out, "data/wa_pre_auto_places.csv")
