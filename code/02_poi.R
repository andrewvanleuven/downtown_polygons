# Load packages ---------------------------------------------------------------

library(tidyverse)  # data manipulation, pipes
library(sf)         # simple features for spatial data
library(tigris)     # Census TIGER/Line and cartographic boundaries
library(mapgl)      # quick interactive map viewing
library(crsuggest)  # identifying suitable coordinate reference systems

# Settings --------------------------------------------------------------------

# Pick a state
st_list <- 'WA'

# Input/output paths
pre_auto_csv <- 'data/national_pre_auto_places.csv'
pmtiles_url  <- 'https://overturemaps-tiles-us-west-2-beta.s3.amazonaws.com/2025-06-25/places.pmtiles'
st_geojson   <- sprintf('data/poi/raw/places_%s.geojsonl', st_list)


# Load pre-auto universe ------------------------------------------------------

pre_auto <- read_csv(pre_auto_csv, show_col_types = FALSE) %>%
  filter(state == st_list) %>%
  select(1:2) %>%
  mutate(city_fips = str_pad(city_fips, width = 5, pad = '0'))

# Find the best projected CRS for this state
crs_best <- counties(st_list, cb = TRUE) %>%
  suggest_crs() %>%
  filter(crs_units == 'm') %>%
  first() %>%
  pull(crs_code) %>%
  as.numeric()

# Place polygons for the pre-auto universe
pre_auto_sf <- places(st_list, cb = TRUE) %>%
  st_transform(crs = crs_best) %>%
  select(city_fips = GEOID) %>%
  filter(city_fips %in% pull(pre_auto, city_fips)) %>%
  left_join(pre_auto, by = 'city_fips')


# Extract POIs from PMTiles ---------------------------------------------------

# Compute the state bounding box for the pmtiles CLI extract command.
# PMTiles CLI expects bbox as 'min_lon,min_lat,max_lon,max_lat'.
bb <- states(cb = TRUE) %>%
  filter(STUSPS == st_list) %>%
  st_bbox()

bbox_string <- sprintf('%f,%f,%f,%f', bb$xmin, bb$ymin, bb$xmax, bb$ymax)

# The pmtiles CLI must be installed and on PATH (macOS: brew install pmtiles).
pmtiles_path <- Sys.which('pmtiles')

if (pmtiles_path == '') {
  stop('pmtiles not found. Install with: brew install pmtiles')
}

if (file.exists(st_geojson)) {
  cat('\u2713 Skipping extraction; file already exists:', st_geojson, '\n')
} else {
  cmd <- sprintf('"%s" extract "%s" "%s" --bbox="%s"',
                 pmtiles_path, pmtiles_url, st_geojson, bbox_string)
  system(cmd)
  cat('\u2713 Done. Output:', st_geojson, '\n')
}


# Load and intersect POIs -----------------------------------------------------

# Read the extracted state places layer (GeoJSONL) and project to state CRS.
# Depending on file size this can take around a minute.
st_poi <- st_read(st_geojson, quiet = FALSE) %>%
  st_transform(crs_best)

# Keep key attributes, then spatially intersect with the pre-auto place polygons
# to assign each POI to a pre-auto town/city.
pre_auto_poi <- st_poi %>%
  select(id, mvt_id, name = 2, brand, categories, names) %>%
  st_intersection(pre_auto_sf)

# Quick visual check
example_town <- sample_n(pre_auto_sf, 1) %>% pull(city_name)
maplibre_view(pre_auto_poi %>% filter(city_name == example_town))


# Flag institutional POIs -----------------------------------------------------

# Category strings that indicate a POI is part of an institutional campus
# rather than a freestanding commercial establishment. These are detected via
# substring match on the raw categories JSON field. The flag is written to the
# output CSV so that downstream scripts can include or exclude them as needed.

institutional_regex <- c(
  'campus_building',              # college/university campus facilities
  'college_university',           # higher-ed institutions themselves
  'hospital',                     # hospitals
  'medical_service_organizations', # oncology centers, clinics, etc.
  'government_building'           # courthouses, municipal buildings, etc.
) %>% paste(collapse = '|')

pre_auto_poi_out <- pre_auto_poi %>%
  st_transform(4326) %>%
  mutate(geometry_wkt = st_as_text(geometry)) %>%
  st_drop_geometry() %>%
  mutate(
    city_name     = str_replace(city_name, ' city', ''),
    city_name     = str_replace(city_name, ' town', ''),
    institutional = str_detect(categories, institutional_regex)
  )

write_csv(pre_auto_poi_out, sprintf('data/poi/clean/pre_auto_poi_%s.csv', st_list))
