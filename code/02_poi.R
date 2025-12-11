# ---------------- PACKAGES ----------------

library(tidyverse)  # data manipulation, pipes
library(sf)         # simple features for spatial data
library(tigris)     # Census TIGER/Line and cartographic boundaries 
library(mapgl)      # quick interactive map viewing
library(crsuggest)  # used for identifying suitable coord. ref. system

# ---------------- SETTINGS ----------------

# Pick a state
st_list <- "WA"

# Input files and paths
pre_auto_csv <- "data/st_pre_auto_places.csv"

# PMTiles vector tiles of global places (Protomaps / Overture-style tiles). 
pmtiles_url  <- "https://overturemaps-tiles-us-west-2-beta.s3.amazonaws.com/2025-06-25/places.pmtiles"

# Output GeoJSONL of WA subset of places (extracted from PMTiles)
st_geojson   <- "data/places_washington.geojsonl"


# ---------------- LOAD PRE-AUTO PLACES ----------------

# Load the CSV created in the previous script
pre_auto <- read_csv(pre_auto_csv, show_col_types = FALSE) %>% 
  select(1:2) %>% 
  mutate(city_fips = str_pad(city_fips, width = 5, pad = "0"))

# Find the best CRS for plotting/GIS
crs_best <- counties(st_list, cb = TRUE) %>% 
  suggest_crs() %>% 
  filter(crs_units == "m") %>% 
  first() %>% 
  pull(crs_code) %>% 
  as.numeric()

# Get WA place polygons from TIGER and keep only those in the pre-auto universe. 
pre_auto_sf <- places(st_list, cb = TRUE) %>%
  st_transform(crs = crs_best) %>% 
  select(city_fips = GEOID) %>%
  filter(city_fips %in% pull(pre_auto, city_fips)) %>% 
  left_join(pre_auto)


# ---------------- GET WASHINGTON BBOX ----------------

# Download all US states, filter to WA, and compute its bounding box. 
st <- states(cb = TRUE) %>%
  filter(STUSPS == st_list)

bb <- st_bbox(st)

# PMTiles CLI expects bbox as "min_lon,min_lat,max_lon,max_lat". 
bbox_string <- sprintf(
  "%f,%f,%f,%f",
  bb$xmin, bb$ymin, bb$xmax, bb$ymax
)


# ---------------- CONFIRM PMTILES CLI EXISTS ----------------

# The pmtiles CLI must be installed and on PATH.
# On macOS, an easy install is: brew install pmtiles. 
pmtiles_path <- Sys.which("pmtiles")

if (pmtiles_path == "") {
  stop("pmtiles not found. Install with: brew install pmtiles")
}


# ---------------- RUN EXTRACTION FROM PMTILES ----------------

if (file.exists(st_geojson)) {
  # If the file already exists, skip extraction but report it.
  cat("✓ Skipping extraction; file already exists:", st_geojson, "\n")
} else {
  # Construct a system command to extract the WA bounding box from the global
  # places PMTiles archive to a GeoJSONL file. [web:26][web:67]
  cmd <- sprintf(
    '"%s" extract "%s" "%s" --bbox="%s"',
    pmtiles_path,
    pmtiles_url,
    st_geojson,
    bbox_string
  )
  
  system(cmd)
  cat("✓ Done. Output:", st_geojson, "\n")
}

# ---------------- LOAD POIs FOR WASHINGTON ----------------

# Read the extracted WA places layer (GeoJSONL) and project to WA CRS.
# Depending on file size, this can take around 1 minute. 
st_poi <- st_read(st_geojson, quiet = FALSE) %>%
  st_transform(crs_best)


# ---------------- INTERSECT POIs WITH PRE-AUTO PLACES ----------------

# Keep key attributes, then spatially intersect with the pre-auto place polygons
# to assign each POI to a pre-auto town/city.
pre_auto_poi <- st_poi %>%
  select(
    id,
    mvt_id,
    name = 2,     
    brand,
    categories,
    names
  ) %>%
  st_intersection(pre_auto_sf)

# ---------------- QUICK MAPVIEW CHECK ----------------

# Example: visualize POIs in one specific pre-auto place (you can swap out any town name)
maplibre_view(pre_auto_poi %>% filter(str_detect(city_name, "Kelso")))
        
pre_auto_poi_out <- pre_auto_poi %>%
  st_transform(4326) %>% 
  mutate(geometry_wkt = st_as_text(geometry)) %>%
  st_drop_geometry() %>%
  mutate(
    city_name = str_replace(city_name, " city", ""),
    city_name = str_replace(city_name, " town", "")
  )

write_csv(pre_auto_poi_out, "data/pre_auto_poi.csv")

