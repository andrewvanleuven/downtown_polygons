# Load packages ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(tigris)
library(crsuggest)
library(smoothr)
library(sfhotspot)
library(glue)
library(furrr)
library(future)
library(beepr)

# Parallel workers -------------------------------------------------------------
#
# This script uses furrr to process all 48 states in parallel. Each worker
# handles one state at a time and needs to hold that state's POI data in memory,
# so worker count is the main tunable parameter.
#
# On a high-core machine (e.g., Mac Studio M3 Ultra) you can set n_workers
# close to the available core count. On a standard laptop (8–16 GB RAM,
# 8 cores), running the full batch in parallel may cause memory pressure.
# If that applies to you, set n_workers <- 2 or 3, or skip this script
# entirely and run 02_poi.R and 03_downtown.R manually for each state.

n_workers <- min(parallel::detectCores() - 2, 24)
plan(multisession, workers = n_workers)

# Package list passed to each worker session
worker_pkgs <- c(
  'tidyverse', 'sf', 'tigris', 'crsuggest',
  'smoothr', 'sfhotspot', 'glue'
)

# Settings --------------------------------------------------------------------

include_institutional <- FALSE

pmtiles_path <- Sys.which('pmtiles')
if (pmtiles_path == '') stop('pmtiles not found. Install with: brew install pmtiles')

pmtiles_url <- 'https://overturemaps-tiles-us-west-2-beta.s3.amazonaws.com/2025-06-25/places.pmtiles'

institutional_regex <- c(
  'campus_building', 'college_university', 'hospital',
  'medical_service_organizations'#, 'government_building'
) %>% paste(collapse = '|')

st_list <- states(cb = TRUE, resolution = '20m') %>%
  filter(as.integer(GEOID) < 60, !STUSPS %in% c('AK', 'HI')) %>%
  arrange(GEOID) %>%
  pull(STUSPS)

national_places <- read_csv('data/national_pre_auto_places.csv', show_col_types = FALSE)

# Pre-compute full state names for city name cleaning (used in loop 2)
st_name_lookup <- states(cb = TRUE, resolution = '20m') %>%
  st_drop_geometry() %>%
  select(STUSPS, NAME)


# KDE function ----------------------------------------------------------------

downtown_kde <- function(tfips, pre_auto_sf, st_dots) {

  zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  minmax <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

  town_parts <- pre_auto_sf %>%
    filter(city_fips == tfips) %>%
    st_cast('POLYGON') %>%
    mutate(part_id = row_number(), area = st_area(geometry))

  poi_counts <- st_dots %>%
    filter(city_fips == tfips) %>%
    st_join(town_parts, join = st_within) %>%
    st_drop_geometry() %>%
    count(part_id)

  best_part_id <- if (nrow(poi_counts) > 0) {
    poi_counts %>% slice_max(n, n = 1, with_ties = FALSE) %>% pull(part_id)
  } else {
    town_parts %>% slice_max(area, n = 1, with_ties = FALSE) %>% pull(part_id)
  }

  town_sf <- town_parts %>%
    filter(part_id == best_part_id) %>%
    select(-part_id, -area) %>%
    st_as_sf()

  town_dots <- st_dots %>%
    filter(city_fips == tfips) %>%
    st_intersection(town_sf) %>%
    select(geometry)

  hotspot_result <- hotspot_kde(town_dots, grid_type = 'hex') %>%
    mutate(kdz = zscore(kde), kdm = minmax(kde)) %>%
    filter(kdm > 0.75)

  blobs <- hotspot_result %>%
    st_union() %>%
    st_cast('POLYGON') %>%
    st_sf() %>%
    mutate(blob_id = row_number())

  blob_metrics <- st_join(hotspot_result, blobs) %>%
    st_drop_geometry() %>%
    group_by(blob_id) %>%
    summarise(mean_kdz = mean(kdz, na.rm = TRUE), n_hexes = n(), .groups = 'drop')

  blobs %>%
    left_join(blob_metrics, by = 'blob_id') %>%
    mutate(score = n_hexes * mean_kdz) %>%
    filter(score == max(score, na.rm = TRUE)) %>%
    st_buffer(30, joinStyle = 'MITRE', endCapStyle = 'SQUARE') %>%
    smooth(method = 'ksmooth', smoothness = 5) %>%
    st_buffer(50)
}


# Loop 1: extract POIs for each state -----------------------------------------

extract_poi <- function(st) {

  out_csv <- sprintf('data/poi/clean/pre_auto_poi_%s.csv', st)
  out_raw <- sprintf('data/poi/raw/places_%s.geojsonl', st)

  crs_best <- tryCatch({
    counties(st, cb = TRUE) %>%
      suggest_crs() %>%
      filter(crs_units == 'm') %>%
      first() %>%
      pull(crs_code) %>%
      as.numeric()
  }, error = function(e) { message(glue('{st} CRS error: {e$message}')); NA })

  if (is.na(crs_best)) return(invisible(NULL))

  state_places <- national_places %>%
    filter(state == st) %>%
    mutate(geometry = st_as_sfc(geometry_wkt, crs = 4326)) %>%
    st_as_sf() %>%
    st_transform(crs_best) %>%
    mutate(city_fips = str_pad(city_fips, width = 5, pad = '0'))

  if (nrow(state_places) == 0) {
    message(glue('{st}: no pre-auto places found, skipping.'))
    return(invisible(NULL))
  }

  if (!file.exists(out_raw)) {
    bb <- states(cb = TRUE) %>% filter(STUSPS == st) %>% st_bbox()
    bbox_string <- sprintf('%f,%f,%f,%f', bb$xmin, bb$ymin, bb$xmax, bb$ymax)
    cmd <- sprintf('"%s" extract "%s" "%s" --bbox="%s"',
                   pmtiles_path, pmtiles_url, out_raw, bbox_string)
    system(cmd)
  }

  st_poi <- tryCatch(
    st_read(out_raw, quiet = TRUE) %>% st_transform(crs_best),
    error = function(e) { message(glue('{st} POI read error: {e$message}')); NULL }
  )
  if (is.null(st_poi)) return(invisible(NULL))

  st_poi %>%
    select(id, mvt_id, name = 2, brand, categories, names) %>%
    st_intersection(state_places) %>%
    st_transform(4326) %>%
    mutate(geometry_wkt = st_as_text(geometry)) %>%
    st_drop_geometry() %>%
    mutate(
      city_name     = str_replace(city_name, ' city', ''),
      city_name     = str_replace(city_name, ' town', ''),
      institutional = str_detect(categories, institutional_regex)
    ) %>%
    write_csv(out_csv)

  message(glue('{st}: POIs written to {out_csv}'))
}

future_walk(st_list, extract_poi, .options = furrr_options(packages = worker_pkgs))
beep()

# Loop 2: delineate downtown polygons for each state --------------------------

delineate <- function(st) {

  out_geojson <- sprintf('data/polygons/downtowns_%s.geojson', st)
  in_csv      <- sprintf('data/poi/clean/pre_auto_poi_%s.csv', st)

  if (!file.exists(in_csv)) {
    message(glue('{st}: POI CSV not found, skipping.'))
    return(invisible(NULL))
  }

  st_name <- st_name_lookup %>% filter(STUSPS == st) %>% pull(NAME)

  crs_best <- tryCatch({
    counties(st, cb = TRUE) %>%
      suggest_crs() %>%
      filter(crs_units == 'm') %>%
      first() %>%
      pull(crs_code) %>%
      as.numeric()
  }, error = function(e) { message(glue('{st} CRS error: {e$message}')); NA })

  if (is.na(crs_best)) return(invisible(NULL))

  pre_auto_sf <- national_places %>%
    filter(state == st) %>%
    mutate(geometry = st_as_sfc(geometry_wkt, crs = 4326)) %>%
    st_as_sf() %>%
    st_transform(crs_best) %>%
    mutate(
      city_fips = str_pad(city_fips, width = 5, pad = '0'),
      city_name = str_replace(city_name, ' city', ''),
      city_name = str_replace(city_name, ' town', ''),
      city_name = str_remove_all(city_name, sprintf(', %s', st_name))
    )

  st_dots <- tryCatch(
    read_csv(in_csv, show_col_types = FALSE) %>%
      mutate(geometry = st_as_sfc(geometry_wkt, crs = 4326)) %>%
      st_as_sf() %>%
      st_transform(crs_best) %>%
      mutate(city_name = str_remove_all(city_name, sprintf(', %s', st_name))) %>%
      filter(!institutional | include_institutional),
    error = function(e) { message(glue('{st} dots read error: {e$message}')); NULL }
  )
  if (is.null(st_dots)) return(invisible(NULL))

  all_fips <- pre_auto_sf %>% st_drop_geometry() %>% pull(city_fips) %>% unique()

  all_blobs <- map_dfr(all_fips, ~ {
    tryCatch(
      downtown_kde(.x, pre_auto_sf, st_dots) %>% mutate(city_fips = .x),
      error = function(e) {
        message(glue('{st} FIPS {.x} failed: {e$message}'))
        NULL
      }
    )
  })

  if (nrow(all_blobs) == 0) {
    message(glue('{st}: no polygons produced, skipping write.'))
    return(invisible(NULL))
  }

  city_lookup <- pre_auto_sf %>% st_drop_geometry() %>% select(city_fips, city_name)

  all_blobs %>%
    left_join(city_lookup, by = 'city_fips') %>%
    select(city_fips, city_name, kde_intensity = mean_kdz, downtown_score = score, geometry) %>%
    st_write(out_geojson, append = FALSE, delete_dsn = TRUE, quiet = TRUE)

  message(glue('{st}: {nrow(all_blobs)} polygons written to {out_geojson}'))
}

future_walk(st_list, delineate, .options = furrr_options(packages = worker_pkgs,
                                                          globals = c('national_places',
                                                                      'st_name_lookup',
                                                                      'include_institutional',
                                                                      'downtown_kde')))
beep()


# Compile Dataverse exports ---------------------------------------------------

dir.create('data/dataverse', showWarnings = FALSE)

# Merge all per-state downtown polygon GeoJSONs into one national file.
# A 'state' column is added since city names are not unique across states.
# All geometries are reprojected to EPSG:4326 before binding.
# See README for a description of kde_intensity and downtown_score.
# Merge all per-state clean POI CSVs into one national file first, so that
# n_poi counts are available for the polygon export below.
poi_files <- list.files('data/poi/clean', pattern = '\\.csv$', full.names = TRUE)

all_poi <- map_dfr(poi_files, ~ read_csv(.x, show_col_types = FALSE) %>%
                     mutate(city_fips = as.character(city_fips),
                            cty_fips  = as.character(cty_fips)))

all_poi %>%
  write_csv('data/dataverse/pre_auto_poi_us.csv')

message('Merged POI file written to data/dataverse/pre_auto_poi_us.csv')
beep()

# Per-city POI count (non-institutional only) — joined into the polygon export.
n_poi_lookup <- all_poi %>%
  filter(!institutional) %>%
  count(city_fips, name = 'n_poi')

# Place-level attributes from the universe file to enrich the polygon output.
place_attrs <- national_places %>%
  mutate(city_fips = as.character(city_fips)) %>%
  select(city_fips, pop_20, cty_fips, cty_type, cty_centrality, cty_msa_adj, rucc)

# Merge all per-state downtown polygon GeoJSONs into one national file.
polygon_files <- list.files('data/polygons', pattern = '\\.geojson$', full.names = TRUE)

map_dfr(polygon_files, function(f) {
  st <- str_extract(basename(f), '(?<=downtowns_)[A-Z]{2}')
  st_read(f, quiet = TRUE) %>% st_transform(4326) %>% mutate(state = st)
}) %>%
  left_join(place_attrs,   by = 'city_fips') %>%
  left_join(n_poi_lookup,  by = 'city_fips') %>%
  select(city_fips, city_name, state, pop_20, n_poi,
         kde_intensity, downtown_score,
         cty_fips, cty_type, cty_centrality, cty_msa_adj, rucc,
         geometry) %>%
  st_write('data/dataverse/downtowns_us.geojson', append = FALSE, delete_dsn = TRUE, quiet = TRUE)

message('Merged polygon file written to data/dataverse/downtowns_us.geojson')
beep()

