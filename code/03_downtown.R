# Load packages ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(glue)
library(smoothr)
library(tigris)
library(mapgl)
library(rleuven)
library(crsuggest)
library(sfhotspot)

# Settings --------------------------------------------------------------------

# Pick a state
st_list <- 'WA'
st_name <- states(TRUE, '20m') %>% filter(STUSPS == st_list) %>% pull(NAME)

# Toggle: set to FALSE to exclude institutional POIs (college campus buildings,
# hospitals, government buildings) from the KDE density calculation. These
# generate foot traffic that is largely captive to the institution rather than
# reflective of a walkable commercial core, so excluding them tends to produce
# tighter, more accurate downtown polygons in college towns and medical centers.
# Set to TRUE to include all POIs regardless of category.
include_institutional <- FALSE

# Find the best projected CRS for this state
crs_best <- counties(st_list, cb = TRUE) %>%
  suggest_crs() %>%
  filter(crs_units == 'm') %>%
  first() %>%
  pull(crs_code) %>%
  as.numeric()


# Load input data -------------------------------------------------------------

# Point-of-interest dots intersected with the pre-auto universe (from 02_poi.R).
# Stored as a CSV with WKT geometry in geometry_wkt (lon/lat, EPSG:4326).
st_dots <- read_csv(sprintf('data/poi/clean/pre_auto_poi_%s.csv', st_list), show_col_types = FALSE) %>%
  mutate(geometry = st_as_sfc(geometry_wkt, crs = 4326)) %>%
  st_as_sf() %>%
  st_transform(crs_best) %>%
  mutate(city_name = str_remove_all(city_name, sprintf(', %s', st_name))) %>%
  filter(!institutional | include_institutional)

# Quick sanity check: visually confirm points for a random town
example_town <- sample_n(st_dots, 1) %>% pull(city_name)
maplibre_view(st_dots %>% filter(city_name == example_town))

# Pre-auto place polygons for this state (from 01_universe_national.R)
pre_auto_sf <- read_csv('data/national_pre_auto_places.csv', show_col_types = FALSE) %>%
  filter(state == st_list) %>%
  mutate(geometry = st_as_sfc(geometry_wkt, crs = 4326)) %>%
  st_as_sf() %>%
  st_transform(crs_best) %>%
  mutate(
    city_fips = str_pad(city_fips, width = 5, pad = '0'),
    city_name = str_replace(city_name, ' city', ''),
    city_name = str_replace(city_name, ' town', ''),
    city_name = str_remove_all(city_name, sprintf(', %s', st_name))
  )


# Downtown KDE function -------------------------------------------------------

downtown_kde <- function(tfips) {

  # Standardization helpers
  zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  minmax <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

  # For multipart places, select the polygon part that contains the most POIs.
  # This is more reliable than selecting by area: some places (e.g., townships
  # that include large uninhabited tracts) have a geographically dominant part
  # that contains almost no commercial activity. Falls back to largest area if
  # no POIs fall in any part (e.g., a place with zero Overture records).
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

  # POI dots within the selected polygon part
  town_dots <- st_dots %>%
    filter(city_fips == tfips) %>%
    st_intersection(town_sf) %>%
    select(geometry)

  # Kernel density estimation on a hex grid; retain the top-density hexagons
  hotspot_result <- hotspot_kde(town_dots, grid_type = 'hex') %>%
    mutate(kdz = zscore(kde), kdm = minmax(kde)) %>%
    filter(kdm > 0.75)

  # Dissolve selected hexagons into contiguous blobs and score each blob
  blobs <- hotspot_result %>%
    st_union() %>%
    st_cast('POLYGON') %>%
    st_sf() %>%
    mutate(blob_id = row_number())

  blob_metrics <- st_join(hotspot_result, blobs) %>%
    st_drop_geometry() %>%
    group_by(blob_id) %>%
    summarise(mean_kdz = mean(kdz, na.rm = TRUE), n_hexes = n(), .groups = 'drop')

  # Select the highest-scoring blob, then buffer and smooth the outline
  blobs %>%
    left_join(blob_metrics, by = 'blob_id') %>%
    mutate(score = n_hexes * mean_kdz) %>%
    filter(score == max(score, na.rm = TRUE)) %>%
    st_buffer(30, joinStyle = 'MITRE', endCapStyle = 'SQUARE') %>%
    smooth(method = 'ksmooth', smoothness = 5) %>%
    st_buffer(50)
}


# Quick test ------------------------------------------------------------------

test_name  <- 'Morton'
test_town  <- pre_auto_sf %>% filter(city_name == test_name)
test_dots  <- st_dots %>% filter(city_name == test_name)
test_fips  <- pull(test_town, city_fips)
test_cfips <- pull(test_town, cty_fips)
test_rds   <- roads(state = str_sub(test_cfips, end = 2),
                    county = str_sub(test_cfips, start = 3)) %>%
  st_transform(st_crs(test_town)) %>%
  st_intersection(test_town)
test <- downtown_kde(test_fips)

a <- mapboxgl(bounds = test_town, style = mapbox_style('dark')) %>%
  add_fill_layer(id = 'Town Boundaries', source = test_town,
                 fill_color = 'white', fill_opacity = 0.1) %>%
  add_fill_layer(id = 'Downtown District', source = test,
                 fill_color = 'dodgerblue', fill_opacity = 0.5) %>%
  add_line_layer(id = 'town_outline', source = test_town,
                 line_color = 'white', line_width = 0.75) %>%
  add_line_layer(id = 'downtown_outline', source = test,
                 line_color = 'white', line_width = 1) %>%
  add_circle_layer(id = 'Points of Interest', source = test_dots,
                   circle_color = '#BB0000', circle_radius = 3,
                   tooltip = 'name', circle_opacity = 0.8,
                   hover_options = list(circle_color = 'yellow', circle_radius = 5)) %>%
  add_layers_control(position = 'top-left',
                     layers = c('Town Boundaries', 'Points of Interest', 'Downtown District'))

b <- mapboxgl(bounds = test_town, style = mapbox_style('satellite')) %>%
  add_fill_layer(id = 'Town Boundaries', source = test_town,
                 fill_color = 'white', fill_opacity = 0.1) %>%
  add_fill_layer(id = 'Downtown District', source = test,
                 fill_color = 'dodgerblue', fill_opacity = 0.5) %>%
  add_line_layer(id = 'town_outline', source = test_town,
                 line_color = 'white', line_width = 0.75) %>%
  add_line_layer(id = 'downtown_outline', source = test,
                 line_color = 'white', line_width = 1) %>%
  add_circle_layer(id = 'Points of Interest', source = test_dots,
                   circle_color = '#BB0000', circle_radius = 3,
                   tooltip = 'name', circle_opacity = 0.8,
                   hover_options = list(circle_color = 'yellow', circle_radius = 5)) %>%
  add_layers_control(position = 'top-left',
                     layers = c('Town Boundaries', 'Points of Interest', 'Downtown District'))

compare(a, b)

# The ggplot code below is used to create the map shown in this repository's README page:

# ggplot() +
#   geom_sf(data = test_town, color = 'white', fill = 'grey20', linewidth = 0.8) +
#   geom_sf(data = test, fill = 'dodgerblue', alpha = 0.6, color = 'white', linewidth = 0.5) +
#   geom_sf(data = test_rds, color = 'gray70', alpha = 0.4, linewidth = 0.3) +
#   geom_sf(data = test_dots, color = '#FF4444', alpha = 0.75, size = 1.25) +
#   labs(
#     title    = 'Downtown District Delineation',
#     subtitle = glue('{test_name}, {st_list}'),
#     caption  = 'Source: Overture Maps, 2025        \n'
#   ) +
#   theme_void(base_family = 'Styrene A', base_size = 18) +
#   theme(
#     plot.title    = element_text(face = 'bold', hjust = 0.5, color = 'white',
#                                  margin = margin(t = 20)),
#     plot.subtitle = element_text(hjust = 0.5, color = 'gray90'),
#     plot.caption  = element_text(color = 'gray70', size = 12),
#     plot.background = element_rect(color = 'white')
#   )
# ggsave('plot/demo_dark.png', width = 11, height = 8.5, dpi = 300)


# Run for all towns in a state ------------------------------------------------

all_fips <- pre_auto_sf %>%
  st_drop_geometry() %>%
  pull(city_fips) %>%
  unique()

# Run downtown_kde() for every city, catching errors so the loop continues.
all_blobs <- map_dfr(all_fips, ~ {
  tryCatch(
    downtown_kde(.x) %>% mutate(city_fips = .x),
    error = function(e) {
      message(glue('Failed for FIPS {.x}: {e$message}'))
      NULL
    }
  )
})


# Inspect and export ----------------------------------------------------------

# Join city names, drop internal scoring columns, and rename KDE metrics.
#   kde_intensity  = mean z-score of KDE values across the hexagons comprising
#                    the selected blob; higher values indicate denser POI clustering
#   downtown_score = kde_intensity × hex count; the composite metric used to
#                    select the primary blob per city (retained for reproducibility)

city_lookup <- pre_auto_sf %>%
  st_drop_geometry() %>%
  select(city_fips, city_name)

all_downtown_polygons <- all_blobs %>%
  left_join(city_lookup, by = 'city_fips') %>%
  select(city_fips, city_name, kde_intensity = mean_kdz, downtown_score = score, geometry)

nrow(all_downtown_polygons)
maplibre_view(st_centroid(all_downtown_polygons))

st_write(
  all_downtown_polygons,
  sprintf('data/polygons/downtowns_%s.geojson', st_list),
  append     = FALSE,
  delete_dsn = TRUE
)
