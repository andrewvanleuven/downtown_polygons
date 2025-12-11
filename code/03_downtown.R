library(tidyverse)
library(sf)
library(glue)
library(smoothr)
library(tigris)
library(mapgl)
library(rleuven)
library(crsuggest)
library(sfhotspot)

# ---------------- INPUT DATA ----------------

# Pick a state
st_list <- "WA"

# Find the best CRS for plotting/GIS
crs_best <- counties(st_list, cb = TRUE) %>% 
  suggest_crs() %>% 
  filter(crs_units == "m") %>% 
  first() %>% 
  pull(crs_code) %>% 
  as.numeric()

# Point-of-interest dots intersected with the pre-auto universe (from previous script).
# These are stored as a CSV with WKT geometry in geometry_wkt (lon/lat, EPSG:4326). 
st_dots <- read_csv("data/pre_auto_poi.csv", show_col_types = FALSE) %>%
  mutate(geometry = st_as_sfc(geometry_wkt, crs = 4326)) %>%
  st_as_sf() %>%
  st_transform(crs_best) %>% 
  mutate(city_name = str_remove_all(city_name, ", Washington"))

# Quick sanity check: visually confirm points 
maplibre_view(st_dots %>% filter(city_name == "Kelso"))

# Pre-auto WA place polygons (universe of towns/cities of interest),
pre_auto <- read_csv("data/st_pre_auto_places.csv", show_col_types = FALSE) %>% 
  select(1:5) %>%
  mutate(city_fips = str_pad(city_fips, width = 5, pad = "0"))

pre_auto_sf <- places(st_list, cb = TRUE) %>%
  st_transform(crs = crs_best) %>%
  select(city_fips = GEOID) %>%
  filter(city_fips %in% pull(pre_auto, city_fips)) %>% 
  left_join(pre_auto) %>%
  # Clean up city names for nicer labels / joins.
  mutate(
    city_name = str_replace(city_name, " city", ""),
    city_name = str_replace(city_name, " town", ""),
    city_name = str_remove_all(city_name, ", Washington")
  )


# ---------------- DOWNTOWN KDE FUNCTION ----------------

downtown_kde <- function(tfips) {
  
  # Standardization helpers
  zscore <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
  
  minmax <- function(x) {
    (x - min(x, na.rm = TRUE)) /
      (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  
  # Largest polygon for the town (handles multipart places)
  town_sf <- pre_auto_sf %>%
    filter(city_fips == tfips) %>%
    st_cast("POLYGON") %>%
    mutate(area = st_area(geometry)) %>%
    slice_max(order_by = area, n = 1, with_ties = FALSE) %>%
    select(-area) %>%
    st_as_sf()
  
  # POI dots within that town (point-in-polygon filter)
  town_dots <- st_dots %>%
    filter(city_fips == tfips) %>%
    st_intersection(town_sf) %>%
    select(geometry)
  
  # Kernel density estimation on a hex grid, then keep high-density hexes. 
  hotspot_result <- hotspot_kde(
    town_dots,
    grid_type = "hex"
  ) %>%
    mutate(
      kdz = zscore(kde),
      kdm = minmax(kde)
    ) %>%
    filter(kdm > 0.75)
  
  # Dissolve selected hexagons into contiguous blobs
  blobs <- hotspot_result %>%
    st_union() %>%
    st_cast("POLYGON") %>%
    st_sf() %>%
    mutate(blob_id = row_number())
  
  # Blob-level metrics: mean KDE z-score and hex count
  blob_metrics <- st_join(hotspot_result, blobs) %>%
    st_drop_geometry() %>%
    group_by(blob_id) %>%
    summarise(
      mean_kdz = mean(kdz, na.rm = TRUE),
      n_hexes  = n(),
      .groups  = "drop"
    )
  
  # Attach metrics and compute composite score
  blobs_with_metrics <- blobs %>%
    left_join(blob_metrics, by = "blob_id")
  
  # Select the best blob (largest/densest), then buffer and smooth the outline
  best_blob <- blobs_with_metrics %>%
    mutate(score = n_hexes * mean_kdz) %>%
    filter(score == max(score, na.rm = TRUE)) %>%
    st_buffer(30, joinStyle = "MITRE", endCapStyle = "SQUARE") %>%
    smooth(method = "ksmooth", smoothness = 5) %>% st_buffer(50)
  
  best_blob
}


# ---------------- QUICK TEST ----------------
# Fix broken Roslyn, WA 
pre_auto_sf <- pre_auto_sf %>%
  filter(city_name != "Roslyn") %>%
  bind_rows(
    filter(pre_auto_sf, city_name == "Roslyn") %>%
      st_cast("POLYGON") %>%
      slice(1)
  )

# maplibre_view() gives a fast visual check. 
test_name <- "Morton"
test_town <- pre_auto_sf %>% filter(city_name == test_name)
test_dots <- st_dots %>% filter(city_name == test_name)
test_fips <- pull(test_town, city_fips)
test_cfips <- pull(test_town, cty_fips)
test_rds <- roads(state = str_sub(test_cfips, end = 2),
                  county = str_sub(test_cfips, start = 3)) %>% 
  st_transform(st_crs(test_town)) %>% 
  st_intersection(test_town)
test <- downtown_kde(test_fips)

a <- mapboxgl(bounds = test_town, style = mapbox_style("dark")) %>%
  add_fill_layer(id = "Town Boundaries", source = test_town, 
                 fill_color = "white", fill_opacity = 0.1) %>%
  add_fill_layer(id = "Downtown District", source = test, 
                 fill_color = "dodgerblue", fill_opacity = 0.5) %>%
  add_line_layer(id = "town_outline", source = test_town,
                 line_color = "white", line_width = .75) %>%
  add_line_layer(id = "downtown_outline", source = test,
                 line_color = "white", line_width = 1) %>%
  add_circle_layer(id = "Points of Interest", source = test_dots, 
                   circle_color = "#BB0000", circle_radius = 3,
                   tooltip = "name", circle_opacity = 0.8,
                   hover_options = list(circle_color = "yellow", 
                                        circle_radius = 5)) %>%
  add_layers_control(position = "top-left",
                     layers = c("Town Boundaries", 
                                "Points of Interest",
                                "Downtown Business District"))

b <- mapboxgl(bounds = test_town, style = mapbox_style("satellite")) %>%
  add_fill_layer(id = "Town Boundaries", source = test_town, 
                 fill_color = "white", fill_opacity = 0.1) %>%
  add_fill_layer(id = "Downtown District", source = test, 
                 fill_color = "dodgerblue", fill_opacity = 0.5) %>%
  add_line_layer(id = "town_outline", source = test_town,
                 line_color = "white", line_width = .75) %>%
  add_line_layer(id = "downtown_outline", source = test,
                 line_color = "white", line_width = 1) %>%
  add_circle_layer(id = "Points of Interest", source = test_dots, 
                   circle_color = "#BB0000", circle_radius = 3,
                   tooltip = "name", circle_opacity = 0.8,
                   hover_options = list(circle_color = "yellow", 
                                        circle_radius = 5)) %>%
  add_layers_control(position = "top-left",
                     layers = c("Town Boundaries", 
                                "Points of Interest",
                                "Downtown Business District"))

compare(a,b)

ggplot() + 
  geom_sf(data = test_town, color = "white", fill = "grey20", linewidth = 0.8) + 
  geom_sf(data = test, fill = "dodgerblue", alpha = .6, color = "white", linewidth = 0.5) + 
  geom_sf(data = test_rds, color = "gray70", alpha = .4, linewidth = 0.3) + 
  geom_sf(data = test_dots, color = "#FF4444", alpha = .75, size = 1.25) + 
  labs(
    title = "Downtown District Delineation",
    subtitle = glue("{test_name}, {st_list}"),
    caption = "Source: Overture Maps, 2025        \n"
  ) +
  theme_void(base_family = "Styrene A", base_size = 18) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "white",
                              margin = margin(t = 20)),
    plot.subtitle = element_text(hjust = 0.5, color = "gray90"),
    plot.caption = element_text(color = "gray70", size = 12),
    plot.background = element_rect(color = "white"),
  )

ggsave("plot/demo_dark.png", width = 11, height = 8.5, dpi = 300)


# ---------------- RUN FOR ALL WA TOWNS ----------------

# Get unique FIPS for all pre-auto towns.
all_fips <- pre_auto_sf %>%
  st_drop_geometry() %>%
  pull(city_fips) %>%
  unique()

# Run downtown_kde() for each city, catching errors but continuing.
all_blobs <- all_fips %>%
  map_dfr(~ {
    tryCatch(
      {
        downtown_kde(.x) %>%
          mutate(city_fips = .x)
      },
      error = function(e) {
        message(glue("Failed for FIPS {.x}: {e$message}"))
        NULL
      }
    )
  })

# Roslyn, WA failed...I fixed it above. YMMV

# ---------------- INSPECT AND EXPORT ----------------

nrow(all_blobs)
maplibre_view(all_blobs)

# Write WA downtown polygons to GeoJSON (overwrite if it exists). 
st_write(
  all_blobs,
  "data/st_downtowns.geojson",
  append    = FALSE,
  delete_dsn = TRUE
)
