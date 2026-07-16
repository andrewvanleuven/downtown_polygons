# Manual visual validation sample (R1.4/R1.5 "sampled manual validation") -----
#
# Draws a reproducible random sample of 30 towns from the full pre-automobile
# universe (not just towns with a Main Street district, which is a biased
# subset) and steps through them one at a time: a Mapbox satellite view with
# the delineated downtown polygon overlaid as a thick yellow outline and no
# fill, so the boundary can be checked directly against the historic core
# visible in the imagery. This is independent evidence from the Main Street
# comparison in 02_validation.R.
#
# Usage (RStudio, interactive only — this will not run via Rscript):
#   1. Source everything above the "Step through the sample" section once.
#   2. Place the cursor on the first `rate_next_town()` call below and press
#      Cmd+Enter (Mac) or Ctrl+Enter (Windows/Linux). RStudio auto-advances
#      the cursor to the next line after each run, so repeatedly pressing
#      Cmd+Enter/Ctrl+Enter walks through all 30 towns in order.
#   3. Each press opens the town's satellite view in the Viewer pane, then
#      prompts for a rating in the console (type a value, press Enter):
#        1 = captures core       4 = no downtown visible in imagery
#        2 = partial capture     5 = captures too much beyond the core
#        3 = miss (polygon located away from the visible core)
#   4. Progress is written to output/manual_validation_ratings.csv after
#      every town, so the session can be closed and resumed later — already-
#      rated towns are skipped automatically on re-source.

library(tidyverse)
library(sf)
library(mapgl)
library(glue)

set.seed(60637)   # fixed seed: the sample is reproducible from this script alone

out_dir <- 'code/paper/rr/output'
out_csv <- file.path(out_dir, 'manual_validation_ratings.csv')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- Draw the sample ----------------------------------------------------------

universe <- read_csv('data/national_pre_auto_places.csv', show_col_types = FALSE) |>
  mutate(city_fips = str_pad(city_fips, width = 7, pad = '0'))

sample30 <- universe |>
  distinct(city_fips, .keep_all = TRUE) |>
  slice_sample(n = 30) |>
  select(city_fips, city_name, state, pop_20) |>
  arrange(state, city_name)

write_csv(sample30, file.path(out_dir, 'manual_validation_sample.csv'))

# --- Load downtown polygons for just the sampled states -----------------------

poly_by_state <- sample30 |>
  distinct(state) |>
  pull(state) |>
  set_names() |>
  map(\(st) st_read(sprintf('data/polygons/downtowns_%s.geojson', st), quiet = TRUE) |>
        mutate(city_fips = str_pad(city_fips, width = 7, pad = '0')))

get_polygon <- function(fips, st) poly_by_state[[st]] |> filter(city_fips == fips)

# --- Interactive rating loop ---------------------------------------------------

queue <- sample30
results <- if (file.exists(out_csv)) {
  read_csv(out_csv, show_col_types = FALSE)
} else {
  tibble(city_fips = character(), city_name = character(), state = character(),
         rating = character(), notes = character(), rated_at = character())
}
queue <- queue |> filter(!city_fips %in% results$city_fips)  # resume support
idx <- nrow(sample30) - nrow(queue)

rate_next_town <- function() {
  if (nrow(queue) == 0) {
    cat('\nAll', nrow(sample30), 'towns rated. Results saved to', out_csv, '\n')
    print(count(results, rating))
    return(invisible(NULL))
  }

  idx  <<- idx + 1
  town <- queue[1, ]
  queue <<- queue[-1, ]

  poly   <- get_polygon(town$city_fips, town$state)
  bounds <- poly |> st_transform(4326) |> st_buffer(400) |> st_bbox()

  cat(glue('\n[{idx}/{nrow(sample30)}] {town$city_name}, {town$state} ',
           '(FIPS {town$city_fips}, pop {town$pop_20})\n'))

  map <- mapboxgl(bounds = bounds, style = mapbox_style('satellite')) |>
    add_line_layer(id = 'downtown_outline', source = st_transform(poly, 4326),
                   line_color = 'yellow', line_width = 5, line_opacity = 1)
  print(map)

  rating <- readline('Rating [1=captures core, 2=partial, 3=miss, 4=no downtown visible, 5=captures too much]: ')
  notes  <- readline('Notes (optional, Enter to skip): ')

  results <<- bind_rows(results, tibble(
    city_fips = town$city_fips, city_name = town$city_name, state = town$state,
    rating = rating, notes = notes, rated_at = as.character(Sys.time())
  ))
  write_csv(results, out_csv)

  if (nrow(queue) == 0) {
    cat('\nDone. Results saved to', out_csv, '\n')
    print(count(results, rating))
  }
}

# --- Step through the sample ---------------------------------------------------
# Put the cursor on the line below and press Cmd+Enter / Ctrl+Enter, then keep
# pressing it — RStudio auto-advances to the next call each time.

rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
rate_next_town()
