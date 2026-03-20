# Load packages ---------------------------------------------------------------

library(tidyverse)   # data wrangling, string ops
library(sf)          # simple features for spatial data
library(tigris)      # Census TIGER/Line and cartographic boundaries
library(janitor)     # data cleaning helpers
library(tidycensus)  # decennial Census data
library(mapgl)       # visualizing spatial data

# State list ------------------------------------------------------------------

# All 48 contiguous U.S. states (excluding AK, HI, and territories).

st_list <- states(cb = TRUE, resolution = '20m') %>%
  filter(as.integer(GEOID) < 60, !STUSPS %in% c('AK', 'HI')) %>%
  arrange(GEOID) %>%
  pull(STUSPS)


# OMB county classifications --------------------------------------------------

# Source: OMB 2023 Core-Based Statistical Area delineation file.
# Classifies counties as Metro, Micro, or Non-Core, and as Central or Outlying.
# Used alongside RUCC to construct a finer-grained urban/rural filter.

omb <- rio::import(
  'https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2023/delineation-files/list1_2023.xlsx',
  skip = 2
) %>%
  clean_names() %>%
  rename(ctype = 5) %>%
  mutate(
    cty_fips = paste0(fips_state_code, fips_county_code),
    cty_type = case_when(
      str_detect(ctype, 'Metro') ~ 'Metro',
      str_detect(ctype, 'Micro') ~ 'Micro',
      .default                   = 'Non-Core'
    )
  ) %>%
  select(cty_fips, cty_type, central_outlying_county) %>%
  filter(cty_fips != 'NANA')


# RUCC codes ------------------------------------------------------------------

# Source: USDA ERS 2023 Rural-Urban Continuum Codes.
# Codes 1–3 = metro-adjacent; 4–9 = nonmetro, ranging from large towns to
# completely rural. Joined with OMB classifications for a composite filter.

rucc <- read_csv(
  'https://ers.usda.gov/sites/default/files/_laserfiche/DataFiles/53251/Ruralurbancontinuumcodes2023.csv',
  show_col_types = FALSE
) %>%
  filter(Attribute == 'RUCC_2023') %>%
  mutate(cty_fips = str_pad(FIPS, width = 5, pad = '0')) %>%
  left_join(omb, by = 'cty_fips') %>%
  mutate(rucc = as.integer(Value)) %>%
  select(
    cty_fips,
    rucc,
    cty_type,
    cty_centrality = central_outlying_county
  ) %>%
  replace_na(list(cty_centrality = '--', cty_type = 'Non-Core')) %>%
  mutate(
    cty_msa_adj = case_when(
      rucc %in% c(4, 6, 8) ~ 'Adjacent',
      rucc %in% c(5, 7, 9) ~ 'Non-Adjacent',
      rucc %in% c(1, 2, 3) ~ 'In Metro'
    )
  )


# 2020 decennial place populations (national) ---------------------------------

# Fetch total population ('P1_001N') for all Census-designated places in the
# contiguous U.S., project to Albers Equal Area (EPSG:6350), and convert
# polygons to centroids for point-in-polygon county assignment.

univ_pop <- get_decennial(
  geography = 'place',
  variables = 'P1_001N',
  year      = 2020,
  geometry  = TRUE
) %>%
  select(GEOID, city_name = NAME, pop_20 = value) %>%
  st_transform(6350) %>%
  st_centroid()


# County geometries with RUCC attributes --------------------------------------

# Download county polygons for the contiguous states, attach RUCC/OMB fields,
# and project to match the place centroids above.

cty_univ <- counties(st_list, cb = TRUE, resolution = '5m') %>%
  select(cty_fips = GEOID, cty_name = NAME, state = STUSPS) %>%
  left_join(rucc, by = 'cty_fips') %>%
  st_transform(6350)


# CDP exception states --------------------------------------------------------

# In New England and several township-heavy states, CDPs often correspond to
# genuine historic town centers rather than post-WWII sprawl. These states are
# exempted from the blanket CDP exclusion applied elsewhere.

cdp_exception_states <- c(
  'CT', 'ME', 'MA', 'NH', 'RI', 'VT',
  'NY', 'NJ', 'PA', 'MI', 'MN', 'WI'
)


# Build candidate universe ----------------------------------------------------

# Intersect place centroids with county polygons to attach county-level
# attributes, then apply three filters:
#   1. Exclude CDPs (except in CDP-exception states).
#   2. Population between 500 and 150,000 in 2020.
#   3. Not in a central metro county — RUCC >= 2 or (RUCC == 1 and outlying).
#      This keeps small cities in metro-adjacent counties while excluding core
#      suburbs and large metros where a different downtown-delineation approach
#      is warranted.

universe_candidates <- st_intersection(univ_pop, cty_univ) %>%
  select(
    city_fips     = GEOID,
    city_name,
    state,
    pop_20,
    cty_fips,
    cty_type,
    cty_centrality,
    cty_msa_adj,
    rucc
  ) %>%
  filter(
    str_detect(city_name, 'CDP', negate = TRUE) | state %in% cdp_exception_states,
    pop_20 >= 500,
    pop_20 <= 150000,
    rucc >= 2 | (rucc == 1 & cty_centrality != 'Central')
  ) %>%
  arrange(city_fips)


# Historical town/city populations (CSV from GitHub) --------------------------

# Source: compiled from Wikipedia into per-state CSVs by the CreatingData project.
# Each CSV contains decennial Census population figures from 1790 to 2010.

wiki_hist_pops <- map_dfr(
  .x = st_list,
  .f = function(x) {
    filen <- sprintf(
      'https://github.com/CreatingData/Historical-Populations/raw/refs/heads/master/wikipedia_state_data/%s.csv',
      x
    )
    read_csv(filen, show_col_types = FALSE) %>%
      select(title:y2010) %>%
      arrange(title)
  }
)


# Helper: normalize place names -----------------------------------------------

# Strips Census-style suffixes (city, town, village, borough, etc.) and trims
# whitespace. Applied to both Census names and historical CSV titles so the
# two datasets can be joined on a common key.

normalize_city_name <- function(name) {
  name %>%
    str_replace_all(' city', '') %>%
    str_replace_all(' town', '') %>%
    str_replace_all(' CDP', '') %>%
    str_replace_all(' village', '') %>%
    str_replace_all(' borough', '') %>%
    str_replace_all(' unified government', '') %>%
    str_replace_all(' metropolitan government', '') %>%
    str_replace_all(' consolidated government', '') %>%
    str_replace_all(' Town, Mass', ', Mass') %>%
    str_replace_all(' \\(balance\\)', '') %>%
    str_replace_all('  ', ' ') %>%
    str_trim()
}


# Clean Census place names ----------------------------------------------------

# Drop geometry, apply name normalization, and manually resolve two IL places
# (Windsor) that share a clean name across different counties.
# Also exclude a handful of records that are spurious duplicates or CDPs
# mis-classified in their state's data.

tiger_places <- universe_candidates %>%
  st_drop_geometry() %>%
  arrange(city_fips) %>%
  mutate(clean_name = normalize_city_name(city_name)) %>%
  mutate(
    clean_name = ifelse(city_fips == 1782309, 'Windsor, Mercer County, Illinois', clean_name),
    clean_name = ifelse(city_fips == 1782322, 'Windsor, Shelby County, Illinois', clean_name)
  ) %>%
  filter(
    city_name != 'Superior village, Wisconsin',
    city_name != 'Seymour CDP, Wisconsin',
    city_name != 'Middletown CDP, Pennsylvania',
    city_name != 'Brownstown CDP, Pennsylvania'
  )


# Clean historical place names ------------------------------------------------

# The Wikipedia CSV titles often include county or state disambiguation
# (e.g., 'Fulton, Oswego County, New York') that the Census name does not.
# This block standardizes known mismatches so both sides join on clean_name.
# After normalization, when multiple rows share a clean_name (e.g., duplicate
# Wikipedia entries for the same place) we keep the row with the highest 2010
# population to avoid inflating the pre-auto filter.

hist_pops_clean <- wiki_hist_pops %>%
  mutate(
    # New York: strip county disambiguation and parenthetical type labels
    title = str_replace_all(title, ' \\(village\\), New York', ', New York'),
    title = str_replace_all(title, ' \\(city\\), New York',   ', New York'),
    # Vermont: strip parenthetical type labels
    title = str_replace_all(title, ' \\(city\\), Vermont',    ', Vermont'),
    # Normalize em/en dashes to hyphens
    title = str_replace_all(title, '[\u2013\u2014]', '-'),
    # Place-by-place name corrections (Census name → Wikipedia title)
    title = ifelse(title == 'Boise, Idaho',                                    'Boise City, Idaho',                                       title),
    title = ifelse(title == 'Ventura, California',                             'San Buenaventura (Ventura), California',                   title),
    title = ifelse(title == 'Paso Robles, California',                         'El Paso de Robles (Paso Robles), California',              title),
    title = ifelse(title == 'Berkeley Springs, West Virginia',                 'Bath (Berkeley Springs), West Virginia',                   title),
    title = ifelse(title == 'Webster Springs, West Virginia',                  'Addison (Webster Springs), West Virginia',                 title),
    title = ifelse(title == 'Reno, Lamar County, Texas',                       'Reno (Lamar County), Texas',                              title),
    title = ifelse(title == 'Mt. Angel, Oregon',                               'Mount Angel, Oregon',                                     title),
    title = ifelse(title == 'Heber City, Utah',                                'Heber, Utah',                                             title),
    title = ifelse(title == 'LaFollette, Tennessee',                           'La Follette, Tennessee',                                  title),
    title = ifelse(title == 'Saint Joseph, Tennessee',                         'St. Joseph, Tennessee',                                   title),
    title = ifelse(title == 'Pleasant Valley, Marion County, West Virginia',   'Pleasant Valley, West Virginia',                          title),
    title = ifelse(title == 'Oakwood, Montgomery County, Ohio',                'Oakwood, Ohio',                                           title),
    title = ifelse(title == 'Herkimer (village), New York',                    'Herkimer, New York',                                      title),
    title = ifelse(title == 'Fulton, Oswego County, New York',                 'Fulton, New York',                                        title),
    title = ifelse(title == 'Middletown, Orange County, New York',             'Middletown, New York',                                    title),
    title = ifelse(title == 'Old Forge, Lackawanna County, Pennsylvania',      'Old Forge, Pennsylvania',                                 title),
    title = ifelse(title == 'Middletown, Dauphin County, Pennsylvania',        'Middletown, Pennsylvania',                                title),
    title = ifelse(title == 'Edgewater, Volusia County, Florida',              'Edgewater, Florida',                                      title),
    title = ifelse(title == 'Midway, Gadsden County, Florida',                 'Midway, Florida',                                         title),
    title = ifelse(title == 'Lake Butler, Union County, Florida',              'Lake Butler, Florida',                                    title),
    title = ifelse(title == 'Swanton (village), Vermont',                      'Swanton, Vermont',                                        title),
    title = ifelse(title == 'Poultney (village), Vermont',                     'Poultney, Vermont',                                       title),
    title = ifelse(title == 'Alburgh (village), Vermont',                      'Alburgh, Vermont',                                        title),
    title = ifelse(title == 'Ludlow (village), Vermont',                       'Ludlow, Vermont',                                         title),
    title = ifelse(title == 'Belle Haven, Accomack County, Virginia',          'Belle Haven, Virginia',                                   title),
    title = ifelse(title == 'Elgin, Kershaw County, South Carolina',           'Elgin, South Carolina',                                   title),
    title = ifelse(title == 'Springdale, Lexington County, South Carolina',    'Springdale, South Carolina',                              title),
    title = ifelse(title == 'Flat Rock, Henderson County, North Carolina',     'Flat Rock, North Carolina',                               title),
    title = ifelse(title == 'Prentiss, Jefferson Davis County, Mississippi',   'Prentiss, Mississippi',                                   title),
    title = ifelse(title == 'Jonestown, Coahoma County, Mississippi',          'Jonestown, Mississippi',                                  title),
    title = ifelse(title == 'Elbow Lake, Grant County, Minnesota',             'Elbow Lake, Minnesota',                                   title),
    title = ifelse(title == 'Shelby, Oceana County, Michigan',                 'Shelby, Michigan',                                        title),
    title = ifelse(title == 'Clinton, Lenawee County, Michigan',               'Clinton, Michigan',                                       title),
    title = ifelse(title == 'Saint Michaels, Maryland',                        'St. Michaels, Maryland',                                  title),
    title = ifelse(title == 'Grosse T\u00eate, Louisiana',                     'Grosse Tete, Louisiana',                                  title),
    title = ifelse(title == 'Oak Grove, West Carroll Parish, Louisiana',       'Oak Grove, Louisiana',                                    title),
    title = ifelse(title == 'Pleasant Hill, Sabine Parish, Louisiana',         'Pleasant Hill, Louisiana',                                title),
    title = ifelse(title == 'LaCenter, Kentucky',                              'La Center, Kentucky',                                     title),
    title = ifelse(title == 'LaSalle, Colorado',                               'La Salle, Colorado',                                      title),
    title = ifelse(title == 'Middlesboro, Kentucky',                           'Middlesborough, Kentucky',                                title),
    title = ifelse(title == 'LaGrange, Indiana',                               'Lagrange, Indiana',                                       title),
    title = ifelse(title == 'Milford, Kosciusko County, Indiana',              'Milford, Indiana',                                        title),
    title = ifelse(title == 'Monroe, Adams County, Indiana',                   'Monroe, Indiana',                                         title),
    title = ifelse(title == 'DePue, Illinois',                                 'De Pue, Illinois',                                        title),
    title = ifelse(title == 'Angels Camp, California',                         'Angels, California',                                      title),
    title = ifelse(title == 'Live Oak, Sutter County, California',             'Live Oak, California',                                    title),
    title = ifelse(title == 'Pine Mountain, Harris County, Georgia',           'Pine Mountain, Georgia',                                  title),
    title = ifelse(title == 'DeValls Bluff, Arkansas',                         'De Valls Bluff, Arkansas',                                title),
    title = ifelse(title == 'Salem, Fulton County, Arkansas',                  'Salem, Arkansas',                                         title),
    title = ifelse(title == 'Southside, Independence County, Arkansas',        'Southside, Arkansas',                                     title),
    title = ifelse(title == 'LaFayette, Alabama',                              'La Fayette, Alabama',                                     title),
    title = ifelse(title == "Jackson's Gap, Alabama",                          "Jacksons' Gap, Alabama",                                  title),
    title = ifelse(title == 'Evergreen, Conecuh County, Alabama',              'Evergreen, Alabama',                                      title),
    title = ifelse(title == 'Lacy Lakeview, Texas',                            'Lacy-Lakeview, Texas',                                    title),
    title = ifelse(title == 'Agua Dulce, El Paso County, Texas',               'Agua Dulce, Texas',                                       title),
    title = ifelse(title == 'Lindsay, Cooke County, Texas',                    'Lindsay, Texas',                                          title),
    title = ifelse(title == 'Palm Valley, Cameron County, Texas',              'Palm Valley, Texas',                                      title),
    title = ifelse(title == 'Pinehurst, Orange County, Texas',                 'Pinehurst, Texas',                                        title),
    title = ifelse(title == 'LaRue, Ohio',                                     'La Rue, Ohio',                                            title),
    title = ifelse(title == 'Bainbridge, Ross County, Ohio',                   'Bainbridge, Ohio',                                        title),
    title = ifelse(title == 'Sherwood, Defiance County, Ohio',                 'Sherwood, Ohio',                                          title),
    title = ifelse(title == 'Shiloh, Richland County, Ohio',                   'Shiloh, Ohio',                                            title),
    title = ifelse(title == 'Dillonvale, Jefferson County, Ohio',              'Dillonvale, Ohio',                                        title),
    title = ifelse(title == 'Oakwood, Paulding County, Ohio',                  'Oakwood (Paulding County), Ohio',                         title),
    title = ifelse(title == 'Philipsburg, Centre County, Pennsylvania',        'Philipsburg, Pennsylvania',                               title),
    title = ifelse(title == 'Nanty Glo, Pennsylvania',                         'Nanty-Glo, Pennsylvania',                                 title),
    title = ifelse(title == 'Coaldale, Schuylkill County, Pennsylvania',       'Coaldale (Schuylkill County), Pennsylvania',              title),
    title = ifelse(title == 'Red Lion, York County, Pennsylvania',             'Red Lion, Pennsylvania',                                  title),
    title = ifelse(title == 'Oakland, Susquehanna County, Pennsylvania',       'Oakland, Pennsylvania',                                   title),
    title = ifelse(title == 'Big Run, Jefferson County, Pennsylvania',         'Big Run, Pennsylvania',                                   title),
    title = ifelse(title == 'Jefferson, York County, Pennsylvania',            'Jefferson (York County), Pennsylvania',                   title),
    title = ifelse(title == 'Pleasantville, Venango County, Pennsylvania',     'Pleasantville (Venango County), Pennsylvania',            title),
    title = ifelse(title == 'Pine Grove, Schuylkill County, Pennsylvania',     'Pine Grove, Pennsylvania',                                title),
    title = ifelse(title == 'Jonestown, Lebanon County, Pennsylvania',         'Jonestown, Pennsylvania',                                 title),
    title = ifelse(title == 'New Bloomfield, Pennsylvania',                    'Bloomfield, Pennsylvania',                                title),
    title = ifelse(title == 'Beavertown, Snyder County, Pennsylvania',         'Beavertown, Pennsylvania',                                title),
    title = ifelse(title == 'Benton, Columbia County, Pennsylvania',           'Benton, Pennsylvania',                                    title),
    title = ifelse(title == 'Brownstown, Cambria County, Pennsylvania',        'Brownstown, Pennsylvania',                                title),
    title = ifelse(title == 'Clinton, Oneida County, New York',                'Clinton, New York',                                       title),
    title = ifelse(title == 'Florida, Orange County, New York',                'Florida, New York',                                       title),
    title = ifelse(title == 'Mohawk, Herkimer County, New York',               'Mohawk, New York',                                        title),
    title = ifelse(title == 'Northville, Fulton County, New York',             'Northville, New York',                                    title),
    title = ifelse(title == 'Unionville, Orange County, New York',             'Unionville, New York',                                    title),
    title = ifelse(title == 'Victory, Saratoga County, New York',              'Victory, New York',                                       title),
    title = ifelse(title == 'Waverly, Tioga County, New York',                 'Waverly, New York',                                       title),
    title = ifelse(title == 'Yorkville, Oneida County, New York',              'Yorkville, New York',                                     title),
    title = ifelse(title == 'Aurora, Cayuga County, New York',                 'Aurora, New York',                                        title),
    title = ifelse(title == 'Woodbury, Orange County, New York',               'Woodbury, New York',                                      title),
    title = ifelse(title == 'Waterloo, New York (village)',                    'Waterloo, New York',                                      title),
    title = ifelse(title == 'Ranson, West Virginia',                           'Ranson corporation, West Virginia',                       title),
    title = ifelse(title == 'Athens, Georgia',                                 'Athens-Clarke County unified government, Georgia',        title),
    title = ifelse(title == 'Cusseta, Georgia',                                'Cusseta-Chattahoochee County, Georgia',                   title),
    title = ifelse(title == 'Bel Air, Harford County, Maryland',               'Bel Air, Maryland',                                       title),
    title = ifelse(title == 'Bessemer, Lawrence County, Pennsylvania',         'Bessemer, Pennsylvania',                                  title),
    title = ifelse(title == 'Shawnee, Perry County, Ohio',                     'Shawnee, Ohio',                                           title),
    title = ifelse(title == 'Dansville, Livingston County, New York',          'Dansville, New York',                                     title),
    title = ifelse(title == 'Pine Level, Johnston County, North Carolina',     'Pine Level, North Carolina',                              title),
    title = ifelse(title == 'Fulton, Oswego County, New York',                 'Fulton, New York',                                        title),
    title = ifelse(title == 'Anaconda, Montana',                               'Anaconda-Deer Lodge County, Montana',                     title),
    title = ifelse(title == 'Butte, Montana',                                  'Butte-Silver Bow, Montana',                               title),
    title = ifelse(title == 'Saint Clair, Missouri',                           'St. Clair, Missouri',                                     title),
    title = ifelse(title == 'DeMotte, Indiana',                                'De Motte, Indiana',                                       title),
    title = ifelse(title == 'Windfall, Indiana',                               'Windfall City, Indiana',                                  title),
    title = ifelse(title == 'Lynchburg, Tennessee',                            'Lynchburg, Moore County, Tennessee',                      title),
    title = ifelse(title == 'Hartsville, Tennessee',                           'Hartsville/Trousdale County, Tennessee',                  title),
    title = ifelse(title == 'Rotterdam (town), New York',                      'Rotterdam, New York',                                     title),
    title = ifelse(title == 'Georgetown, Quitman County, Georgia',             'Georgetown-Quitman County, Georgia',                      title),
    clean_name = normalize_city_name(title)
  ) %>%
  # When multiple Wikipedia entries share a clean_name (county disambiguation
  # variants for the same place), keep the one with the highest 2010 population.
  group_by(clean_name) %>%
  slice_max(y2010, n = 1, with_ties = FALSE) %>%
  ungroup()


# Join Census places with historical populations ------------------------------

# Left-join the Census candidate universe to the cleaned historical data on
# clean_name. Drop rows with no 2010 historical population (i.e., no Wikipedia
# match), as the pre-auto filter relies on it.

joined <- left_join(tiger_places, hist_pops_clean, by = 'clean_name') %>%
  select(city_fips, city_name = clean_name, state, cty_fips:rucc, y1790:y2010, y2020 = pop_20) %>%
  arrange(city_fips) %>%
  filter(!is.na(y2010))


# Diagnostics: unmatched Census places ----------------------------------------

# Any Census place that failed to join will be absent from `joined` (dropped by
# the !is.na(y2010) filter above). The anti_join below surfaces those records so
# they can be inspected and, if warranted, added to the name-correction block.
#
# Two notes on known patterns in the residual:
#
#   Wisconsin villages: The Wikipedia source contains very few pre-WWII entries
#   for Wisconsin villages. Rather than reflecting a data gap, this appears to
#   be a genuine historical pattern — most Wisconsin villages incorporated after
#   1945 and lack the pre-automobile commercial core this method is designed to
#   detect. They are excluded from the diagnostic output to avoid noise.
#
#   CDPs in exception states: A small number of CDPs in the exception-state list
#   still fail to match. Spot-checking these consistently reveals places that
#   did not appear in pre-WWII Census records, suggesting they are post-war
#   settlements that were correctly omitted from the Wikipedia historical series
#   rather than matching failures requiring correction.

nojoin <- anti_join(tiger_places, hist_pops_clean, by = 'clean_name') %>%
  arrange(state, desc(pop_20)) %>%
  filter(
    state %in% cdp_exception_states,
    str_detect(city_name, 'village, Wisconsin', negate = TRUE)
  )

rm(nojoin)


# Filter to pre-automobile-era towns ------------------------------------------

# Criteria (consistent with the Cityscape paper methodology):
#   - Population in at least one decade from 1900–1940 exceeded 750, signaling
#     that the town existed and had some economic density before car-dependent
#     development patterns took hold.
#   - 2020 population between 500 and 150,000 (already applied above, but
#     retained here for clarity and in case the joined table is reused).
#
# This excludes: (1) post-WWII suburbs with no pre-auto core; (2) ghost towns
# too small to have a legible commercial district; (3) large cities where a
# different downtown-delineation approach is warranted.

hist_pop_floor    <- 750
current_pop_floor <- 500
current_pop_ceil  <- 150000

pre_auto_towns <- joined %>%
  filter(
    y1900 >= hist_pop_floor | y1910 >= hist_pop_floor | y1920 >= hist_pop_floor |
      y1930 >= hist_pop_floor | y1940 >= hist_pop_floor,
    y2020 >= current_pop_floor,
    y2020 <= current_pop_ceil
  )


# Attach place polygon geometries ---------------------------------------------

# Download Census cartographic boundary place polygons for each state, join to
# the pre-auto universe on city_fips, and project to Albers Equal Area.
# Year columns are dropped since they are only needed for the filter above.

pre_auto_sf <- map_dfr(
  .x = unique(pre_auto_towns$state),
  .f = ~ places(.x, cb = TRUE) %>%
    select(city_fips = GEOID) %>%
    inner_join(pre_auto_towns, by = 'city_fips')
) %>%
  st_transform(6350) %>%
  select(city_fips, city_name, state, pop_20 = y2020, cty_fips:rucc)

maplibre_view(pre_auto_sf)  # visual sanity check of the full universe


# Export to CSV ---------------------------------------------------------------

# Geometry is serialized as WKT (lon/lat, EPSG:4326) so that the file can be
# read by downstream scripts without an additional spatial library dependency.

pre_auto_sf %>%
  st_transform(4326) %>%
  mutate(geometry_wkt = st_as_text(geometry)) %>%
  st_drop_geometry() %>%
  write_csv('data/national_pre_auto_places.csv')
