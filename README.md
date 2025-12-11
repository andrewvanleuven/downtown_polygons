# Downtown polygons for Washington towns

This repo builds approximate downtown/central business district polygons for a universe of Washington towns and small cities, using historical population data and modern points-of-interest (POI) data.

The workflow has three main stages:

1. Construct the **pre-auto town universe** (which cities/towns to include).
2. Intersect that universe with **POI data** from vector tiles.
3. Run a **kernel density–based** algorithm to delineate downtown polygons.

## 1. Data sources

- **Historical town/city populations (1790–2010)**  
  CSV files compiled from Wikipedia for each U.S. state, hosted in the [`Historical-Populations` GitHub project](https://github.com/CreatingData/Historical-Populations) (one CSV per state). These provide long-run population series for cities and towns. 

- **2020 decennial Census place populations and geometries**  
  Retrieved via the [`tidycensus` R package](https://github.com/walkerke/tidycensus) (`get_decennial()` with `geography = "place"`, `variables = "P1_001N"`, `state = "WA"`, `year = 2020`, `geometry = TRUE`). Used to get current population and place centroids for Washington cities and towns. 

- **County Rural–Urban Continuum Codes (RUCC)**  
  2023 [RUCC codes](https://www.ers.usda.gov/data-products/rural-urban-continuum-codes) from the USDA Economic Research Service, used to exclude central metropolitan counties (e.g., the counties containing Seattle and Spokane) so that the analysis focuses on small and mid-sized towns/cities whose downtowns are more comparable to one another.

- **TIGER/Line cartographic boundary files**  
  County and place boundaries for Washington downloaded via the [`tigris` R package](https://github.com/walkerke/tigris) (`counties()` and `places()` with `cb = TRUE`). Used for town polygons and county overlays. 

- **POI vector tiles (Overture/Protomaps)**  
  Places tiles (`places.pmtiles`) served from an S3 bucket and extracted using the [`pmtiles` CLI](https://pmtiles.io/#url=https%3A%2F%2Foverturemaps-tiles-us-west-2-beta.s3.amazonaws.com%2F2025-04-23%2Fplaces.pmtiles&map=6.38/47.293/-121.091) to a WA-only GeoJSONL file. These features are then reprojected and intersected with the pre-auto towns to identify POIs inside each town. 

## 2. Universe definition (“pre-auto” towns)

The pre-auto town universe is constructed in the first script (`01_build_preauto_universe.R`):

- Start with **all Census places in Washington** with 2020 decennial population (`get_decennial()` for `geography = "place"`; `tidycensus`).  
- Join places to the **historical population series** from the Wikipedia-derived CSVs by normalizing place names (e.g., removing “city”, “town”, “village” suffixes) and handling a few ad hoc name fixes (e.g., “Seattle” → “Seattle, Washington”). 
- Attach **RUCC codes** at the county level via place centroids intersected with counties and the USDA RUCC table. 

A place is included in the pre-auto universe if:

- It has population ≥ 700 in at least one pre–World War II decennial year: 1900, 1910, 1920, 1930, or 1940.  
- It is not in the most urban RUCC category (`rucc > 1`).  
- Its 2020 population is between 500 and 150,000.

The output is an `sf` object of Washington places that meet these criteria, saved as a CSV with WKT geometry (e.g., `wa_pre_auto_places.csv`).

## 3. Scripts

### 01_build_preauto_universe.R

- Reads historical population CSVs for Washington from the GitHub Wikipedia-derived dataset.  
- Pulls 2020 Census place populations and geometries with `tidycensus`.  
- Attaches RUCC codes and filters down to “pre-auto” towns using the population and RUCC rules above.  
- Writes `wa_pre_auto_places.csv`, which includes place identifiers, cleaned names, population series, and polygon geometry (WKT) in an appropriate Washington CRS.

### 02_extract_pois.R

- Uses the `pmtiles` CLI to extract Washington-only POIs from a global `places.pmtiles` archive, using the state’s bounding box from `tigris::states()`.   
- Reads the extracted WA GeoJSONL into `sf`, reprojects to a Washington CRS, and intersects with `wa_pre_auto_places` to keep only POIs that fall inside pre-auto towns.  
- Outputs a POI–town intersection file (e.g., `pre_auto_poi.csv` with WKT geometry) for use in the downtown estimation step.

### 03_downtown_kde.R

- Rebuilds `sf` objects from the CSV/WKT outputs:
  - `wa_pre_auto_places.csv` → `pre_auto_sf` (town polygons).
  - `pre_auto_poi.csv` → `wa_dots` (POI points).
- Defines a `downtown_kde()` function that:
  - Selects the largest polygon for a given town (to handle multipart places).  
  - Intersects POI points with that town polygon.  
  - Runs `sfhotspot::hotspot_kde()` on a hex grid and keeps only the high-density hexagons (top quartile via a min–max scaled KDE measure).  
  - Dissolves selected hexagons into contiguous blobs, computes blob-level metrics (mean KDE z-score, hex count), and picks the “best” blob by a composite score.  
  - Buffers and smooths that blob to produce a final downtown polygon.

- Loops over all `city_fips` in `pre_auto_sf`, applies `downtown_kde()` with error handling, and writes a combined GeoJSON (e.g., `wa_downtowns.geojson`) with a downtown polygon for each town.

## 4. Example: Ritzville, WA

The code below shows how to generate and visualize the downtown polygon for Ritzville, using `mapgl` and MapLibre:

