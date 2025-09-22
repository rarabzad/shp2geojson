# GeoJSON converter — Raven shapefile → GeoJSON

---

## Overview

This repository contains a robust R function and a Shiny UI that converts watershed shapefiles (ESRI `.shp`) into Raven-compatible GeoJSON files. The converter:

* Reads an input shapefile (via `sf`)
* Reads a Raven `.rvh` file (via `RavenR::rvn_rvh_read`)
* Matches subbasin IDs (with fuzzy matching fallback)
* Copies selected RVH attributes into the shapefile attribute table
* Computes or applies outlet coordinates (from CSV input or from RVH HRU table)
* Optionally simplifies geometry
* Writes a validated GeoJSON file

The Shiny app provides a friendly UI for uploading files, selecting matching attributes, previewing data, plotting the basins, and downloading the resulting GeoJSON.

---

## Key changes (since older README)

* The converter is now **sf-based**, returns a structured **list** (not a plain success string).
* The converter writes a `.geojson` file using `sf::st_write()` (avoids `.htm` download fallback).
* The Shiny app now:

  * Suggests and **assigns** a CRS (EPSG or PROJ string) only when the shapefile CRS is missing,
  * Shows the full PROJ string in the CRS text box (only if the CRS was missing),
  * Uses the converter return value to enable download only after the actual file is written,
  * Uses `sf`, `rmapshaper` and `shinycssloaders` for a modern, robust UI/UX.

---

## Live App

Open the app in your browser:

[https://raven-shapefile2geojson.share.connect.posit.cloud](https://raven-shapefile2geojson.share.connect.posit.cloud)

---

## Function: `rvn_rvh_shp_geojson()`

**Signature (updated)**

```r
rvn_rvh_shp_geojson(
  shpfile,
  rvhfile,
  outputfile = file.path(getwd(), "output.geojson"),
  CRSshp = NA,
  matchingColumns = list(shpfile = "subid", rvhfile = "SBID"),
  outletCoords = list(ID = NA, outletLat = NA, outletLng = NA),
  simplifyGeometry = TRUE,
  missing_value = -9999,
  overwrite = TRUE
)
```

**Return value**

A `list` with elements:

* `success` — `TRUE`/`FALSE` (logical) whether write succeeded
* `file` — absolute path to the written GeoJSON (or `NULL`)
* `sf` — the `sf` object that was written (useful for inspection)
* `message` — human-friendly string

**Important behavior**

* If the input shapefile has **no CRS** and you do **not** supply `CRSshp`, the function will `stop()` and ask for a CRS. (The Shiny app supplies a suggested CRS automatically and sets it on the shapefile when missing.)
* The function ensures the output filename ends with `.geojson`.
* Numeric `CRSshp` (e.g., `26960`) is accepted (treated as EPSG). You may also provide a PROJ string.
* `simplifyGeometry = TRUE` calls `rmapshaper::ms_simplify(..., keep = 0.05)` by default; you may pass a numeric keep fraction (e.g., `0.1`).

---

## Quick script example

```r
library(RavenR) # for rvn_rvh_read used by the converter
# source the converter or install the package
source("rvn_rvh_shp_geojson.R")

shpfile  <- "test_cases/Liard/subbasin_20180718.shp"
rvhfile  <- "test_cases/Liard/Liard.rvh"
outgeo   <- file.path(getwd(), "Liard.geojson")

res <- rvn_rvh_shp_geojson(
  shpfile = shpfile,
  rvhfile = rvhfile,
  outputfile = outgeo,
  CRSshp = NA,  # or "EPSG:4326" / 4326 / PROJ string if shapefile has no CRS
  matchingColumns = list(shpfile = "Sub_B", rvhfile = "Name"),
  simplifyGeometry = TRUE,
  overwrite = TRUE
)

if (res$success) {
  message("Wrote GeoJSON to: ", res$file)
} else {
  stop("Converter failed: ", res$message)
}
```

---

## Shiny app — usage notes

1. Open the app URL (see above).
2. Upload:

   * a Raven `.rvh` file,
   * a ZIP containing the shapefile components (`.shp`, `.shx`, `.dbf`, `.prj` if available),
   * optionally, a CSV with outlet coordinates (no header, columns: `ID, Lon, Lat`). The CSV must have NO HEADER and three columns: Subbasin ID, Longitude, Latitude.
3. If the shapefile **does not** include a CRS, the app will automatically:

   * compute a suggested CRS from the extent (WGS84 if coordinates look like lon/lat; otherwise a NAD83 UTM zone),
   * **assign that CRS to the shapefile**, and
   * **fill the CRS text box** with the PROJ4 string (only if the CRS text box is empty).
4. Choose the matching columns (the app will suggest likely columns). Matching uses exact or case-insensitive matches and a fuzzy fallback if necessary.
5. Click **Plot Map** to preview basins and outlets (app transforms to WGS84 for display).
6. Click **Convert to GeoJSON**. When the converter finishes successfully:

   * the app stores the exact path returned by the converter,
   * the **Download GeoJSON** button is enabled,
   * clicking Download delivers the exact file produced by the converter (no `.htm` fallback).
7. If conversion fails, an error message is shown in the UI.

---

## App ↔ Converter integration details (developer notes)

* The app unzips the uploaded shapefile into a temporary `exdir` and the converter writes the GeoJSON into the same folder by default. The Shiny app then reads `res$file` to present the file to the user for download.
* The Shiny `downloadHandler` copies the file returned by the converter (`res$file`). If the converter reports `success = TRUE` and `file` exists, download is allowed.
* The app disables the Download button until `res$success == TRUE` and the file exists. This prevents the browser from obtaining an HTML fallback page (the `.htm` issue).

---

## Troubleshooting

### World map with no layers shown when plotting

* Cause: `st_transform()` was called on an `sf` object with no CRS set.
* Fix: Either ensure the shapefile has a valid CRS (preferred) or supply a valid `CRSshp` (EPSG integer or PROJ string). The Shiny app will suggest & assign a CRS automatically *only if the shapefile CRS is missing*.

### `GDAL Error 1: PROJ: proj_create: Error 1027: utm: Invalid value for zone`

* Cause: Trying to construct a UTM PROJ string with an invalid zone (outside `1..60`) or with coordinates in a projected CRS that are not longitudes.
* Fix: Prefer using EPSG codes (e.g., `26900 + zone` for NAD83 UTM) when assigning CRSs. The converter uses EPSG numbers for UTM zones to avoid this error.

### Download returns `.htm`

* Cause: Shiny may generate a fallback HTML file when the source file path is missing or invalid at download time.
* Fix: The converter now returns `res$file` and success status. The app uses that returned path and only enables the download button when the file exists — this eliminates the `.htm` fallback.

---

## Dependencies

Make sure the following packages are installed:

* R (>= 4.x)
* sf
* RavenR
* rmapshaper
* stringdist
* dplyr
* geojsonio (only if you use the older writer; new converter uses `sf::st_write`)
* shiny, shinythemes, shinyWidgets, shinyjs, shinycssloaders (for the app)

Install (example):

```r
install.packages(c("sf","rmapshaper","stringdist","dplyr","shiny","shinythemes","shinyWidgets","shinyjs","shinycssloaders","RavenR"))

```

---

## References

* Chamberlain, S., & Teucher, A. (2018). geojsonio: Convert Data from and to ‘GeoJSON’. CRAN.
* Chlumsky, R., Craig, J. R., Lin, S. G., Grass, S., Scantlebury, L., Brown, G., & Arabzadeh, R. (2021). RavenR.
* Craig, J. R., Brown, G., Chlumsky, R., Jenkinson, R. W., Jost, G., Lee, K., … & Tolson, B. A. (2020). Flexible watershed simulation with the Raven hydrological modelling framework.

---

## License

GPL-3.0

