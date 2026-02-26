# plotphotos

An R package to copy (not move) JPEG/JPG photos into a flattened output structure by nearest plot `locality_id`.

## What it does

-   Scans one or more input folders recursively.
-   Reads EXIF GPS from JPG/JPEG.
-   Assigns each GPS photo to nearest plot point (from `.gpkg` or `.shp`).
-   Copies into subfolders under `out_root`:
    -   `correct` (within threshold)
    -   `review_far` (over threshold)
    -   `review_multiple` (multiple plots within threshold)
    -   `review_no_gps` (logged only; not copied by default)
    -   `name` (no GPS, but filename already begins with `locality_id`)
    -   `folder` (no GPS, but parent folder name equals `locality_id`; files renamed to `locality_id` pattern)

## Conflict-free naming across multiple runs

The package checks the output folder before copying: - first file uses `ID.jpg` - second becomes `ID_1.jpg` - third becomes `ID_2.jpg` - …

It also reads the existing `rename_log.csv` (if present) and skips sources already copied.

## Example

``` r
library(plotphotos)

copy_photos_to_plots(
  img_roots   = c("X:/MIKROKLIMA/NPCS", "X:/MIKROKLIMA/Cesky_Kras"),
  out_root    = "J:/foto_test",
  plots_path  = "active.gpkg",
  plots_layer = "localities_active",
  id_field    = "locality_id",
  threshold_m = 30,
  names   = TRUE,
  folders = TRUE
)
```

``` bash
```

### Install for colleagues

**remotes (easy)**

``` r
install.packages("remotes")

# If the repo is private, set a token first (read_repository):
# Sys.setenv(GITLAB_PAT = "YOUR_TOKEN_HERE")

remotes::install_gitlab(
  repo = "josef.bruna/locality_photo_rename",
  host = "git.sorbus.ibot.cas.cz",
  upgrade = "never"
)

library(plotphotos)

img_roots <- c(
  "O:/FOTKY/2025-07-17 NPČŠ - snímkování - Macek"
  
)

copy_photos_to_plots(
  img_roots   = img_roots,
  out_root    = "J:/foto_test_package",
  plots_path  = "active.gpkg",
  plots_layer = "localities_active",
  threshold_m = 30,
  far_max_m = 100,  # e.g. only copy GPS photos up to 100 m away
  names   = TRUE,   # copy no-GPS files already named by locality_id -> out_root/name
  folders = TRUE,   # copy no-GPS files in folders named locality_id -> out_root/folder (renamed)
  verbose = TRUE
)
```
