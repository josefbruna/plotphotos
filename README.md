# plotphotos 0.2.0

Sort files by plot locality and write capture dates onto JPEG copies. The package
has two public functions:

- `copy_photos_to_plots()` copies and sorts files using JPEG GPS coordinates,
  plot IDs in parent folder names, or plot IDs in filenames.
- `write_into_image()` writes a date in the bottom-right corner of JPEG copies,
  keeping the input folder structure.

Both functions keep the input originals and avoid overwriting existing output files.

## Install or update

```r
# Install remotes first if needed:
# install.packages("remotes")
remotes::install_github("josefbruna/plotphotos", upgrade = "never", force = TRUE)
library(plotphotos)
packageVersion("plotphotos") # 0.2.0 after this version is on GitHub
```

The sorting function uses `sf`, `units`, and `exifr` (ExifTool through `exifr`).
The new date-writing function uses `jpeg` and R's graphics devices; it does not
call ExifTool or ImageMagick.

## 1. Copy and sort files

`copy_photos_to_plots()` scans one or more input folders recursively. It copies
all regular files, including JPEGs without GPS and non-JPEG files. JPEG GPS
coordinates are used when available; other files use the same folder/name
matching rules as JPEGs without GPS.

The output is flat within each category. Category folders are created only when
a file is copied into them.

| Condition | Default output folder | Output filename |
| --- | --- | --- |
| GPS: one plot within `threshold_m` | `correct` | Plot value from `name_field` |
| GPS: multiple plots within `threshold_m` | `review_multiple` | Candidate plot values, nearest first |
| GPS: nearest plot beyond `threshold_m`, up to `far_max_m` | `review_far` | Nearest plot value |
| GPS: nearest plot beyond `far_max_m` | `review_far` | Original filename |
| No usable GPS: parent folder matches `match_field`, with `folders = TRUE` | `folder` | Matched plot value from `name_field` |
| No usable GPS: filename starts with `match_field`, with `names = TRUE` | `name` | Matched plot value from `name_field` |
| No usable GPS and no enabled folder/name rule matches | `no_gps` | Original filename |

Folder matching takes priority over filename matching. Both are case-insensitive.
A filename prefix must be the whole filename stem or be followed by an underscore,
space, or hyphen. When IDs overlap, the longest matching prefix wins.

`names = FALSE` or `folders = FALSE` disables that matching rule; it does not
prevent unmatched files from being copied. Missing or unusable EXIF GPS likewise
does not cause a file to be omitted. GPS is read only from JPG/JPEG files.

`far_max_m` is now the upper limit for naming a distant photo after its nearest
plot. Photos beyond it are copied to `review_far` with their original filename
and logged as `gps_over_max_distance`. Use `far_max_m = Inf` to name every distant
GPS photo after its nearest plot.

The function uses `ID.jpg`, then `ID_1.jpg`, `ID_2.jpg`, and so on when a name is
already taken. JPEG extensions are normalized to `.jpg`; other file types are
retained. Successful copies preserve file modification times where supported.

### Example

The Geoecology Department plot file is available at
`\\ibot.cas.cz\public\Freenas\x_gis_share\MIKROKLIMA\active.gpkg`,
with layer `localities_active` and field `locality_id`.

```r
library(plotphotos)

plots_path <- "\\\\ibot.cas.cz\\public\\Freenas\\x_gis_share\\MIKROKLIMA\\active.gpkg"
img_roots <- c(
  "X:/MIKROKLIMA/NPCS",
  "X:/MIKROKLIMA/Cesky_Kras"
)

copy_log <- copy_photos_to_plots(
  img_roots   = img_roots,
  out_root    = "plotphotos_output",
  plots_path  = plots_path,
  plots_layer = "localities_active",
  match_field = "locality_id",
  name_field  = "locality_id",
  threshold_m = 30,
  far_max_m   = 100,
  names       = TRUE,
  folders     = TRUE,
  verbose     = TRUE
)
```

### Logs and repeated runs

`rename_log.csv` records the source, destination, coordinates, distance, matching
status, and copy result. The function appends new results and invisibly returns
the cumulative log. Sources already recorded as successfully copied are skipped
while their destination still exists. Previously skipped files from version
0.1.4 remain eligible and will now be copied.

Keep the log with its output folders for this repeat-run behavior. Input and
output must be different folders. An output folder nested under an input folder
is supported and excluded from scanning.

## 2. Write dates into JPEG copies

```r
stamp_log <- write_into_image(
  input_folder  = "plotphotos_output",
  output_folder = "plotphotos_dated",
  date_format   = "%Y-%m-%d"
)
```

The function scans recursively, writes black date text with a white outline in
the bottom-right corner, and saves JPEG copies at quality 75. Relative subfolders
are retained, so `correct/ABC.jpg` becomes `correct/ABC.jpg` under the new output
folder. Other file formats are copied unchanged.

The date is selected in this order:

1. EXIF `DateTimeOriginal`.
2. EXIF `DateTimeDigitized` (also called `CreateDate`).
3. EXIF `DateTime`.
4. File modification time, then file creation/change time only if modification time is unavailable.
5. `unknown date` if no usable timestamp is available.

Missing or invalid EXIF dates fall through to the next source. A filesystem
timestamp is a fallback and may differ from the actual capture date; the source
used is recorded in the log. Modification time takes priority on every platform,
because copying a file on Windows gives it a new creation time. The sorting step
preserves modification time. EXIF dates are used as recorded, without timezone conversion.

`date_format` uses R's `strftime()` formatting, for example:

| Format | Example stamp |
| --- | --- |
| `"%Y-%m-%d"` | `2026-10-26` |
| `"%d.%m.%Y"` | `26.10.2026` |
| `"%Y-%m-%d %H:%M"` | `2026-10-26 14:30` |

EXIF orientation is applied to the output pixels and reset to normal to avoid
double rotation. The original EXIF APP1 segment is retained where possible.
Other metadata blocks, embedded thumbnails, and image-dimension tags are not
regenerated. JPEG output is recompressed, so input originals should be retained.

JPEGs that cannot be stamped, including unsupported CMYK images, are copied
unchanged to `unprocessed/` under the output folder, preserving their relative
subfolders. This folder is created only when needed. No input files are moved.

`processing_log.csv` records the source and destination, status, stamp text, date
source, and any error. The CSV accumulates results; the function invisibly returns
the log for the current run. Existing
output filenames receive a numeric suffix instead of being overwritten. A required
output subfolder also receives a suffix if an existing file blocks its name. Calling
the function again creates additional copies. Use a fresh output folder when
changing the date format. The input and output folders must differ; a nested
output folder is excluded from scanning. The output folder cannot contain the input folder.

For the full argument and return-value details, use
`?copy_photos_to_plots` and `?write_into_image`.

## Changes in 0.2.0

- Copy unmatched files without GPS, files beyond `far_max_m`, and non-JPEG files.
- Create category folders only when used.
- Keep files in the processing list even when EXIF results are missing or incomplete.
- Add `write_into_image(input_folder, output_folder, date_format)`.
- Add `jpeg` as a dependency and update function help and examples.
- Replace the placeholder test with regression coverage for sorting and date writing.
