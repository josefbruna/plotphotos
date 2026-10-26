# plotphotos 0.3.0

Organize field photographs and accompanying files by plot locality, then optionally
resize JPEG copies and write capture dates onto them. There are just two public
functions: `copy_photos_to_plots()` and `write_into_image()`.

**Created and maintained by Josef Brůna, with extensive assistance from OpenAI's
ChatGPT in code development, debugging, tests, and documentation.** Josef defines
the requirements and is responsible for reviewing and approving releases and
maintaining the package. ChatGPT is acknowledged as a development tool, not a
package author. The package does not call an AI service or send photographs to one.
Specific model snapshots used throughout development were not recorded.

**Release status:** 0.3.0 is a CRAN-preparation candidate. It has not been submitted
to or accepted by CRAN. The editor used for this revision had no R runtime and
could not download one, so no passing R checks are claimed for this revision.
The included workflow and tests must be run before release/submission.

## Install or update

After this version has been pushed to GitHub, restart R and install it in a fresh
session, before loading an older copy of the package:

```r
# install.packages("remotes")  # once, if needed
remotes::install_github(
  "josefbruna/plotphotos", upgrade = "never", force = TRUE,
  INSTALL_opts = "--html"
)
```

For the downloaded source, unzip it and install the directory containing
`DESCRIPTION` (do not install the parent delivery directory):

```r
remotes::install_local(
  "path/to/plotphotos", upgrade = "never", force = TRUE,
  INSTALL_opts = "--html"
)
```

Restart R once more after installation. Then:

```r
library(plotphotos)
packageVersion("plotphotos")  # should be 0.3.0
help("plotphotos", package = "plotphotos")
help("copy_photos_to_plots", package = "plotphotos")
help("write_into_image", package = "plotphotos")
citation("plotphotos")
```

`--html` requests installation of HTML help pages. There is no separate runtime
requirement for `roxygen2`, `devtools`, or `usethis`.

### Dependencies

GPS sorting uses `sf`, `units`, and `exifr`. GPS extraction also requires a working
ExifTool backend as configured by `exifr`; do not assume that installing R alone
provides it. When metadata extraction fails, the package warns and retains files
using the non-GPS rules. Check the sorting log rather than assuming GPS was used.

Image annotation uses `jpeg`, `magick`, and R graphics. `magick` uses ImageMagick
libraries, but this function does not call an ImageMagick or ExifTool command-line
program. Installing dependencies from source may require system libraries; use
suitable binaries where available and follow the dependency installation messages.

## 1. Copy and sort field files

```r
copy_log <- copy_photos_to_plots(
  img_roots   = c("photos/site_A", "photos/site_B"),
  out_root    = "sorted_photos",
  plots_path  = "plots.gpkg",
  plots_layer = "plots",
  match_field = "locality_id",
  name_field  = "locality_id",
  threshold_m = 30,
  far_max_m   = 100,
  names       = TRUE,
  folders     = TRUE
)
```

The plot layer must contain one POINT geometry per plot, a defined coordinate
reference system, and the requested ID columns. All regular files are considered,
including hidden files, JPEGs without GPS, and non-image files.

| Condition | Default output category | Filename stem |
| --- | --- | --- |
| GPS: exactly one plot within `threshold_m` | `correct` | Plot value from `name_field` |
| GPS: multiple plots within `threshold_m` | `review_multiple` | Candidate plot values, nearest first |
| GPS: nearest plot beyond `threshold_m`, up to `far_max_m` | `review_far` | Nearest plot value |
| GPS: nearest plot beyond `far_max_m` | `review_far` | Original stem |
| No usable GPS: immediate parent folder matches and `folders = TRUE` | `folder` | Matched plot value |
| No usable GPS: filename prefix matches and `names = TRUE` | `name` | Matched plot value |
| No usable GPS and no enabled rule matches | `no_gps` | Original stem |

Folder matching has priority over filename matching. Both ignore case. A filename
prefix must equal the whole stem or be followed by an underscore, space, or
hyphen. The longest matching ID wins. `names = FALSE` and `folders = FALSE` disable
matching rules, not copying. GPS extraction is limited to JPG/JPEG files.

`far_max_m` limits *renaming*, not copying. Use `Inf` to remove that naming limit.
Distances use `sf`'s spherical geometry engine, in metres. This handles locations
across UTM zones and the antimeridian. Distances close to a threshold can differ
slightly from the projected distances used in 0.2.1; the caller's `sf` engine
setting is restored afterwards.

Output files are flat within each category. Categories are created only when
needed. JPEG extensions become `.jpg`; other extensions are preserved. Unsafe
Windows filenames are sanitized and very long stems shortened. Existing names
receive `_1`, `_2`, and subsequent suffixes. Source modification times are
preserved where supported by the filesystem.

### A runnable example with no private data

```r
(function() {
  root <- tempfile("plotphotos-example-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  input <- file.path(root, "input")
  dir.create(file.path(input, "P01"), recursive = TRUE)
  writeLines("Field notes", file.path(input, "P01", "notes.txt"))
  writeLines("Unmatched file", file.path(input, "other.txt"))

  plots <- sf::st_as_sf(
    data.frame(locality_id = "P01", lon = 14.5, lat = 50.0),
    coords = c("lon", "lat"), crs = 4326
  )
  gpkg <- file.path(root, "plots.gpkg")
  sf::st_write(plots, gpkg, layer = "plots", quiet = TRUE)
  log <- copy_photos_to_plots(
    input, file.path(root, "sorted"), gpkg,
    plots_layer = "plots", verbose = FALSE
  )
  print(log[, c("status", "copy_ok")])
})()
```

This example exercises the folder and unmatched rules without requiring an
ExifTool installation. The function help contains the same portable example.

### Sorting log and repeat runs

`rename_log.csv` records source/destination paths, read coordinates, nearest
plot distance, plot values used for naming, routing status, and `copy_ok`.
A routing status alone does not mean copying succeeded: inspect `copy_ok`.
The return value is the cumulative log, including earlier runs.

Successfully copied source paths are skipped while their logged destinations
still exist. Deleted destination files can be restored by rerunning. This is
**not content synchronization**: replacing a source image at the same path is not
automatically detected. Use a fresh output directory and log to reprocess changed
inputs. Keep the existing log with its output tree for ordinary repeat runs.

## 2. Resize JPEG copies and write dates

```r
stamp_log <- write_into_image(
  input_folder  = "sorted_photos",
  output_folder = "dated_photos",
  date_format   = "%Y-%m-%d",
  resize        = TRUE,
  long_side     = 2000,
  quality       = 80
)
```

EXIF orientation is applied first. Larger photographs are reduced using Lanczos
resampling, and black date text with a white outline is drawn in the bottom-right
corner at the final resolution. The image is JPEG-encoded once at the requested
quality; no intermediate JPEG is written.

| Parameter | Default | Meaning |
| --- | --- | --- |
| `resize` | `TRUE` | Reduce images larger than `long_side`. `FALSE` retains dimensions after orientation. |
| `long_side` | `2000` | Maximum longer-side length in pixels; a positive whole number. Smaller images are never enlarged. |
| `quality` | `80` | JPEG encoder setting, as a whole number from 1 to 100. Not a measured percentage of visual quality. |

Aspect ratio is preserved subject to whole-pixel rounding. Even without resizing,
annotated JPEGs are recompressed; keep the original photographs.

```r
write_into_image(
  "sorted_photos", "dated_full_size", date_format = "%d.%m.%Y",
  resize = FALSE, quality = 90
)
```

The first valid date is used from EXIF `DateTimeOriginal`, `DateTimeDigitized`
(`CreateDate`), then `DateTime`. If these are unavailable or invalid, the function
uses file modification time, then file creation/change time as a last fallback.
Without any usable timestamp it writes `unknown date`.

EXIF clock times are used as recorded, without timezone conversion; offset-time
tags are not interpreted. Filesystem times use the current local timezone and
are not guaranteed capture dates. The selected source is logged. Modification
time takes precedence on every platform so a preserved modification time remains
useful in a sort-then-stamp workflow.

Examples of `date_format` are `%Y-%m-%d`, `%d.%m.%Y`, and `%Y-%m-%d %H:%M`.
See `?strftime` for the formatting rules and the function help for a runnable
example that generates a small JPEG in a temporary directory.

### Output, metadata, and failures

Relative subfolders are retained; non-JPEG files are copied unchanged. Existing
output files receive suffixes rather than being overwritten. An existing file
blocking a needed directory causes an alternate directory name to be selected.

The original EXIF APP1 segment is retained where possible, including any GPS
locations it contains; the output is not anonymized. Its orientation is
reset to normal, and existing standard dimension tags are updated. **ICC colour
profiles and XMP are not retained. Embedded EXIF thumbnails are not rebuilt and
may still show the original image.** Exact colour-profile preservation is not
provided. CMYK JPEGs are retained unchanged in the failure category.

JPEGs that cannot be processed are copied unchanged under `unprocessed/`, which
is created only when needed. `processing_log.csv` records the actual destination,
status, stamp text, date source, error, and processing time. The function returns
the current run; the CSV accumulates all runs.

Statuses are `stamped`, `copied` (non-JPEG copied unchanged), `unprocessed`
(failed JPEG whose original was retained), or `failed` (no output copy).
Inspect the log, especially `unprocessed` and `failed` rows.

The root log filename and failure category are reserved. An input file named
`processing_log.csv` receives a suffix. Input directories named
`processing_log.csv` and `unprocessed` become `processing_log_input` and
`unprocessed_input`, respectively. Alternate names may share an existing folder;
file suffixes prevent overwrites and the log records each destination.

**Repeated stamping calls create additional copies.** Use a fresh output folder
when changing annotation, dimensions, or quality settings.

## Shared file-safety rules

Both functions leave input originals unchanged. Input and output roots must
differ; the output cannot contain an input root. A nested output tree is allowed
and excluded before recursive traversal. Symbolic-link cycles are avoided;
linked targets outside the input roots are skipped with a warning. Output links
cannot redirect writes outside the selected output root.

Sorting category directories must be distinct and must not contain one another.
A custom sorting log cannot point into an input tree outside the excluded output.
Each encountered physical file path is processed once per run. Hard links are
not deduplicated by inode. Filesystem permissions and path limits can still
prevent copies; these failures are reported rather than silently counted as
successes.

Logs are UTF-8 CSVs. Existing ASCII/UTF-8 logs from 0.2.x are supported; incompatible
or incorrectly encoded logs are not silently replaced. Convert an older log to
UTF-8 or choose a new log path when required. Log writes are staged, retaining a
previous-log backup until replacement succeeds. A process interruption can leave
a hidden `.plotphotos-log-backup-*.csv` next to the log; retain it for recovery.
**Do not run concurrent writers to the same output tree or log.**

## Help pages showing "Internal Server Error"

This revision ships plain, complete `.Rd` help sources and tests that resolve and
render all three installed help topics. An installed-help server error alone does
not establish whether the cause is the package documentation, an old loaded help
database, another library copy, or the R/RStudio help server. The reported local
error could not be reproduced in the editor's environment because R is absent.

Use the fresh-session installation above; do not overwrite a loaded package and
continue using that same session. Confirm the actual installation:

```r
packageVersion("plotphotos")
find.package("plotphotos")
.libPaths()
help("copy_photos_to_plots", package = "plotphotos", help_type = "text")
help("write_into_image", package = "plotphotos", help_type = "text")
```

If text help works but HTML help still fails after a clean restart, report the
exact console error, `sessionInfo()`, and the library path. Do not assume a
successful source edit alone proves that a local help-server failure is fixed.

## Authorship, licence, and citation

Josef Brůna is the author and maintainer. The existing Institute of Botany
copyright and MIT licence are retained. AI assistance is disclosed prominently
above and in the installed package help.

Recommended package citation:

> Brůna, J. (2026). plotphotos: Organize Field Photographs and Annotate Capture
> Dates. R package version 0.3.0. https://github.com/josefbruna/plotphotos

Separate acknowledgement of the AI development tool:

> OpenAI. (2026). ChatGPT [Large language model]. https://chatgpt.com/
> Used extensively for code development, debugging, tests, and documentation
> under Josef Brůna's direction; specific model snapshots were not recorded.

`citation("plotphotos")` returns these as two clearly distinguished entries,
with the package citation first. It does not list OpenAI as a package co-author.

## Checks before CRAN submission

The `.github/workflows/R-CMD-check.yaml` workflow runs after a GitHub push or pull
request and can also be started manually. It is configured for Windows and macOS
R-release, and Linux R-release, R-devel, and R-oldrel. The Linux release job also
attempts the PDF manual check; all jobs check installed HTML help. None of these
runs has been launched or observed during this revision.

From a terminal in the parent of the package directory, with dependencies and
LaTeX installed for the manual:

```sh
R CMD build plotphotos
R CMD check --as-cran plotphotos_0.3.0.tar.gz
```

The archive produced by `R CMD build` is the CRAN submission source tarball. The
GitHub/source ZIP delivered with this revision is not that built tarball.

For maintainers editing documentation, regenerate both help and namespace files
before rebuilding:

```r
# install.packages("roxygen2")  # development only
roxygen2::roxygenise("path/to/plotphotos")
```

Review every error, warning, and note, and check on current R-release and R-devel.
Run Windows checks, inspect the two help pages in a fresh local R session, and
exercise a small batch of your real photographs before submission. Update
`cran-comments.md` with actual results, verify the maintainer email and copyright,
and obtain the maintainer's approval. Do not claim a successful check from the
mere presence of a workflow or tests.

Development references:

- R Core Team, *Writing R Extensions*: https://cran.r-project.org/doc/manuals/r-release/R-exts.html
- CRAN Repository Policy (verify current requirements before submission): https://cran.r-project.org/web/packages/policies.html
- r-lib Actions: https://github.com/r-lib/actions
- OpenAI, *Sharing & publication policy*: https://openai.com/policies/sharing-publication-policy/

See `NEWS.md` for changes from 0.2.1.
