#' Copy and sort field files by GPS, plot identifier, or folder
#'
#' Copy files into plot-based categories without changing the originals. JPEG GPS coordinates are used when available. Files without usable GPS, including non-image files, are retained using folder matching, filename matching, or an unmatched category.
#'
#' @param img_roots Character vector of existing input root folders. Files are scanned recursively, including hidden files and folders.
#' @param out_root A single output folder. It cannot equal or contain an input root. An output folder inside an input root is allowed and is excluded from scanning.
#' @param plots_path Path to a GeoPackage or Shapefile containing one POINT geometry per plot and a defined coordinate reference system.
#' @param plots_layer Layer name for a GeoPackage. May be \code{NULL} only when it has exactly one layer. Ignored for other supported vector files.
#' @param match_field Column used to match immediate parent folders or filename prefixes. Matching ignores case. Missing or empty values do not match.
#' @param name_field Column used for output filename stems. Defaults to \code{match_field}. Empty values fall back to the corresponding match value; if both are missing, the original filename is retained.
#' @param threshold_m Finite, non-negative distance in metres. A GPS photograph is within a plot threshold when its distance is less than or equal to this value.
#' @param far_max_m Maximum distance in metres for renaming a distant photograph after its nearest plot. Must be at least \code{threshold_m}; \code{Inf} removes this naming limit. More distant files are still copied to \code{sub_far}, with their original filename stem.
#' @param names A single logical value. Enable filename-prefix matching when usable JPEG GPS is unavailable.
#' @param folders A single logical value. Enable immediate-parent-folder matching when usable JPEG GPS is unavailable. Folder matches take priority over filename matches.
#' @param sub_correct Relative category directory for exactly one plot within the distance threshold.
#' @param sub_far Relative category directory for photographs beyond the distance threshold, including those beyond \code{far_max_m}.
#' @param sub_multiple Relative category directory for multiple plots within the distance threshold.
#' @param sub_no_gps Relative category directory for files without usable GPS and without an enabled folder or filename match.
#' @param sub_named Relative category directory for filename matches.
#' @param sub_folder Relative category directory for parent-folder matches.
#' @param write_log_csv CSV log path. By default, \code{rename_log.csv} under \code{out_root}. It must not be inside an input root unless it is also inside the excluded output tree. Existing logs must be compatible CSV files; they are not blindly overwritten.
#' @param verbose A single logical value. Print progress and a summary when \code{TRUE}.
#'
#' @details
#' Each regular input file is assigned to one category. Only JPG/JPEG files
#' are inspected for GPS metadata, through \pkg{exifr}. A missing ExifTool backend,
#' unreadable metadata, absent GPS fields, or invalid coordinates does not discard
#' files: folder, filename, and no-GPS rules are used instead. Metadata-read errors
#' produce a warning so that a missing GPS backend cannot silently look like a
#' successful spatial assignment.
#'
#' The assignment order is:
#' \enumerate{
#' \item Valid JPEG GPS: choose the nearest plot and count plots within
#' \code{threshold_m}. Use \code{sub_multiple} for multiple candidates,
#' \code{sub_correct} for one candidate, and \code{sub_far} otherwise.
#' \item Without usable GPS, try an exact immediate-parent-folder match when
#' \code{folders = TRUE}.
#' \item Otherwise, try a filename-prefix match when \code{names = TRUE}.
#' An ID must end the filename stem or be followed by an underscore, space, or
#' hyphen. The longest matching ID wins.
#' \item Otherwise, copy to \code{sub_no_gps}.
#' }
#'
#' Distances are computed in metres on longitude/latitude coordinates using
#' \pkg{sf}'s spherical geometry engine. This avoids relying on one UTM zone.
#' The caller's spherical-geometry setting is restored on return or error.
#' Duplicate matching IDs, ignoring case, generate a warning; folder/name lookup
#' uses the first occurrence. Empty point geometries cannot be GPS candidates.
#'
#' Within each category, files are flattened into one directory. Multiple candidate
#' plot names are joined in nearest-first order. Unsafe filename characters and
#' Windows reserved names are sanitized; very long stems are shortened. Numeric
#' suffixes avoid existing filenames. JPEG extensions become \code{.jpg}; other
#' extensions are retained. Modification times are preserved where supported.
#'
#' @section File safety and repeated runs:
#' Input files are never moved or overwritten. Category directories are created
#' only when a copy is attempted and are removed if newly created, empty, and the
#' copy fails. Category paths must be distinct, non-overlapping, and below the
#' output root. The output tree is pruned before input traversal. Symbolic-link
#' cycles are avoided; linked targets outside all input roots are skipped with a
#' warning. Each encountered physical file path is processed once per run.
#'
#' A previously successful source path is skipped only while its logged destination
#' still exists. This is path-based bookkeeping, not content synchronization:
#' changed source contents at the same path are not automatically detected. Use a
#' fresh output directory and log to reprocess replaced inputs. Previously failed
#' copies can be retried. Existing ASCII/UTF-8 logs from version 0.2.x remain usable.
#'
#' Logs are written to a temporary file before replacement, with the previous log
#' retained until replacement succeeds. An interrupted replacement may leave a
#' hidden backup CSV alongside the log. Do not run concurrent writers against the
#' same output directory or log. I/O failures are reported rather than treated as
#' successful copies; an input file cannot be copied when the operating system
#' refuses access.
#'
#' @return Invisibly returns the cumulative log data frame, including previous runs.
#' Its columns are:
#' \describe{
#' \item{source_file}{Input file path.}
#' \item{new_file}{Destination path, or \code{NA} on copy failure.}
#' \item{lon, lat}{Read JPEG GPS coordinates, or \code{NA} when absent. Invalid
#' values may remain in the log but are not used for spatial assignment.}
#' \item{min_dist_m}{Nearest-plot distance, or \code{NA} without usable GPS.}
#' \item{ids_used}{Plot values used for renaming, or \code{NA} when keeping the original name.}
#' \item{status}{One of \code{correct_within_threshold},
#' \code{multiple_within_threshold}, \code{far_over_threshold},
#' \code{gps_over_max_distance}, \code{no_gps_folder}, \code{no_gps_named},
#' or \code{no_gps}. This describes routing, not whether copying succeeded.}
#' \item{copy_ok}{Logical copy-success flag.}
#' }
#' When there is no new work, the existing log, or a zero-row data frame with the
#' same columns, is returned without creating output directories.
#'
#' @seealso \code{\link{write_into_image}}, \code{\link{plotphotos-package}}
#'
#' @examples
#' (function() {
#'   root <- tempfile("plotphotos-example-")
#'   dir.create(root)
#'   on.exit(unlink(root, recursive = TRUE))
#'   input <- file.path(root, "input")
#'   dir.create(file.path(input, "P01"), recursive = TRUE)
#'   # Non-image files also demonstrate routing without an ExifTool installation.
#'   writeLines("Field notes", file.path(input, "P01", "notes.txt"))
#'   writeLines("Unmatched file", file.path(input, "other.txt"))
#'   plots <- sf::st_as_sf(
#'     data.frame(locality_id = "P01", lon = 14.5, lat = 50.0),
#'     coords = c("lon", "lat"), crs = 4326
#'   )
#'   gpkg <- file.path(root, "plots.gpkg")
#'   sf::st_write(plots, gpkg, layer = "plots", quiet = TRUE)
#'   log <- copy_photos_to_plots(
#'     input, file.path(root, "sorted"), gpkg,
#'     plots_layer = "plots", verbose = FALSE
#'   )
#'   log[, c("status", "copy_ok")]
#' })()
#' @encoding UTF-8
#' @export
copy_photos_to_plots <- function(
    img_roots,
    out_root,
    plots_path,
    plots_layer = NULL,
    match_field = "locality_id",
    name_field = match_field,
    threshold_m = 30,
    far_max_m = 100,
    names = TRUE,
    folders = TRUE,
    sub_correct = "correct",
    sub_far = "review_far",
    sub_multiple = "review_multiple",
    sub_no_gps = "no_gps",
    sub_named = "name",
    sub_folder = "folder",
    write_log_csv = file.path(out_root, "rename_log.csv"),
    verbose = TRUE
) {
  scalar_text <- function(x) is.character(x) && length(x) == 1L &&
    !is.na(x) && nzchar(x)
  if (!is.character(img_roots) || !length(img_roots) ||
      anyNA(img_roots) || any(!nzchar(img_roots))) {
    stop("img_roots must contain non-empty folder paths.", call. = FALSE)
  }
  text_args <- list(out_root, plots_path, match_field, name_field, write_log_csv)
  if (!all(vapply(text_args, scalar_text, logical(1)))) {
    stop("Output, plot, log and field arguments must be non-empty strings.", call. = FALSE)
  }
  if (!is.null(plots_layer) && !scalar_text(plots_layer)) {
    stop("plots_layer must be NULL or a non-empty string.", call. = FALSE)
  }
  if (!is.numeric(threshold_m) || length(threshold_m) != 1L ||
      !is.finite(threshold_m) || threshold_m < 0 ||
      !is.numeric(far_max_m) || length(far_max_m) != 1L ||
      is.na(far_max_m) || far_max_m < threshold_m) {
    stop("Require 0 <= threshold_m <= far_max_m; only far_max_m may be Inf.", call. = FALSE)
  }
  if (!all(vapply(list(names, folders, verbose), function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }, logical(1)))) stop("names, folders and verbose must be TRUE or FALSE.", call. = FALSE)

  missing_roots <- img_roots[!dir.exists(img_roots)]
  if (length(missing_roots)) warning("Input folders not found: ",
                                    paste(missing_roots, collapse = ", "), call. = FALSE)
  img_roots <- unique(canonical_path(img_roots[dir.exists(img_roots)]))
  if (!length(img_roots)) stop("No existing img_roots.", call. = FALSE)
  out_root <- canonical_path(out_root)
  write_log_csv <- canonical_path(write_log_csv)
  if (!path_within(write_log_csv, out_root) &&
      any(vapply(img_roots, function(root) path_within(write_log_csv, root), logical(1)))) {
    stop("write_log_csv must not be inside an input root outside out_root.", call. = FALSE)
  }
  if (file.exists(out_root) && !dir.exists(out_root)) {
    stop("out_root is an existing file: ", out_root, call. = FALSE)
  }
  if (any(path_within(img_roots, out_root))) {
    stop("Input roots cannot equal or be inside out_root.", call. = FALSE)
  }
  subfolders <- c(sub_correct, sub_far, sub_multiple, sub_no_gps, sub_named, sub_folder)
  if (length(subfolders) != 6L || anyNA(subfolders) || any(!nzchar(subfolders)) ||
      !all(vapply(list(sub_correct, sub_far, sub_multiple, sub_no_gps,
                      sub_named, sub_folder), scalar_text, logical(1)))) {
    stop("Category subfolders must be non-empty strings.", call. = FALSE)
  }
  dest_dirs <- canonical_path(file.path(out_root, subfolders))
  if (any(!path_within(dest_dirs, out_root)) || any(path_key(dest_dirs) == path_key(out_root))) {
    stop("Category subfolders must stay below out_root.", call. = FALSE)
  }
  if (anyDuplicated(path_key(dest_dirs)) || any(vapply(seq_along(dest_dirs), function(i) {
    any(path_within(dest_dirs[-i], dest_dirs[i]))
  }, logical(1)))) {
    stop("Category subfolders must be distinct and must not contain one another.", call. = FALSE)
  }
  if (path_key(write_log_csv) %in% path_key(dest_dirs)) {
    stop("write_log_csv must not be a category directory.", call. = FALSE)
  }
  base::names(dest_dirs) <- c("correct", "far", "multiple", "no_gps", "name", "folder")

  empty_log <- data.frame(source_file = character(), new_file = character(),
                          lon = numeric(), lat = numeric(), min_dist_m = numeric(),
                          ids_used = character(), status = character(), copy_ok = logical(),
                          stringsAsFactors = FALSE)
  old <- empty_log
  if (file.exists(write_log_csv)) {
    old <- tryCatch(read_processing_log(write_log_csv),
                    error = function(e) stop("Cannot read existing log: ", conditionMessage(e),
                                              call. = FALSE))
    if (!all(base::names(empty_log) %in% base::names(old))) {
      stop("Existing log does not have the expected columns; choose another log path.", call. = FALSE)
    }
    old <- old[, base::names(empty_log), drop = FALSE]
  }
  done <- !is.na(old$copy_ok) & old$copy_ok & !is.na(old$new_file) &
    nzchar(old$new_file) & file.exists(old$new_file)
  already_done <- path_key(canonical_path(old$source_file[done & !is.na(old$source_file)]))

  files <- scan_input_files(img_roots, exclude_trees = out_root,
                            exclude_files = write_log_csv)
  files <- files[!path_key(canonical_path(files)) %in% already_done]
  if (!length(files)) {
    if (verbose) message("Nothing to do: no new input files.")
    return(invisible(old))
  }
  if (!file.exists(plots_path)) stop("Plots file not found: ", plots_path, call. = FALSE)
  is_gpkg <- grepl("\\.gpkg$", plots_path, ignore.case = TRUE)
  if (is_gpkg) {
    layers <- sf::st_layers(plots_path)$name
    if (!length(layers)) stop("No layers found in GeoPackage.", call. = FALSE)
    if (is.null(plots_layer)) {
      if (length(layers) != 1L) stop("GeoPackage has multiple layers; provide plots_layer.", call. = FALSE)
      plots_layer <- layers[1L]
    }
    plots <- sf::st_read(plots_path, layer = plots_layer, quiet = TRUE)
  } else {
    plots <- sf::st_read(plots_path, quiet = TRUE)
  }
  for (field in unique(c(match_field, name_field))) {
    if (!field %in% base::names(plots)) stop("Field not found in plots: ", field, call. = FALSE)
  }
  if (!nrow(plots)) stop("Plots data is empty.", call. = FALSE)
  if (is.na(sf::st_crs(plots))) stop("Plots data has no CRS defined.", call. = FALSE)
  if (any(as.character(sf::st_geometry_type(plots)) != "POINT")) {
    stop("Plots must have POINT geometries, one point per plot.", call. = FALSE)
  }

  # Keep the filesystem inventory authoritative, even when EXIF returns no row.
  dec <- data.frame(SourceFile = sort(files), GPSLatitude = NA_real_, GPSLongitude = NA_real_,
                    status = "no_gps", min_dist_m = NA_real_, base_name = NA_character_,
                    dest_dir = unname(dest_dirs["no_gps"]), keep_original_name = TRUE,
                    stringsAsFactors = FALSE)
  is_jpeg <- grepl("\\.(jpe?g)$", dec$SourceFile, ignore.case = TRUE)
  if (any(is_jpeg)) {
    ex <- tryCatch(exifr::read_exif(dec$SourceFile[is_jpeg],
                                   tags = c("SourceFile", "GPSLatitude", "GPSLongitude")),
                   error = function(e) {
                     warning("EXIF could not be read; copying using folder/name/no-GPS rules: ",
                             conditionMessage(e), call. = FALSE)
                     NULL
                   })
    if (!is.null(ex) && nrow(ex) > 0L && "SourceFile" %in% base::names(ex)) {
      src <- as.character(ex$SourceFile)
      valid <- !is.na(src) & nzchar(src)
      ex <- ex[valid, , drop = FALSE]
      idx <- match(path_key(canonical_path(dec$SourceFile)), path_key(canonical_path(src[valid])))
      for (tag in c("GPSLatitude", "GPSLongitude")) {
        if (tag %in% base::names(ex)) {
          dec[[tag]] <- suppressWarnings(as.numeric(as.character(ex[[tag]][idx])))
        }
      }
    }
  }
  has_gps <- is_jpeg & is.finite(dec$GPSLatitude) & abs(dec$GPSLatitude) <= 90 &
    is.finite(dec$GPSLongitude) & abs(dec$GPSLongitude) <= 180

  match_vals <- as.character(plots[[match_field]])
  plot_names <- as.character(plots[[name_field]])
  missing_name <- is.na(plot_names) | !nzchar(plot_names)
  plot_names[missing_name] <- match_vals[missing_name]
  valid_ids <- !is.na(match_vals) & nzchar(match_vals)
  lookup <- unique(data.frame(id = match_vals[valid_ids], name = plot_names[valid_ids],
                              stringsAsFactors = FALSE))
  keys <- tolower(lookup$id)
  if (anyDuplicated(keys)) warning("Duplicate match_field values ignoring case; using the first name.",
                                    call. = FALSE)
  lookup <- lookup[!duplicated(keys), , drop = FALSE]
  keys <- tolower(lookup$id)
  match_order <- lookup$id[order(nchar(lookup$id), decreasing = TRUE)]

  no_gps <- which(!has_gps)
  if (length(no_gps) && nrow(lookup)) {
    hit <- rep(NA_integer_, length(no_gps))
    if (folders) {
      hit <- match(tolower(basename(dirname(dec$SourceFile[no_gps]))), keys)
      found <- !is.na(hit)
      rows <- no_gps[found]
      dec$status[rows] <- "no_gps_folder"
      dec$dest_dir[rows] <- dest_dirs["folder"]
      dec$base_name[rows] <- lookup$name[hit[found]]
      dec$keep_original_name[rows] <- FALSE
    }
    remaining <- no_gps[is.na(hit)]
    if (names && length(remaining)) {
      stems <- file_stem(dec$SourceFile[remaining])
      ids <- vapply(stems, match_locality_prefix, character(1), ids = match_order)
      matched <- match(tolower(ids), keys)
      found <- !is.na(matched)
      rows <- remaining[found]
      dec$status[rows] <- "no_gps_named"
      dec$dest_dir[rows] <- dest_dirs["name"]
      dec$base_name[rows] <- lookup$name[matched[found]]
      dec$keep_original_name[rows] <- FALSE
    }
  }

  gps <- which(has_gps)
  if (length(gps)) {
    usable <- !sf::st_is_empty(plots)
    if (!any(usable)) stop("Plots data has no non-empty geometries.", call. = FALSE)
    plots_gps <- plots[usable, , drop = FALSE]
    gps_names <- plot_names[usable]
    # Spherical distances avoid UTM-zone, antimeridian and polar errors.
    # Restore the caller's sf option on normal return or any error.
    old_s2 <- sf::sf_use_s2()
    if (!old_s2) {
      suppressMessages(sf::sf_use_s2(TRUE))
      on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
    }
    plots_ll <- sf::st_transform(plots_gps, 4326)
    points <- sf::st_as_sf(data.frame(lon = dec$GPSLongitude[gps], lat = dec$GPSLatitude[gps]),
                           coords = c("lon", "lat"), crs = 4326)
    nearest <- sf::st_nearest_feature(points, plots_ll)
    distance <- as.numeric(units::drop_units(sf::st_distance(points, plots_ll[nearest, ],
                                                            by_element = TRUE)))
    within <- sf::st_is_within_distance(points, plots_ll, dist = threshold_m)
    for (j in seq_along(gps)) {
      i <- gps[j]
      dec$min_dist_m[i] <- distance[j]
      dec$keep_original_name[i] <- FALSE
      if (length(within[[j]]) >= 2L) {
        candidates <- within[[j]]
        d <- as.numeric(units::drop_units(sf::st_distance(points[j, ], plots_ll[candidates, ])))
        ids <- gps_names[candidates[order(d)]]
        ids <- ids[!is.na(ids) & nzchar(ids)]
        dec$base_name[i] <- paste(ids, collapse = " ")
        dec$status[i] <- "multiple_within_threshold"
        dec$dest_dir[i] <- dest_dirs["multiple"]
      } else if (!is.finite(distance[j]) || distance[j] > far_max_m) {
        dec$status[i] <- "gps_over_max_distance"
        dec$dest_dir[i] <- dest_dirs["far"]
        dec$keep_original_name[i] <- TRUE
      } else {
        dec$base_name[i] <- gps_names[nearest[j]]
        far <- distance[j] > threshold_m
        dec$status[i] <- if (far) "far_over_threshold" else "correct_within_threshold"
        dec$dest_dir[i] <- dest_dirs[if (far) "far" else "correct"]
      }
    }
  }
  missing_name <- is.na(dec$base_name) | !nzchar(dec$base_name)
  dec$keep_original_name[missing_name] <- TRUE

  if (!dir.exists(out_root) && !dir.create(out_root, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create output folder: ", out_root, call. = FALSE)
  }
  log_dir <- dirname(write_log_csv)
  if (!dir.exists(log_dir) && !dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create log folder: ", log_dir, call. = FALSE)
  }
  rows <- vector("list", nrow(dec))
  if (verbose) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(dec), style = 3)
    on.exit(close(pb), add = TRUE)
  }
  for (i in seq_len(nrow(dec))) {
    src <- dec$SourceFile[i]
    dest_dir <- dec$dest_dir[i]
    new_dirs <- character()
    p <- dest_dir
    while (!dir.exists(p) && path_key(p) != path_key(out_root)) {
      new_dirs <- c(new_dirs, p)
      parent <- dirname(p)
      if (identical(parent, p)) break
      p <- parent
    }
    stem <- if (dec$keep_original_name[i]) file_stem(src) else dec$base_name[i]
    destination <- allocate_dest_path(dest_dir, stem, file_extension(src), reserved = write_log_csv)
    dir_ok <- dir.exists(dest_dir) || dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    ok <- dir_ok && isTRUE(tryCatch(file.copy(src, destination, overwrite = FALSE, copy.date = TRUE),
                                   error = function(e) FALSE))
    if (!ok) {
      warning("Failed to copy: ", src, " -> ", destination, call. = FALSE)
      # The destination was unused; discard any partial result of a failed copy.
      if (file.exists(destination) && !dir.exists(destination)) unlink(destination)
      for (p in new_dirs) {
        if (dir.exists(p) && !length(list.files(p, all.files = TRUE, no.. = TRUE))) unlink(p, recursive = TRUE)
      }
    }
    rows[[i]] <- data.frame(source_file = src, new_file = if (ok) destination else NA_character_,
                            lon = dec$GPSLongitude[i], lat = dec$GPSLatitude[i],
                            min_dist_m = dec$min_dist_m[i],
                            ids_used = if (dec$keep_original_name[i]) NA_character_ else dec$base_name[i],
                            status = dec$status[i], copy_ok = ok, stringsAsFactors = FALSE)
    if (verbose) utils::setTxtProgressBar(pb, i)
  }
  current <- do.call(rbind, rows)
  log <- rbind(old, current)
  write_processing_log(log, write_log_csv)
  if (verbose) {
    message("\nCopied ", sum(current$copy_ok), " of ", nrow(current), " new files; ",
            sum(!current$copy_ok), " failed. Log: ", write_log_csv)
    print(with(current, table(status, copy_ok, useNA = "ifany")))
  }
  invisible(log)
}
