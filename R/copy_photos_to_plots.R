#' Copy and sort files by nearest plot or locality name
#'
#' Copies every regular file from one or more input folders into a flattened
#' output structure. JPEG/JPG photographs with valid EXIF coordinates are assigned
#' to the nearest plot. Files without usable GPS, including other file formats,
#' are matched by parent folder or filename when enabled; unmatched files are
#' copied to the no-GPS folder. Input files are never moved or overwritten.
#'
#' @details
#' The sorting order is:
#' * Valid JPEG GPS: use the correct, multiple, or far folder. Files farther than
#'   \code{far_max_m} are still copied to the far folder, but keep their original
#'   filename rather than being renamed to a possibly unrelated plot.
#' * No usable GPS and \code{folders = TRUE}: match the immediate parent folder
#'   to \code{match_field}, ignoring case.
#' * Otherwise, with \code{names = TRUE}: match the filename stem to
#'   \code{match_field}, ignoring case. An ID must end the stem or be followed by
#'   an underscore, space, or hyphen; the longest matching ID wins.
#' * Otherwise: copy to the no-GPS folder with the original filename.
#'
#' Folder matches take precedence over filename matches. Matched files are
#' renamed using \code{name_field}, falling back to \code{match_field} when empty.
#' JPEG extensions are normalized to .jpg; other extensions are preserved.
#' Existing destination files are never overwritten: numeric suffixes resolve
#' collisions. A source already recorded as successfully copied is skipped only
#' while its logged destination still exists. Missing or unreadable EXIF never
#' prevents a file from being copied.
#'
#' Category folders are created only when needed. A nested output tree and the
#' log file are excluded from input scanning. Input roots cannot equal or be
#' inside the output root. Source modification times are preserved when copying.
#'
#' @param img_roots Character vector of input root folders, scanned recursively
#'   including hidden files and folders.
#' @param out_root Output root folder.
#' @param plots_path Path to a GeoPackage (.gpkg) or Shapefile (.shp) with plots.
#' @param plots_layer Layer name inside a GeoPackage; required if it has multiple layers.
#' @param match_field Field used for matching files without GPS by name or folder.
#' @param name_field Field used for output filename stems. Defaults to \code{match_field}.
#' @param threshold_m Non-negative distance in metres separating correct and far files.
#' @param far_max_m Maximum distance in metres for renaming a GPS file to its nearest
#'   plot. Must be at least \code{threshold_m}; use \code{Inf} for no limit. More
#'   distant files are copied to \code{sub_far} with their original filename.
#' @param names Logical; enable filename-prefix matching for files without GPS.
#' @param folders Logical; enable parent-folder matching for files without GPS.
#' @param sub_correct Relative subfolder for one plot within \code{threshold_m}.
#' @param sub_far Relative subfolder for GPS files beyond \code{threshold_m}, including
#'   files beyond \code{far_max_m}.
#' @param sub_multiple Relative subfolder for multiple plots within \code{threshold_m}.
#' @param sub_no_gps Relative subfolder for files without GPS and without an enabled match.
#' @param sub_named Relative subfolder for filename-rule matches.
#' @param sub_folder Relative subfolder for parent-folder-rule matches.
#' @param write_log_csv Output CSV path, defaulting to rename_log.csv under \code{out_root}.
#' @param verbose Logical; print progress and a summary.
#' @return Invisibly returns the cumulative log data.frame. Columns are
#'   \code{source_file}, \code{new_file}, \code{lon}, \code{lat}, \code{min_dist_m},
#'   \code{ids_used}, \code{status}, and \code{copy_ok}. Failed copies have a missing
#'   destination and \code{copy_ok = FALSE}. If there is no new work, the existing
#'   log is returned, or an empty log with the same columns.
#' @seealso \code{\link{write_into_image}}
#' @examples
#' \dontrun{
#' copy_photos_to_plots(
#'   img_roots = c("photos/site_A", "photos/site_B"),
#'   out_root = "sorted_photos",
#'   plots_path = "active.gpkg",
#'   plots_layer = "localities_active",
#'   threshold_m = 30,
#'   far_max_m = 100
#' )
#' }
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
  base::names(dest_dirs) <- c("correct", "far", "multiple", "no_gps", "name", "folder")

  empty_log <- data.frame(source_file = character(), new_file = character(),
                          lon = numeric(), lat = numeric(), min_dist_m = numeric(),
                          ids_used = character(), status = character(), copy_ok = logical(),
                          stringsAsFactors = FALSE)
  old <- empty_log
  if (file.exists(write_log_csv)) {
    old <- tryCatch(utils::read.csv(write_log_csv, stringsAsFactors = FALSE),
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

  files <- unique(canonical_path(unlist(lapply(img_roots, function(root) {
    list.files(root, full.names = TRUE, recursive = TRUE, all.files = TRUE,
               include.dirs = FALSE, no.. = TRUE)
  }), use.names = FALSE)))
  files <- files[utils::file_test("-f", files)]
  files <- files[!path_within(files, out_root) & path_key(files) != path_key(write_log_csv)]
  files <- files[!duplicated(path_key(files))]
  files <- files[!path_key(files) %in% already_done]
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
      idx <- match(path_key(dec$SourceFile), path_key(canonical_path(src[valid])))
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
    centre <- sf::st_coordinates(sf::st_transform(sf::st_centroid(sf::st_union(plots_gps)), 4326))
    crs <- utm_epsg_from_lonlat(centre[1L, 1L], centre[1L, 2L])
    plots_utm <- sf::st_transform(plots_gps, crs)
    points <- sf::st_as_sf(data.frame(lon = dec$GPSLongitude[gps], lat = dec$GPSLatitude[gps]),
                           coords = c("lon", "lat"), crs = 4326)
    points <- sf::st_transform(points, crs)
    nearest <- sf::st_nearest_feature(points, plots_utm)
    distance <- as.numeric(units::drop_units(sf::st_distance(points, plots_utm[nearest, ],
                                                            by_element = TRUE)))
    within <- sf::st_is_within_distance(points, plots_utm, dist = threshold_m)
    for (j in seq_along(gps)) {
      i <- gps[j]
      dec$min_dist_m[i] <- distance[j]
      dec$keep_original_name[i] <- FALSE
      if (length(within[[j]]) >= 2L) {
        candidates <- within[[j]]
        d <- as.numeric(units::drop_units(sf::st_distance(points[j, ], plots_utm[candidates, ])))
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
    destination <- allocate_dest_path(dest_dir, stem, file_extension(src))
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
  utils::write.csv(log, write_log_csv, row.names = FALSE)
  if (verbose) {
    message("\nCopied ", sum(current$copy_ok), " of ", nrow(current), " new files; ",
            sum(!current$copy_ok), " failed. Log: ", write_log_csv)
    print(with(current, table(status, copy_ok, useNA = "ifany")))
  }
  invisible(log)
}
