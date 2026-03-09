#' Copy and rename photos by nearest plot
#'
#' Copies (does not move) JPG/JPEG photos from one or more input folders, reads EXIF GPS,
#' assigns each GPS photo to the nearest plot, and copies into flattened output subfolders.
#' Photos without GPS are skipped by default, but can still be copied when:
#' * `names = TRUE`: the filename already begins with a value from `match_field` (any suffix).
#' * `folders = TRUE`: the parent folder name equals a value from `match_field`
#'   (files are renamed using `name_field`).
#'
#' Naming avoids conflicts across runs by checking the output folder for existing filenames.
#' Additionally, if a log exists, the function will skip sources already copied successfully.
#'
#' @param img_roots Character vector of input root folders (recursive).
#' @param out_root Output root folder.
#' @param plots_path Path to GeoPackage (.gpkg) or Shapefile (.shp) with plot points.
#' @param plots_layer Layer name inside GeoPackage (required only if multiple layers).
#' @param match_field Field name used for matching no-GPS photos by filename prefix or parent folder.
#' @param name_field Field name used for the final output filename stem. Defaults to `match_field`.
#' @param threshold_m Distance threshold (meters) for "correct" vs "far".
#' @param far_max_m Upper distance threshold (meters) for "far".
#' @param names Logical; allow copying no-GPS photos whose filename already begins with a value from `match_field` into subfolder "name".
#' @param folders Logical; allow copying no-GPS photos whose parent folder name equals a value from `match_field` into subfolder "folder" (renamed using `name_field`).
#' @param sub_correct Subfolder name for within threshold.
#' @param sub_far Subfolder name for over threshold.
#' @param sub_multiple Subfolder name for multiple within threshold.
#' @param sub_no_gps Subfolder name for no-GPS photos (logged; not copied unless `names`/`folders` rules match).
#' @param sub_named Subfolder name for name-rule copies.
#' @param sub_folder Subfolder name for folder-rule copies.
#' @param write_log_csv Path to output log CSV (default: `<out_root>/rename_log.csv`).
#' @param verbose Logical; print folder indicator and summary.
#' @return Invisibly returns the log data.frame.
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
    sub_correct  = "correct",
    sub_far      = "review_far",
    sub_multiple = "review_multiple",
    sub_no_gps   = "no_gps",
    sub_named    = "name",
    sub_folder   = "folder",
    write_log_csv = file.path(out_root, "rename_log.csv"),
    verbose = TRUE
) {
  stopifnot(is.character(img_roots), length(img_roots) >= 1)
  stopifnot(is.character(out_root), length(out_root) == 1)
  stopifnot(is.character(plots_path), length(plots_path) == 1)
  
  if (!file.exists(plots_path)) {
    stop(sprintf("Plots file not found: %s", plots_path))
  }
  
  # ---- read plots (gpkg or shp) ----
  is_gpkg <- grepl("\\.gpkg$", plots_path, ignore.case = TRUE)
  
  if (is_gpkg) {
    lyr <- sf::st_layers(plots_path)$name
    if (length(lyr) == 0) stop("No layers found in GeoPackage.")
    
    if (is.null(plots_layer) || !nzchar(plots_layer)) {
      if (length(lyr) == 1) {
        plots_layer <- lyr[1]
      } else {
        stop(sprintf(
          "GeoPackage has multiple layers (%s). Provide plots_layer.",
          paste(lyr, collapse = ", ")
        ))
      }
    }
    
    plots <- sf::st_read(plots_path, layer = plots_layer, quiet = TRUE)
  } else {
    plots <- sf::st_read(plots_path, quiet = TRUE)
  }
  
  if (!match_field %in% names(plots)) {
    stop(sprintf("Field '%s' not found in plots data.", match_field))
  }
  if (!name_field %in% names(plots)) {
    stop(sprintf("Field '%s' not found in plots data.", name_field))
  }
  if (is.na(sf::st_crs(plots))) {
    stop("Plots data has no CRS defined. Define CRS before running (st_set_crs).")
  }
  
  # ---- output folders ----
  dir.create(out_root, showWarnings = FALSE, recursive = TRUE)
  out_subs <- c(sub_correct, sub_far, sub_multiple, sub_named, sub_folder)
  for (s in out_subs) {
    dir.create(file.path(out_root, s), showWarnings = FALSE, recursive = TRUE)
  }
  
  # ---- existing log (skip already-copied sources) ----
  already_done <- character(0)
  if (file.exists(write_log_csv)) {
    old <- tryCatch(
      utils::read.csv(write_log_csv, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    
    if (!is.null(old) && all(c("source_file", "new_file") %in% names(old))) {
      ok <- !is.na(old$new_file) & nzchar(old$new_file) & file.exists(old$new_file)
      already_done <- unique(old$source_file[ok])
    }
  }
  
  # ---- list jpg files across roots ----
  img_roots <- unique(img_roots)
  missing_roots <- img_roots[!dir.exists(img_roots)]
  if (length(missing_roots)) {
    warning(
      "Some img_roots do not exist and will be skipped:\n  ",
      paste(missing_roots, collapse = "\n  ")
    )
  }
  
  img_roots <- img_roots[dir.exists(img_roots)]
  if (!length(img_roots)) stop("No existing img_roots.")
  
  out_root_n <- normalizePath(out_root, winslash = "/", mustWork = FALSE)
  
  jpg_files <- unique(unlist(lapply(img_roots, function(root) {
    files <- list.files(
      root,
      pattern = "\\.(jpg|jpeg)$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
    
    files_n <- normalizePath(files, winslash = "/", mustWork = FALSE)
    root_n  <- normalizePath(root, winslash = "/", mustWork = FALSE)
    
    if (startsWith(out_root_n, paste0(root_n, "/")) || out_root_n == root_n) {
      files <- files[!startsWith(files_n, paste0(out_root_n, "/"))]
    } else {
      files <- files[!startsWith(files_n, paste0(out_root_n, "/"))]
    }
    
    files
  })))
  
  if (!length(jpg_files)) stop("No JPG/JPEG files found under img_roots.")
  
  if (length(already_done)) {
    jpg_files <- jpg_files[!jpg_files %in% already_done]
    if (!length(jpg_files)) {
      if (verbose) {
        message("Nothing to do: all found sources already copied (based on existing log).")
      }
      return(invisible(data.frame()))
    }
  }
  
  # ---- read EXIF for this batch ----
  ex <- exifr::read_exif(
    jpg_files,
    tags = c("SourceFile", "GPSLatitude", "GPSLongitude")
  )
  ex$ext <- normalize_ext(tools::file_ext(ex$SourceFile))
  ex$has_gps <- is.finite(ex$GPSLatitude) & is.finite(ex$GPSLongitude)
  
  # ---- choose metric CRS (UTM) ----
  if (any(ex$has_gps)) {
    lon0 <- mean(ex$GPSLongitude[ex$has_gps], na.rm = TRUE)
    lat0 <- mean(ex$GPSLatitude[ex$has_gps],  na.rm = TRUE)
  } else {
    c0 <- sf::st_transform(sf::st_centroid(sf::st_union(plots)), 4326)
    cc <- sf::st_coordinates(c0)
    lon0 <- cc[1]
    lat0 <- cc[2]
  }
  
  utm_epsg <- utm_epsg_from_lonlat(lon0, lat0)
  plots_utm <- sf::st_transform(plots, utm_epsg)
  
  # ---- decisions table ----
  dec <- data.frame(
    SourceFile = ex$SourceFile,
    GPSLatitude = ex$GPSLatitude,
    GPSLongitude = ex$GPSLongitude,
    ext = ex$ext,
    status = NA_character_,
    min_dist_m = NA_real_,
    base_name = NA_character_,
    dest_dir = NA_character_,
    keep_original_name = FALSE,
    stringsAsFactors = FALSE
  )
  
  # ---- prepare match/name lookup for no-GPS rules ----
  match_vals <- as.character(plots[[match_field]])
  name_vals  <- as.character(plots[[name_field]])
  
  keep <- !is.na(match_vals) & nzchar(match_vals)
  match_vals <- match_vals[keep]
  name_vals  <- name_vals[keep]
  
  # fallback: if name_field is empty for a row, use match_field
  missing_name <- is.na(name_vals) | !nzchar(name_vals)
  name_vals[missing_name] <- match_vals[missing_name]
  
  lookup_df <- unique(data.frame(
    match_value = match_vals,
    name_value  = name_vals,
    stringsAsFactors = FALSE
  ))
  lookup_df$match_key <- tolower(lookup_df$match_value)
  
  dup_keys <- unique(lookup_df$match_value[duplicated(lookup_df$match_key)])
  if (length(dup_keys)) {
    warning(
      sprintf(
        "Duplicate values in '%s' after lowercasing; first match will be used for no-GPS matching: %s",
        match_field,
        paste(dup_keys, collapse = ", ")
      )
    )
    lookup_df <- lookup_df[!duplicated(lookup_df$match_key), , drop = FALSE]
  }
  
  if (!nrow(lookup_df)) {
    stop(sprintf("No non-empty values found in match_field '%s'.", match_field))
  }
  
  match_set <- lookup_df$match_value
  match_ord <- match_set[order(nchar(match_set), decreasing = TRUE)]
  match_keys <- lookup_df$match_key
  match_to_name <- stats::setNames(lookup_df$name_value, lookup_df$match_key)
  
  # ---- handle no GPS ----
  no_gps_idx <- which(!ex$has_gps)
  if (length(no_gps_idx)) {
    dec$status[no_gps_idx] <- "no_gps"
    dec$keep_original_name[no_gps_idx] <- TRUE
    dec$dest_dir[no_gps_idx] <- file.path(out_root, sub_no_gps)
    
    parent_folder <- basename(dirname(dec$SourceFile[no_gps_idx]))
    
    if (isTRUE(folders)) {
      parent_key <- tolower(parent_folder)
      hit <- parent_key %in% match_keys
      
      if (any(hit)) {
        idx <- no_gps_idx[hit]
        final_name <- unname(match_to_name[parent_key[hit]])
        
        dec$status[idx] <- "no_gps_folder"
        dec$keep_original_name[idx] <- FALSE
        dec$base_name[idx] <- final_name
        dec$dest_dir[idx] <- file.path(out_root, sub_folder)
      }
    }
    
    if (isTRUE(names)) {
      still <- no_gps_idx[dec$status[no_gps_idx] == "no_gps"]
      
      if (length(still)) {
        base_still <- tools::file_path_sans_ext(basename(dec$SourceFile[still]))
        matched_value <- vapply(
          base_still,
          function(b) match_locality_prefix(b, match_ord),
          character(1)
        )
        
        hit2 <- !is.na(matched_value)
        if (any(hit2)) {
          idx <- still[hit2]
          
          dec$status[idx] <- "no_gps_named"
          dec$keep_original_name[idx] <- FALSE
          dec$base_name[idx] <- unname(match_to_name[tolower(matched_value[hit2])])
          dec$dest_dir[idx] <- file.path(out_root, sub_named)
        }
      }
    }
  }
  
  # ---- handle GPS photos ----
  gps_idx <- which(ex$has_gps)
  if (length(gps_idx)) {
    pts <- sf::st_as_sf(
      data.frame(
        lon = ex$GPSLongitude[gps_idx],
        lat = ex$GPSLatitude[gps_idx]
      ),
      coords = c("lon", "lat"),
      crs = 4326
    )
    pts_utm <- sf::st_transform(pts, utm_epsg)
    
    plot_name_utm  <- as.character(plots_utm[[name_field]])
    plot_match_utm <- as.character(plots_utm[[match_field]])
    missing_plot_name <- is.na(plot_name_utm) | !nzchar(plot_name_utm)
    plot_name_utm[missing_plot_name] <- plot_match_utm[missing_plot_name]
    
    nearest_idx <- sf::st_nearest_feature(pts_utm, plots_utm)
    min_dist <- sf::st_distance(pts_utm, plots_utm[nearest_idx, ], by_element = TRUE)
    min_dist_num <- as.numeric(units::drop_units(min_dist))
    
    within_list <- sf::st_is_within_distance(pts_utm, plots_utm, dist = threshold_m)
    
    for (j in seq_along(gps_idx)) {
      row_i <- gps_idx[j]
      w <- within_list[[j]]
      dec$min_dist_m[row_i] <- min_dist_num[j]
      
      if (length(w) >= 2) {
        dec$status[row_i] <- "multiple_within_threshold"
        dec$dest_dir[row_i] <- file.path(out_root, sub_multiple)
        
        d_w <- sf::st_distance(pts_utm[j, ], plots_utm[w, ], by_element = FALSE)
        d_w_num <- as.numeric(units::drop_units(d_w))
        ord <- order(d_w_num)
        
        final_names <- plot_name_utm[w[ord]]
        dec$base_name[row_i] <- paste(final_names, collapse = " ")
        
      } else {
        final_name <- plot_name_utm[nearest_idx[j]]
        dec$base_name[row_i] <- final_name
        
        if (min_dist_num[j] > threshold_m) {
          if (is.finite(far_max_m) && min_dist_num[j] > far_max_m) {
            dec$status[row_i] <- "gps_over_max_distance"
            dec$dest_dir[row_i] <- NA_character_
          } else {
            dec$status[row_i] <- "far_over_threshold"
            dec$dest_dir[row_i] <- file.path(out_root, sub_far)
          }
        } else {
          dec$status[row_i] <- "correct_within_threshold"
          dec$dest_dir[row_i] <- file.path(out_root, sub_correct)
        }
      }
    }
  }
  
  # ---- copy with progress + folder indicator ----
  dec <- dec[order(dec$SourceFile), , drop = FALSE]
  n_all <- nrow(dec)
  
  src_dirs <- normalizePath(dirname(dec$SourceFile), winslash = "/", mustWork = FALSE)
  dir_levels <- unique(src_dirs)
  dir_idx <- match(src_dirs, dir_levels)
  current_dir_idx <- NA_integer_
  
  pb <- utils::txtProgressBar(min = 0, max = n_all, style = 3)
  on.exit(try(close(pb), silent = TRUE), add = TRUE)
  
  copied_n <- 0L
  skipped_no_gps_n <- 0L
  skipped_done_n <- 0L
  failed_n <- 0L
  
  log_rows <- vector("list", n_all)
  
  for (i in seq_len(n_all)) {
    src <- dec$SourceFile[i]
    
    if (is.na(current_dir_idx) || dir_idx[i] != current_dir_idx) {
      current_dir_idx <- dir_idx[i]
      if (verbose) {
        message(sprintf(
          "\n[%d/%d] Processing folder: %s",
          current_dir_idx, length(dir_levels), dirname(src)
        ))
      }
    }
    
    if (length(already_done) && src %in% already_done) {
      skipped_done_n <- skipped_done_n + 1L
      log_rows[[i]] <- data.frame(
        source_file = src,
        new_file = NA_character_,
        lon = dec$GPSLongitude[i],
        lat = dec$GPSLatitude[i],
        min_dist_m = dec$min_dist_m[i],
        ids_used = dec$base_name[i],
        status = paste0(dec$status[i], "_already_done"),
        copy_ok = NA,
        stringsAsFactors = FALSE
      )
      utils::setTxtProgressBar(pb, i)
      next
    }
    
    if (identical(dec$status[i], "no_gps")) {
      skipped_no_gps_n <- skipped_no_gps_n + 1L
      log_rows[[i]] <- data.frame(
        source_file = src,
        new_file = NA_character_,
        lon = dec$GPSLongitude[i],
        lat = dec$GPSLatitude[i],
        min_dist_m = dec$min_dist_m[i],
        ids_used = NA_character_,
        status = dec$status[i],
        copy_ok = NA,
        stringsAsFactors = FALSE
      )
      utils::setTxtProgressBar(pb, i)
      next
    }
    
    # skip anything that has no destination (e.g. gps_over_max_distance)
    if (is.na(dec$dest_dir[i]) || !nzchar(dec$dest_dir[i])) {
      log_rows[[i]] <- data.frame(
        source_file = src,
        new_file = NA_character_,
        lon = dec$GPSLongitude[i],
        lat = dec$GPSLatitude[i],
        min_dist_m = dec$min_dist_m[i],
        ids_used = dec$base_name[i],
        status = dec$status[i],
        copy_ok = NA,
        stringsAsFactors = FALSE
      )
      utils::setTxtProgressBar(pb, i)
      next
    }
    
    dest_dir <- dec$dest_dir[i]
    dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
    
    ext <- dec$ext[i]
    
    if (isTRUE(dec$keep_original_name[i])) {
      base_no_ext <- tools::file_path_sans_ext(basename(src))
    } else {
      base_no_ext <- dec$base_name[i]
      if (is.na(base_no_ext) || base_no_ext == "") {
        base_no_ext <- "unknown_id"
      }
    }
    
    dest_path <- allocate_dest_path(dest_dir, base_no_ext, ext)
    
    ok <- file.copy(src, dest_path, overwrite = FALSE)
    if (!isTRUE(ok)) {
      failed_n <- failed_n + 1L
      warning(sprintf("Failed to copy: %s -> %s", src, dest_path))
      dest_written <- NA_character_
    } else {
      copied_n <- copied_n + 1L
      dest_written <- dest_path
    }
    
    log_rows[[i]] <- data.frame(
      source_file = src,
      new_file = dest_written,
      lon = dec$GPSLongitude[i],
      lat = dec$GPSLatitude[i],
      min_dist_m = dec$min_dist_m[i],
      ids_used = if (isTRUE(dec$keep_original_name[i])) NA_character_ else dec$base_name[i],
      status = dec$status[i],
      copy_ok = ok,
      stringsAsFactors = FALSE
    )
    
    utils::setTxtProgressBar(pb, i)
  }
  
  log_df_run <- do.call(rbind, log_rows)
  log_df <- log_df_run
  
  if (file.exists(write_log_csv)) {
    old <- tryCatch(
      utils::read.csv(write_log_csv, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (!is.null(old)) {
      log_df <- rbind(old, log_df)
    }
  }
  
  utils::write.csv(log_df, write_log_csv, row.names = FALSE)
  
  if (verbose) {
    message("\nDone.")
    message("Threshold (m): ", threshold_m)
    message("Max far distance (m): ", ifelse(is.finite(far_max_m), far_max_m, "Inf"))
    message("Plots file: ", plots_path)
    if (is_gpkg) message("Plots layer: ", plots_layer)
    message("Match field: ", match_field)
    message("Name field: ", name_field)
    message("Log written to: ", write_log_csv)
    message("Output folders (flat) under: ", out_root)
    
    df <- log_df_run
    
    ok   <- !is.na(df$copy_ok) & df$copy_ok
    fail <- !is.na(df$copy_ok) & !df$copy_ok
    already <- grepl("_already_done$", df$status)
    
    # GPS categories (copied successfully)
    gps_correct  <- sum(df$status == "correct_within_threshold"  & ok)
    gps_far      <- sum(df$status == "far_over_threshold"        & ok)
    gps_multiple <- sum(df$status == "multiple_within_threshold" & ok)
    
    # GPS categories (not copied)
    gps_too_far <- sum(df$status == "gps_over_max_distance")
    
    # No GPS categories (copied successfully)
    no_gps_name   <- sum(df$status == "no_gps_named"  & ok)
    no_gps_folder <- sum(df$status == "no_gps_folder" & ok)
    
    # Not copied categories
    not_copied_plain_no_gps <- sum(df$status == "no_gps")
    not_copied_already      <- sum(already)
    failed_copies           <- sum(fail)
    
    total_seen   <- nrow(df)
    total_copied <- sum(ok)
    
    message("\nSummary (this run):")
    
    message("GPS (copied):")
    message("  Correct (<= ", threshold_m, " m): ", gps_correct)
    message("  Far (> ", threshold_m, " m): ", gps_far)
    message("  Multiple within threshold: ", gps_multiple)
    
    message("GPS (not copied):")
    message("  Too far (> ", ifelse(is.finite(far_max_m), far_max_m, "Inf"), " m): ", gps_too_far)
    
    message("No GPS (copied):")
    message("  By filename starts with ", match_field, " (names=TRUE): ", no_gps_name)
    message("  By parent folder = ", match_field, " (folders=TRUE): ", no_gps_folder)
    
    message("Not copied:")
    message("  No GPS (not name/folder): ", not_copied_plain_no_gps)
    message("  Already done (from existing log): ", not_copied_already)
    message("  Failed copies: ", failed_copies)
    
    message("Totals:")
    message("  Seen this run: ", total_seen)
    message("  Copied this run: ", total_copied)
    
    print(with(df, table(status, copy_ok, useNA = "ifany")))
    
    message("\nCumulative status counts (all runs):")
    print(table(log_df$status, useNA = "ifany"))
  }
  
  invisible(log_df)
}