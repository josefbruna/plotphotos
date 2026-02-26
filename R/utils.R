#' @keywords internal
utm_epsg_from_lonlat <- function(lon, lat) {
  zone <- floor((lon + 180) / 6) + 1
  if (lat >= 0) 32600 + zone else 32700 + zone
}

#' @keywords internal
sanitize_filename <- function(x) {
  # keep spaces but remove illegal/problematic chars
  x <- gsub("[\\\\/:*?\"<>|]+", "_", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' @keywords internal
normalize_ext <- function(ext) {
  ext <- tolower(ext)
  ext[ext == "jpeg"] <- "jpg"
  ext
}

#' @keywords internal
make_path <- function(dest_dir, base_no_ext, ext) {
  base_no_ext <- sanitize_filename(base_no_ext)
  ext <- normalize_ext(ext)
  file.path(dest_dir, paste0(base_no_ext, ".", ext))
}

#' @keywords internal
allocate_dest_path <- function(dest_dir, base_no_ext, ext) {
  # Rule: first is without suffix, second is _1, third _2, ...
  ext <- normalize_ext(ext)
  base_no_ext <- sanitize_filename(base_no_ext)

  p0 <- file.path(dest_dir, paste0(base_no_ext, ".", ext))
  if (!file.exists(p0)) return(p0)

  k <- 1L
  repeat {
    pk <- file.path(dest_dir, paste0(base_no_ext, "_", k, ".", ext))
    if (!file.exists(pk)) return(pk)
    k <- k + 1L
  }
}

#' @keywords internal
match_locality_prefix <- function(filename_no_ext, ids) {
  # ids should already be ordered by decreasing nchar
  b <- tolower(filename_no_ext)
  for (id in ids) {
    idk <- tolower(id)
    if (identical(b, idk)) return(id)
    if (startsWith(b, paste0(idk, "_")) ||
        startsWith(b, paste0(idk, " ")) ||
        startsWith(b, paste0(idk, "-"))) return(id)
  }
  NA_character_
}
