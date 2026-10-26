# Internal helpers shared by the two public workflows.
utm_epsg_from_lonlat <- function(lon, lat) {
  zone <- max(1, min(60, floor((lon + 180) / 6) + 1))
  if (lat >= 0) 32600 + zone else 32700 + zone
}

sanitize_filename <- function(x) {
  x <- gsub("[\\\\/:*?\"<>|]+", "_", x)
  x <- trimws(gsub("\\s+", " ", x))
  x[is.na(x) | !nzchar(x) | x %in% c(".", "..")] <- "unnamed"
  x
}

normalize_ext <- function(ext) {
  ext[tolower(ext) %in% c("jpg", "jpeg")] <- "jpg"
  ext
}

file_extension <- function(path) {
  ext <- tools::file_ext(path)
  ext[grepl("^\\.[^.]+$", basename(path))] <- ""
  ext
}

file_stem <- function(path) {
  filename <- basename(path)
  ext <- file_extension(path)
  keep <- nzchar(ext)
  filename[keep] <- substr(filename[keep], 1L, nchar(filename[keep]) - nchar(ext[keep]) - 1L)
  filename
}

make_path <- function(dest_dir, base_no_ext, ext) {
  ext <- normalize_ext(ext)
  file.path(dest_dir, paste0(sanitize_filename(base_no_ext), ifelse(nzchar(ext), paste0(".", ext), "")))
}

allocate_dest_path <- function(dest_dir, base_no_ext, ext) {
  base_no_ext <- sanitize_filename(base_no_ext)
  path <- make_path(dest_dir, base_no_ext, ext)
  suffix <- 0L
  while (file.exists(path)) {
    suffix <- suffix + 1L
    path <- make_path(dest_dir, paste0(base_no_ext, "_", suffix), ext)
  }
  path
}

match_locality_prefix <- function(filename_no_ext, ids) {
  stem <- tolower(filename_no_ext)
  for (id in ids) {
    key <- tolower(id)
    if (identical(stem, key) || startsWith(stem, paste0(key, "_")) ||
        startsWith(stem, paste0(key, " ")) || startsWith(stem, paste0(key, "-"))) return(id)
  }
  NA_character_
}

# Resolve existing ancestors too, so relative paths, symlinks and missing output
# directories can be compared consistently before any directory is created.
canonical_path <- function(paths) {
  one <- function(path) {
    path <- path.expand(path)
    if (file.exists(path) || dir.exists(path)) return(normalizePath(path, winslash = "/", mustWork = TRUE))
    parent <- dirname(path)
    if (identical(parent, path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
    parent <- one(parent)
    leaf <- basename(path)
    if (leaf == ".") return(parent)
    if (leaf == "..") return(dirname(parent))
    file.path(parent, leaf)
  }
  vapply(paths, one, character(1), USE.NAMES = FALSE)
}

path_key <- function(path) {
  if (.Platform$OS.type == "windows") tolower(path) else path
}

path_within <- function(path, root) {
  path <- path_key(path)
  root <- sub("/+$", "", path_key(root))
  path == root | startsWith(path, paste0(root, "/"))
}
