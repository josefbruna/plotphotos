#' Organize field photographs and annotate dates
#'
#' Tools for copying field photographs and accompanying files into plot-based categories and for annotating dated, optionally resized JPEG copies.
#'
#'
#' @details
#' The package has two public workflows:
#' \describe{
#' \item{\code{\link{copy_photos_to_plots}}}{Copy field files into plot-based
#' categories using JPEG GPS coordinates, parent folders, or filename prefixes.
#' Files without usable GPS are retained in matching or unmatched categories.}
#' \item{\code{\link{write_into_image}}}{Optionally reduce JPEG dimensions and
#' write capture dates onto copies, while retaining input originals.}
#' }
#'
#' See the individual function pages for arguments, executable temporary-directory
#' examples, return values, and limitations. Both workflows are file-processing
#' utilities, not backup or synchronization systems. Read their returned logs and
#' retain original photographs. No AI service is called while using the package.
#'
#' Use \code{citation("plotphotos")} for the package citation and the separately
#' identified AI-tool reference. Report reproducible issues using the package's
#' BugReports address. This version is a CRAN-preparation release, not a claim of
#' CRAN acceptance or completed cross-platform validation.
#'
#' @section Authorship and extensive AI assistance:
#' Created and maintained by Josef Brůna, with extensive assistance from OpenAI's ChatGPT in code development, debugging, tests, and documentation. Josef Brůna defines the requirements and is responsible for reviewing and approving releases and maintaining the package. ChatGPT is acknowledged as a development tool, not as a package author or a runtime dependency. The precise model snapshots used throughout development were not recorded.
#'
#' @author Josef Brůna (author and maintainer). Institute of Botany (copyright holder).
#'
#' @references
#' OpenAI (2026). ChatGPT [Large language model].
#' \url{https://chatgpt.com/}. Acknowledged for extensive development assistance;
#' not listed as a package co-author. Specific model snapshots were not recorded.
#'
#' @seealso \code{\link{copy_photos_to_plots}}, \code{\link{write_into_image}}
#' @name plotphotos-package
#' @aliases plotphotos
#' @encoding UTF-8
#' @docType package
#' @keywords package
NULL

sanitize_filename <- function(x) {
  x <- gsub("[\\\\/:*?\"<>|[:cntrl:]]+", "_", x)
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x <- sub("[. ]+$", "", x)
  x[is.na(x) | !nzchar(x) | x %in% c(".", "..")] <- "unnamed"
  reserved <- grepl("^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])([.]|$)",
                    x, ignore.case = TRUE)
  x[reserved] <- paste0("_", x[reserved])
  # Leave room for a suffix and extension on common filesystems.
  vapply(x, function(value) {
    while (nchar(value, type = "bytes") > 180L) {
      value <- substr(value, 1L, nchar(value) - 1L)
    }
    value
  }, character(1), USE.NAMES = FALSE)
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

allocate_dest_path <- function(dest_dir, base_no_ext, ext, reserved = character()) {
  base_no_ext <- sanitize_filename(base_no_ext)
  path <- make_path(dest_dir, base_no_ext, ext)
  suffix <- 0L
  while (file.exists(path) || path_key(path) %in% path_key(reserved)) {
    suffix <- suffix + 1L
    suffix_text <- paste0("_", suffix)
    stem <- base_no_ext
    while (nchar(stem, type = "bytes") + nchar(suffix_text) > 180L) {
      stem <- substr(stem, 1L, nchar(stem) - 1L)
    }
    path <- make_path(dest_dir, paste0(stem, suffix_text), ext)
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

# Traverse directories once, pruning output trees before descending. This avoids
# recursing forever through a symbolic-link cycle. Preserve lexical file paths
# for parent-folder matching and stamping's relative-folder mapping.
scan_input_files <- function(roots, exclude_trees = character(),
                             exclude_files = character()) {
  roots <- canonical_path(roots)
  exclude_trees <- canonical_path(exclude_trees)
  exclude_files <- path_key(canonical_path(exclude_files))
  seen_dirs <- seen_files <- character()
  result <- character()
  skipped <- character()
  inside_any <- function(path, trees) {
    any(vapply(trees, function(tree) path_within(path, tree), logical(1)))
  }
  pending <- sort(roots)
  while (length(pending)) {
    directory <- pending[1L]
    pending <- pending[-1L]
    real <- canonical_path(directory)
    if (path_key(real) %in% seen_dirs || inside_any(real, exclude_trees)) next
    if (!inside_any(real, roots)) {
      skipped <- c(skipped, directory)
      next
    }
    seen_dirs <- c(seen_dirs, path_key(real))
    entries <- list.files(directory, full.names = TRUE, all.files = TRUE,
                          no.. = TRUE)
    if (file.access(directory, 4L) != 0L) {
      warning("Cannot read input directory: ", directory, call. = FALSE)
    }
    for (entry in entries) {
      if (dir.exists(entry)) {
        pending <- c(pending, entry)
      } else if (isTRUE(utils::file_test("-f", entry))) {
        target <- canonical_path(entry)
        key <- path_key(target)
        if (inside_any(target, exclude_trees) || key %in% exclude_files ||
            key %in% seen_files) next
        if (!inside_any(target, roots)) {
          skipped <- c(skipped, entry)
          next
        }
        seen_files <- c(seen_files, key)
        result <- c(result, entry)
      }
    }
  }
  if (length(skipped)) {
    warning("Skipped symbolic links outside the input roots: ",
            paste(unique(skipped), collapse = ", "), call. = FALSE)
  }
  sort(result)
}

# Replace a log only after its new contents have been written. A previous log
# is kept as a backup until replacement succeeds (including on Windows).
write_processing_log <- function(log, path) {
  parent <- dirname(path)
  if (!dir.exists(parent) &&
      !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create log directory: ", parent, call. = FALSE)
  }
  if (dir.exists(path)) stop("Log path is a directory: ", path, call. = FALSE)
  tmp <- tempfile(".plotphotos-log-", tmpdir = parent, fileext = ".csv")
  backup <- NULL
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(log, tmp, row.names = FALSE, fileEncoding = "UTF-8")
  if (file.exists(path)) {
    backup <- tempfile(".plotphotos-log-backup-", tmpdir = parent, fileext = ".csv")
    if (!replace_path(path, backup)) {
      stop("Cannot replace log; the original log was left unchanged: ", path,
           call. = FALSE)
    }
  }
  if (!replace_path(tmp, path)) {
    restored <- is.null(backup) || replace_path(backup, path)
    detail <- if (restored) "The previous log was preserved." else
      paste("The previous log is recoverable at", backup)
    stop("Cannot save log: ", path, ". ", detail, call. = FALSE)
  }
  if (!is.null(backup)) unlink(backup)
  invisible(path)
}

# Never silently accept a truncated CSV after an encoding/read warning.
read_processing_log <- function(path) {
  withCallingHandlers(
    utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
    warning = function(w) {
      stop("Cannot safely read the existing UTF-8 log: ", conditionMessage(w),
           ". Convert a legacy log to UTF-8 or choose a new log path.", call. = FALSE)
    }
  )
}

# Separate binding so failure recovery can be tested without changing base R.
replace_path <- function(from, to) file.rename(from, to)
