#' Write capture dates into JPEG copies
#'
#' Recursively writes capture dates into the bottom-right corner of JPEG/JPG
#' copies, using black text with a white halo. Original input files are unchanged.
#' Other files are copied unchanged, and relative subfolders are retained.
#'
#' @param input_folder Input folder to scan recursively.
#' @param output_folder Folder for stamped copies and the processing log. It must
#'   differ from the input folder and must not contain it. A nested output folder
#'   is excluded from scanning.
#' @param date_format A single non-empty date/time format accepted by
#'   [base::strftime()], such as `"%Y-%m-%d"` or
#'   `"%d.%m.%Y %H:%M"`. Defaults to `"%Y-%m-%d"`.
#'
#' @details
#' Dates are taken from the first valid EXIF DateTimeOriginal, DateTimeDigitized
#' (CreateDate), or DateTime tag, in that order. EXIF dates retain their recorded
#' clock time without timezone conversion. If no usable EXIF date exists, the
#' file modification time is used on every platform, so copied photos retain
#' their fallback date when their modification time was preserved. File ctime
#' (creation time on Windows; status-change time on Unix) is the last fallback.
#' These file times are not guaranteed capture dates; the log identifies their use.
#' If no date is available, the stamp reads \code{"unknown date"}.
#'
#' JPEGs are re-encoded at quality 75. EXIF orientation is applied to the pixels
#' before stamping, and the retained EXIF Orientation tag is reset to 1. The
#' original EXIF APP1 segment is retained on a best-effort basis; other metadata
#' (including ICC/XMP), embedded thumbnails, and dimension tags are not rebuilt.
#' CMYK JPEGs are retained as unprocessed originals instead of being rendered
#' with incorrect colours. No ImageMagick or external ExifTool is needed.
#'
#' Existing output files are never overwritten; numbered suffixes are added as
#' needed. If an existing file blocks a required output subfolder, that subfolder
#' also receives a numeric suffix. JPEGs that cannot be stamped are copied unchanged under
#' \code{unprocessed}, preserving their relative paths, and the reason is logged.
#' This folder is created only when needed. Input files are never moved.
#'
#' A cumulative \code{processing_log.csv} is written under the output folder.
#' That filename is reserved: an input file with the same name receives a
#' suffix, and an input subfolder with that name becomes
#' \code{processing_log_input}. Repeated calls create additional output copies.
#' An empty input folder produces an empty result without creating output folders.
#'
#' @return Invisibly returns a data frame for the current run, with columns
#'   \code{file}, \code{relative}, \code{output_file}, \code{status},
#'   \code{stamp_text}, \code{date_source}, \code{error}, and \code{processed_at}.
#'   Status is \code{stamped}, \code{copied} (another file type),
#'   \code{unprocessed} (failed JPEG retained), or \code{failed} (no output copy).
#' @seealso [copy_photos_to_plots()]
#' @examples
#' \dontrun{
#' write_into_image("plotphotos_output", "dated_photos")
#' write_into_image("photos", "dated_photos", date_format = "%d.%m.%Y %H:%M")
#' }
#' @export
write_into_image <- function(input_folder, output_folder,
                             date_format = "%Y-%m-%d") {
  for (arg in c("input_folder", "output_folder", "date_format")) {
    value <- get(arg)
    if (!is.character(value) || length(value) != 1L ||
        is.na(value) || !nzchar(value)) {
      stop(arg, " must be a single non-empty character string.", call. = FALSE)
    }
  }
  if (!dir.exists(input_folder)) stop("Input folder does not exist: ", input_folder)
  input_folder <- normalizePath(input_folder, winslash = "/", mustWork = TRUE)
  output_folder <- .stamp_absolute_path(output_folder)
  path_key <- function(x) if (.Platform$OS.type == "windows") tolower(x) else x
  if (identical(path_key(input_folder), path_key(output_folder))) {
    stop("input_folder and output_folder must be different.", call. = FALSE)
  }
  if (startsWith(path_key(input_folder),
                 paste0(sub("/+$", "", path_key(output_folder)), "/"))) {
    stop("output_folder must not contain input_folder.", call. = FALSE)
  }
  if (file.exists(output_folder) && !dir.exists(output_folder)) {
    stop("output_folder is an existing file: ", output_folder)
  }
  files <- list.files(input_folder, recursive = TRUE, all.files = TRUE,
                      full.names = TRUE, include.dirs = FALSE, no.. = TRUE)
  files <- files[utils::file_test("-f", files)]
  canonical <- normalizePath(files, winslash = "/", mustWork = TRUE)
  outside_output <- !startsWith(path_key(canonical),
                               paste0(sub("/+$", "", path_key(output_folder)), "/"))
  files <- files[outside_output]
  log_template <- data.frame(file = character(), relative = character(),
    output_file = character(), status = character(), stamp_text = character(),
    date_source = character(), error = character(), processed_at = character(),
    stringsAsFactors = FALSE)
  if (!length(files)) {
    message("No files found in: ", input_folder)
    return(invisible(log_template))
  }

  log_csv <- file.path(output_folder, "processing_log.csv")
  old <- NULL
  if (file.exists(log_csv)) {
    old <- tryCatch(utils::read.csv(log_csv, stringsAsFactors = FALSE),
                    error = function(e) NULL)
    required <- setdiff(names(log_template), "output_file")
    if (is.null(old) || !all(required %in% names(old))) {
      stop("Existing processing_log.csv is not a compatible processing log.",
           call. = FALSE)
    }
    if (!"output_file" %in% names(old)) old$output_file <- NA_character_
    old <- old[, names(log_template), drop = FALSE]
  }
  rows <- vector("list", length(files))
  root_prefix <- paste0(sub("/+$", "", input_folder), "/")
  reserved <- c(log_csv, file.path(output_folder, "unprocessed"))
  for (i in seq_along(files)) {
    src <- files[i]
    relative <- substring(src, nchar(root_prefix) + 1L)
    target_relative <- sub("^processing_log[.]csv/", "processing_log_input/",
                           relative, ignore.case = .Platform$OS.type == "windows")
    dest <- .stamp_unique_path(file.path(output_folder, target_relative),
                               reserved, root = output_folder)
    is_jpeg <- grepl("[.](jpg|jpeg)$", src, ignore.case = TRUE)
    status <- if (is_jpeg) "stamped" else "copied"
    stamp_text <- date_source <- error <- ""
    output_file <- NA_character_
    tryCatch({
      if (is_jpeg) {
        ex <- .stamp_exif(src)
        dt <- ex$dt
        date_source <- ex$date_source
        if (is.na(dt)) {
          info <- file.info(src)
          fields <- c("mtime", "ctime")
          for (field in fields) {
            if (!is.na(info[[field]][1])) {
              dt <- info[[field]][1]
              date_source <- paste0("file:", field)
              break
            }
          }
        }
        stamp_text <- if (is.na(dt)) "unknown date" else format(dt, date_format)
        .stamp_write(src, dest, stamp_text, ex$orientation, ex$app1)
      } else {
        .stamp_copy(src, dest)
      }
      output_file <- dest
    }, error = function(e) {
      error <<- conditionMessage(e)
      status <<- "failed"
      if (is_jpeg) {
        fallback <- .stamp_unique_path(file.path(output_folder, "unprocessed", relative),
                                       root = output_folder)
        tryCatch({
          .stamp_copy(src, fallback)
          output_file <<- fallback
          status <<- "unprocessed"
        }, error = function(copy_error) {
          error <<- paste(error, "Original copy also failed:", conditionMessage(copy_error))
        })
      }
    })
    rows[[i]] <- data.frame(file = src, relative = relative,
      output_file = output_file, status = status, stamp_text = stamp_text,
      date_source = date_source, error = error,
      processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE)
  }
  result <- do.call(rbind, rows)
  if (!dir.exists(output_folder) &&
      !dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create output folder: ", output_folder)
  }
  utils::write.csv(rbind(old, result), log_csv, row.names = FALSE)
  message("Stamped: ", sum(result$status == "stamped"),
          "; copied unchanged: ", sum(result$status == "copied"),
          "; unprocessed JPEGs retained: ", sum(result$status == "unprocessed"),
          "; failed copies: ", sum(result$status == "failed"),
          ". Log: ", log_csv)
  if (any(result$status == "failed")) {
    warning("Some files could not be copied; see processing_log.csv.", call. = FALSE)
  }
  invisible(result)
}

# Resolve a prospective output path using its nearest existing ancestor.
.stamp_absolute_path <- function(path) {
  path <- path.expand(path)
  tail <- character()
  while (!dir.exists(path) && !file.exists(path)) {
    parent <- dirname(path)
    if (identical(parent, path)) stop("Cannot resolve output_folder: ", path)
    tail <- c(basename(path), tail)
    path <- parent
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  for (part in tail) {
    if (part == "..") path <- dirname(path)
    else if (part != ".") path <- file.path(path, part)
  }
  path
}

.stamp_unique_path <- function(path, reserved = character(), root) {
  key <- function(x) if (.Platform$OS.type == "windows") tolower(x) else x
  # Reuse directories, but suffix a component blocked by an existing file.
  directory <- function(parent) {
    if (identical(key(parent), key(root)) || dir.exists(parent)) return(parent)
    above <- dirname(parent)
    if (identical(above, parent)) stop("Cannot resolve output subfolder: ", parent)
    base <- file.path(directory(above), basename(parent))
    candidate <- base
    i <- 0L
    while (file.exists(candidate) && !dir.exists(candidate)) {
      i <- i + 1L
      candidate <- paste0(base, "_", i)
    }
    candidate
  }
  path <- file.path(directory(dirname(path)), basename(path))
  base <- tools::file_path_sans_ext(path)
  ext <- tools::file_ext(path)
  suffix <- if (nzchar(ext)) paste0(".", ext) else ""
  candidate <- path
  i <- 0L
  while (file.exists(candidate) || key(candidate) %in% key(reserved)) {
    i <- i + 1L
    candidate <- paste0(base, "_", i, suffix)
  }
  candidate
}

.stamp_copy <- function(from, to) {
  parent <- dirname(to)
  new_parent <- !dir.exists(parent)
  if (new_parent && !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create folder: ", parent)
  }
  if (file.exists(to)) stop("Destination already exists: ", to)
  ok <- suppressWarnings(file.copy(from, to, overwrite = FALSE, copy.date = TRUE))
  if (!isTRUE(ok)) {
    if (file.exists(to)) unlink(to)
    if (new_parent && !length(list.files(parent, all.files = TRUE, no.. = TRUE))) {
      unlink(parent, recursive = TRUE)
    }
    stop("Cannot copy file to: ", to)
  }
  invisible(to)
}

# Numeric arithmetic avoids the signed overflow of bitwShiftL for TIFF LONGs.
.stamp_uint <- function(bytes, pos, size, little = TRUE) {
  if (length(pos) != 1L || !is.finite(pos) || pos < 1 ||
      pos + size - 1 > length(bytes)) return(NA_real_)
  powers <- if (little) 0:(size - 1L) else (size - 1L):0
  sum(as.integer(bytes[seq.int(pos, length.out = size)]) * 256^powers)
}

.stamp_tag <- function(tiff, offset, tag, little) {
  count <- .stamp_uint(tiff, offset, 2L, little)
  if (is.na(count) || offset + 2 + 12 * count - 1 > length(tiff)) return(NULL)
  for (i in seq_len(count)) {
    pos <- offset + 2 + (i - 1) * 12
    if (.stamp_uint(tiff, pos, 2L, little) == tag) {
      return(list(type = .stamp_uint(tiff, pos + 2, 2L, little),
        count = .stamp_uint(tiff, pos + 4, 4L, little),
        value = .stamp_uint(tiff, pos + 8, 4L, little), pos = pos + 8))
    }
  }
  NULL
}

.stamp_ascii_tag <- function(tiff, offset, tag, little) {
  entry <- .stamp_tag(tiff, offset, tag, little)
  if (is.null(entry) || entry$type != 2 || entry$count < 1) return("")
  start <- if (entry$count <= 4) entry$pos else entry$value + 1
  end <- start + entry$count - 1
  if (start < 1 || end > length(tiff)) return("")
  value <- tiff[seq.int(start, end)]
  value <- value[value != as.raw(0)]
  tryCatch(trimws(rawToChar(value)), error = function(e) "")
}

.stamp_app1 <- function(bytes) {
  n <- length(bytes)
  if (n < 4 || !identical(bytes[1:2], as.raw(c(255, 216)))) return(NULL)
  i <- 3L
  while (i + 3 <= n) {
    if (bytes[i] != as.raw(255)) break
    marker <- as.integer(bytes[i + 1L])
    if (marker == 255) { i <- i + 1L; next }
    if (marker %in% c(218L, 217L)) break
    if (marker == 1L || marker %in% 208:215) { i <- i + 2L; next }
    size <- .stamp_uint(bytes, i + 2L, 2L, little = FALSE)
    end <- i + size + 1
    if (is.na(size) || size < 2 || end > n) break
    if (marker == 225L && size >= 8 &&
        identical(bytes[(i + 4):(i + 9)], as.raw(c(69, 120, 105, 102, 0, 0)))) {
      return(bytes[seq.int(i, end)])
    }
    i <- end + 1
  }
  NULL
}

.stamp_exif <- function(path) {
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  app1 <- .stamp_app1(bytes)
  result <- list(dt = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
                 date_source = "none", orientation = 1L, app1 = app1)
  if (is.null(app1) || length(app1) < 18) return(result)
  tiff <- app1[11:length(app1)]
  little <- identical(tiff[1:2], charToRaw("II"))
  if (!little && !identical(tiff[1:2], charToRaw("MM"))) return(result)
  if (.stamp_uint(tiff, 3L, 2L, little) != 42) return(result)
  ifd0 <- .stamp_uint(tiff, 5L, 4L, little) + 1
  orientation <- .stamp_tag(tiff, ifd0, 274L, little)
  if (!is.null(orientation) && orientation$type == 3 && orientation$count == 1) {
    value <- .stamp_uint(tiff, orientation$pos, 2L, little)
    if (value %in% 1:8) result$orientation <- as.integer(value)
  }
  exif <- .stamp_tag(tiff, ifd0, 34665L, little)
  dates <- character()
  if (!is.null(exif) && exif$type == 4 && exif$count == 1) {
    dates <- c(DateTimeOriginal = .stamp_ascii_tag(tiff, exif$value + 1, 36867L, little),
               CreateDate = .stamp_ascii_tag(tiff, exif$value + 1, 36868L, little))
  }
  dates <- c(dates, DateTime = .stamp_ascii_tag(tiff, ifd0, 306L, little))
  for (field in names(dates)) {
    value <- dates[[field]]
    if (!grepl("^[0-9]{4}:[0-9]{2}:[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$", value)) next
    dt <- suppressWarnings(tryCatch(as.POSIXct(value, format = "%Y:%m:%d %H:%M:%S", tz = "UTC"),
                                    error = function(e) result$dt))
    if (!is.na(dt) && format(dt, "%Y:%m:%d %H:%M:%S") == value) {
      result$dt <- dt
      result$date_source <- paste0("exif:", field)
      break
    }
  }
  result
}

.stamp_orient <- function(img, orientation) {
  h <- dim(img)[1L]
  w <- dim(img)[2L]
  switch(as.character(orientation),
    "2" = img[, w:1L, , drop = FALSE],
    "3" = img[h:1L, w:1L, , drop = FALSE],
    "4" = img[h:1L, , , drop = FALSE],
    "5" = aperm(img, c(2L, 1L, 3L)),
    "6" = aperm(img[h:1L, , , drop = FALSE], c(2L, 1L, 3L)),
    "7" = aperm(img[h:1L, w:1L, , drop = FALSE], c(2L, 1L, 3L)),
    "8" = aperm(img[, w:1L, , drop = FALSE], c(2L, 1L, 3L)),
    img)
}

.stamp_reset_orientation <- function(app1) {
  if (is.null(app1) || length(app1) < 18) return(app1)
  tiff <- app1[11:length(app1)]
  little <- identical(tiff[1:2], charToRaw("II"))
  if (!little && !identical(tiff[1:2], charToRaw("MM"))) return(app1)
  if (.stamp_uint(tiff, 3L, 2L, little) != 42) return(app1)
  tag <- .stamp_tag(tiff, .stamp_uint(tiff, 5L, 4L, little) + 1, 274L, little)
  if (!is.null(tag) && tag$type == 3 && tag$count == 1) {
    app1[(10 + tag$pos):(13 + tag$pos)] <-
      as.raw(if (little) c(1, 0, 0, 0) else c(0, 1, 0, 0))
  }
  app1
}

.stamp_write <- function(infile, outfile, label, orientation, app1) {
  img <- jpeg::readJPEG(infile)
  dims <- dim(img)
  if (length(dims) == 2L) img <- array(rep(img, 3L), dim = c(dims, 3L))
  if (length(dim(img)) != 3L || dim(img)[3L] != 3L) {
    stop("Unsupported JPEG colour model (such as CMYK); original retained.")
  }
  img <- .stamp_orient(img, orientation)
  h <- dim(img)[1L]
  w <- dim(img)[2L]
  tmp <- tempfile("plotphotos_stamp_", fileext = ".jpg")
  device <- NA_integer_
  on.exit({
    if (!is.na(device) && device %in% grDevices::dev.list()) {
      try(grDevices::dev.off(device), silent = TRUE)
    }
    unlink(tmp)
  }, add = TRUE)
  opened <- FALSE
  if (isTRUE(capabilities("cairo"))) {
    opened <- tryCatch({
      grDevices::jpeg(tmp, width = w, height = h, units = "px", quality = 75,
                      pointsize = 12, type = "cairo")
      TRUE
    }, error = function(e) FALSE)
  }
  if (!opened) grDevices::jpeg(tmp, width = w, height = h, units = "px",
                               quality = 75, pointsize = 12)
  device <- grDevices::dev.cur()
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, w), ylim = c(0, h), asp = 1)
  graphics::rasterImage(grDevices::as.raster(img), 0, 0, w, h, interpolate = FALSE)
  long <- max(w, h)
  margin <- min(max(10, round(long * 0.012)), floor(min(w, h) / 10))
  cex <- max(18, round(long * 0.032)) / 12
  cex <- cex * min(1, (w - 2 * margin) / graphics::strwidth(label, cex = cex),
                  (h - 2 * margin) / graphics::strheight(label, cex = cex))
  halo <- max(1L, as.integer(round(long * 0.0035)))
  for (dx in seq.int(-halo, halo)) {
    for (dy in seq.int(-halo, halo)) {
      if ((dx != 0 || dy != 0) && dx * dx + dy * dy <= halo * halo) {
        graphics::text(w - margin + dx, margin + dy, label,
                       adj = c(1, 0), cex = cex, col = "white", xpd = NA)
      }
    }
  }
  graphics::text(w - margin, margin, label, adj = c(1, 0), cex = cex,
                 col = "black", xpd = NA)
  grDevices::dev.off(device)
  device <- NA_integer_
  if (!is.null(app1)) {
    bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
    app1 <- .stamp_reset_orientation(app1)
    insert <- 2L
    if (length(bytes) >= 6 && identical(bytes[3:4], as.raw(c(255, 224)))) {
      end <- .stamp_uint(bytes, 5L, 2L, little = FALSE) + 4
      if (!is.na(end) && end >= 6 && end < length(bytes)) insert <- end
    }
    writeBin(c(bytes[seq_len(insert)], app1, bytes[seq.int(insert + 1L, length(bytes))]), tmp)
  }
  .stamp_copy(tmp, outfile)
  suppressWarnings(Sys.setFileTime(outfile, file.info(infile)$mtime))
  invisible(outfile)
}
