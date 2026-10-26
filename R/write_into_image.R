#' Resize JPEG copies and annotate capture dates
#'
#' Create JPEG copies with a date in the bottom-right corner. Optionally resize larger photographs before annotation. Original inputs remain unchanged, non-JPEG files are copied without alteration, and the relative folder structure is retained.
#'
#' @param input_folder A single input folder, scanned recursively, including hidden files and folders.
#' @param output_folder A single folder for output copies and \code{processing_log.csv}. It must differ from the input folder and must not contain it. A nested output folder is allowed and is excluded from input traversal.
#' @param date_format A single non-empty format string accepted by \code{\link[base:strptime]{strftime}}, for example \code{"%Y-%m-%d"} or \code{"%d.%m.%Y %H:%M"}. The default is \code{"%Y-%m-%d"}.
#' @param resize A single logical value. If \code{TRUE}, reduce images whose longer side exceeds \code{long_side} before adding the date. Smaller images are never enlarged. If \code{FALSE}, retain the pixel dimensions after applying EXIF orientation.
#' @param long_side A positive whole number giving the maximum longer-side length in pixels when resizing. Defaults to \code{2000}. Aspect ratio is retained, subject to whole-pixel rounding.
#' @param quality A whole number from 1 to 100 controlling JPEG encoding quality. Defaults to \code{80}, including when resizing is disabled. This is an encoder setting, not a measured percentage of preserved visual quality.
#'
#' @details
#' The date is selected from the first valid EXIF \code{DateTimeOriginal},
#' \code{DateTimeDigitized} (also called \code{CreateDate}), or \code{DateTime}
#' tag, in that order. The recorded EXIF clock time is retained without timezone
#' conversion; EXIF offset-time tags are not interpreted. If no EXIF date is valid,
#' file modification time is used, then file creation/change time as a last
#' fallback. Filesystem times are formatted in the current local timezone.
#' They are not guaranteed capture dates. The log records the selected source.
#' If no time is available, the text is \code{"unknown date"}.
#'
#' EXIF orientation is applied before resizing. Larger images are resized with
#' \code{magick::image_resize()} and the Lanczos filter. Black date text with a
#' white outline is drawn in the bottom-right corner at the final resolution.
#' The image is then JPEG-encoded once at the requested quality. This operation
#' is lossy even when \code{resize = FALSE}; retain your original photographs.
#' No external ExifTool or ImageMagick command-line program is called by this
#' function, although the \pkg{magick} R package uses ImageMagick libraries.
#'
#' The original EXIF APP1 segment is reinserted on a best-effort basis.
#' Retained EXIF may include GPS locations; output files are not anonymized.
#' Its orientation tag is reset to normal, and existing standard width and height
#' tags are updated. Other metadata segments, including ICC colour profiles and
#' XMP, are not retained. Embedded EXIF thumbnails are not rebuilt and may show
#' the original, unannotated image. Exact colour-profile preservation is not
#' provided. CMYK JPEGs are copied unchanged to the failure category instead of
#' being rendered with an unsupported colour model.
#'
#' @section File safety and repeated runs:
#' Input originals are never moved or overwritten. Non-JPEG files are copied
#' unchanged. Relative subfolders are retained unless a reserved name or an existing
#' file blocks a directory; affected directories then receive an alternate name.
#' Existing destination files receive numeric suffixes, never silent replacement.
#' Symbolic-link cycles are avoided, links outside the input tree are skipped with
#' a warning, and output subfolder links cannot redirect writes outside the output
#' root. Each encountered physical file path is processed once per run.
#'
#' JPEGs that cannot be decoded, annotated, or written are copied unchanged into
#' \code{unprocessed} under the output root, preserving their relative path. That
#' folder is created only when needed. If copying the original also fails, the row
#' has status \code{failed}, and a warning is issued. Newly created, empty folders
#' are cleaned up after copy failure.
#'
#' The root filename \code{processing_log.csv} is reserved for the cumulative log.
#' An input file with that name receives a suffix; an input folder with that name
#' is mapped to \code{processing_log_input}. An input folder called
#' \code{unprocessed} is mapped to \code{unprocessed_input} so that the failure
#' category is not used for successfully processed inputs. Alternate directory
#' names may share a directory with a pre-existing input name; filename suffixes
#' still prevent overwriting, and the log records each actual destination.
#'
#' Repeated calls create additional output copies; this workflow does not skip
#' previously stamped sources. Use a fresh output folder when changing settings.
#' The log is staged before replacement and the previous log is retained until
#' replacement succeeds. Do not run concurrent writers to the same output tree.
#' An empty input produces a zero-row result without creating output directories.
#'
#' @return Invisibly returns a data frame for the current run. The CSV log accumulates
#' all runs. Columns are:
#' \describe{
#' \item{file}{Source path.}
#' \item{relative}{Source path relative to the input root.}
#' \item{output_file}{Actual output path, or \code{NA} if no copy could be written.}
#' \item{status}{\code{stamped} for a processed JPEG, \code{copied} for another
#' file type copied unchanged, \code{unprocessed} for a failed JPEG whose original
#' was retained in the failure category, or \code{failed} if no output copy exists.}
#' \item{stamp_text}{Text selected for annotation; empty for non-JPEG files.}
#' \item{date_source}{The selected EXIF tag, \code{file:mtime}, \code{file:ctime},
#' or \code{none}; empty for non-JPEG files.}
#' \item{error}{The failure reason, or an empty string.}
#' \item{processed_at}{Local processing time, formatted as year-month-day hour:minute:second.}
#' }
#'
#' @seealso \code{\link{copy_photos_to_plots}}, \code{\link[base:strptime]{strftime}}
#'
#' @examples
#' if (capabilities("jpeg")) {
#'   (function() {
#'     root <- tempfile("plotphotos-stamp-")
#'     dir.create(root)
#'     on.exit(unlink(root, recursive = TRUE))
#'     input <- file.path(root, "input")
#'     dir.create(input)
#'     image <- file.path(input, "example.jpg")
#'     jpeg::writeJPEG(array(0.65, dim = c(120, 200, 3)), image)
#'     Sys.setFileTime(image, as.POSIXct("2024-04-22 12:00:00", tz = "UTC"))
#'     log <- write_into_image(
#'       input, file.path(root, "dated"), date_format = "%d.%m.%Y",
#'       resize = TRUE, long_side = 160, quality = 80
#'     )
#'     log[, c("status", "stamp_text", "date_source")]
#'   })()
#' }
#' @author Josef Brůna (author and maintainer). OpenAI's ChatGPT provided extensive assistance with code, tests, and documentation as a development tool, not as a co-author. See \code{citation("plotphotos")} for the package citation and separate AI-tool acknowledgement.
#' @encoding UTF-8
#' @export
write_into_image <- function(input_folder, output_folder,
                             date_format = "%Y-%m-%d",
                             resize = TRUE, long_side = 2000, quality = 80) {
  for (arg in c("input_folder", "output_folder", "date_format")) {
    value <- get(arg)
    if (!is.character(value) || length(value) != 1L ||
        is.na(value) || !nzchar(value)) {
      stop(arg, " must be a single non-empty character string.", call. = FALSE)
    }
  }
  if (!is.logical(resize) || length(resize) != 1L || is.na(resize)) {
    stop("resize must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(long_side) || length(long_side) != 1L ||
      !is.finite(long_side) || long_side < 1 ||
      long_side > .Machine$integer.max || long_side != floor(long_side)) {
    stop("long_side must be a positive whole number of pixels.", call. = FALSE)
  }
  if (!is.numeric(quality) || length(quality) != 1L || !is.finite(quality) ||
      quality < 1 || quality > 100 || quality != floor(quality)) {
    stop("quality must be a whole number from 1 to 100 (for example, 80).", call. = FALSE)
  }
  if (!dir.exists(input_folder)) stop("Input folder does not exist: ", input_folder)
  input_folder <- normalizePath(input_folder, winslash = "/", mustWork = TRUE)
  output_folder <- canonical_path(output_folder)
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
  files <- scan_input_files(input_folder, exclude_trees = output_folder)
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
  if (!path_within(canonical_path(log_csv), output_folder)) {
    stop("processing_log.csv resolves outside output_folder.", call. = FALSE)
  }
  if (file.exists(log_csv)) {
    old <- tryCatch(read_processing_log(log_csv),
                    error = function(e) stop("Cannot read existing processing log: ",
                      conditionMessage(e), call. = FALSE))
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
    target_relative <- sub("^unprocessed/", "unprocessed_input/", target_relative,
                           ignore.case = .Platform$OS.type == "windows")
    is_jpeg <- grepl("[.](jpg|jpeg)$", src, ignore.case = TRUE)
    status <- if (is_jpeg) "stamped" else "copied"
    stamp_text <- date_source <- error <- ""
    output_file <- NA_character_
    tryCatch({
      dest <- .stamp_unique_path(file.path(output_folder, target_relative),
                                 reserved, root = output_folder)
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
        .stamp_write(src, dest, stamp_text, ex$orientation, ex$app1,
                     resize = resize, long_side = long_side, quality = quality)
      } else {
        .stamp_copy(src, dest)
      }
      output_file <- dest
    }, error = function(e) {
      error <<- conditionMessage(e)
      status <<- "failed"
      if (is_jpeg) {
        tryCatch({
          fallback <- .stamp_unique_path(file.path(output_folder, "unprocessed", relative),
                                         root = output_folder)
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
  write_processing_log(rbind(old, result), log_csv)
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
  resolved <- canonical_path(dirname(path))
  if (!path_within(resolved, canonical_path(root))) {
    stop("Output subfolder resolves outside output_folder: ", dirname(path), call. = FALSE)
  }
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
  new_dirs <- character()
  directory <- parent
  while (!dir.exists(directory)) {
    new_dirs <- c(new_dirs, directory)
    above <- dirname(directory)
    if (identical(above, directory)) break
    directory <- above
  }
  ok <- FALSE
  on.exit({
    if (!ok) {
      for (directory in new_dirs) {
        if (dir.exists(directory) &&
            !length(list.files(directory, all.files = TRUE, no.. = TRUE))) {
          unlink(directory, recursive = TRUE)
        }
      }
    }
  }, add = TRUE)
  if (!dir.exists(parent) &&
      !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create folder: ", parent)
  }
  if (file.exists(to)) stop("Destination already exists: ", to)
  ok <- isTRUE(tryCatch(
    suppressWarnings(file.copy(from, to, overwrite = FALSE, copy.date = TRUE)),
    error = function(e) FALSE
  ))
  if (!ok) {
    if (file.exists(to)) unlink(to)
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

.stamp_read_app1 <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  if (!identical(readBin(con, "raw", n = 2L), as.raw(c(255, 216)))) return(NULL)
  repeat {
    prefix <- readBin(con, "raw", n = 1L)
    if (length(prefix) != 1L || prefix != as.raw(255)) return(NULL)
    marker <- readBin(con, "raw", n = 1L)
    while (length(marker) == 1L && marker == as.raw(255)) {
      marker <- readBin(con, "raw", n = 1L)
    }
    if (!length(marker)) return(NULL)
    code <- as.integer(marker)
    if (code %in% c(218L, 217L)) return(NULL)
    if (code == 1L || code %in% 208:215) next
    size_bytes <- readBin(con, "raw", n = 2L)
    if (length(size_bytes) != 2L) return(NULL)
    size <- .stamp_uint(size_bytes, 1L, 2L, little = FALSE)
    if (size < 2L) return(NULL)
    payload <- readBin(con, "raw", n = size - 2L)
    if (length(payload) != size - 2L) return(NULL)
    if (code == 225L && length(payload) >= 6L &&
        identical(payload[1:6], as.raw(c(69, 120, 105, 102, 0, 0)))) {
      return(c(prefix, marker, size_bytes, payload))
    }
  }
}

.stamp_exif <- function(path) {
  app1 <- .stamp_read_app1(path)
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

.stamp_resize <- function(img, long_side) {
  original <- magick::image_read(img)
  on.exit(magick::image_destroy(original), add = TRUE)
  resized <- magick::image_resize(original,
    geometry = sprintf("%dx%d>", as.integer(long_side), as.integer(long_side)),
    filter = "Lanczos")
  on.exit(magick::image_destroy(resized), add = TRUE)
  grDevices::as.raster(resized)
}

.stamp_reset_orientation <- function(app1, width = NULL, height = NULL) {
  if (is.null(app1) || length(app1) < 18) return(app1)
  tiff <- app1[11:length(app1)]
  little <- identical(tiff[1:2], charToRaw("II"))
  if (!little && !identical(tiff[1:2], charToRaw("MM"))) return(app1)
  if (.stamp_uint(tiff, 3L, 2L, little) != 42) return(app1)
  ifd0 <- .stamp_uint(tiff, 5L, 4L, little) + 1
  patch <- function(offset, id, value) {
    if (is.null(value)) return(invisible(NULL))
    tag <- .stamp_tag(tiff, offset, id, little)
    if (is.null(tag) || !tag$type %in% c(3, 4) || tag$count != 1) return(invisible(NULL))
    size <- if (tag$type == 3) 2L else 4L
    if (!is.finite(value) || value < 0 || value >= 256^size) return(invisible(NULL))
    powers <- if (little) 0:(size - 1L) else (size - 1L):0
    bytes <- as.raw(floor(value / 256^powers) %% 256)
    app1[(10 + tag$pos):(13 + tag$pos)] <<- c(bytes, raw(4L - size))
    invisible(NULL)
  }
  patch(ifd0, 274L, 1L) # Orientation
  patch(ifd0, 256L, width) # ImageWidth
  patch(ifd0, 257L, height) # ImageLength
  exif <- .stamp_tag(tiff, ifd0, 34665L, little)
  if (!is.null(exif) && exif$type == 4 && exif$count == 1) {
    patch(exif$value + 1, 40962L, width) # PixelXDimension
    patch(exif$value + 1, 40963L, height) # PixelYDimension
  }
  app1
}

.stamp_write <- function(infile, outfile, label, orientation, app1,
                          resize = TRUE, long_side = 2000, quality = 80) {
  img <- jpeg::readJPEG(infile)
  dims <- dim(img)
  if (length(dims) == 2L) img <- array(rep(img, 3L), dim = c(dims, 3L))
  if (length(dim(img)) != 3L || dim(img)[3L] != 3L) {
    stop("Unsupported JPEG colour model (such as CMYK); original retained.")
  }
  img <- .stamp_orient(img, orientation)
  bitmap <- if (resize && max(dim(img)[1:2]) > long_side) {
    .stamp_resize(img, long_side)
  } else {
    grDevices::as.raster(img)
  }
  h <- nrow(bitmap)
  w <- ncol(bitmap)
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
      grDevices::jpeg(tmp, width = w, height = h, units = "px", quality = quality,
                      pointsize = 12, type = "cairo")
      TRUE
    }, error = function(e) FALSE)
  }
  if (!opened) grDevices::jpeg(tmp, width = w, height = h, units = "px",
                               quality = quality, pointsize = 12)
  device <- grDevices::dev.cur()
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, w), ylim = c(0, h), asp = 1)
  graphics::rasterImage(bitmap, 0, 0, w, h, interpolate = FALSE)
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
    app1 <- .stamp_reset_orientation(app1, width = w, height = h)
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
