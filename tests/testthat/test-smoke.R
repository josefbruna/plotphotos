test_root <- function() {
  root <- tempfile("plotphotos-")
  dir.create(root)
  root
}

test_file <- function(path, text = "original") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(text, path)
  path
}

test_plots <- function(root) {
  plots <- sf::st_as_sf(data.frame(locality_id = c("P1", "P2", "P3"),
    label = c("A", "B", "C"), x = c(500000, 500020, 501000), y = 5540000),
    coords = c("x", "y"), crs = 32633)
  path <- file.path(root, "plots.gpkg")
  sf::st_write(plots, path, quiet = TRUE)
  path
}

test_that("every input file is routed, including missing EXIF rows and distant GPS", {
  root <- test_root()
  input <- file.path(root, "input")
  output <- file.path(input, "sorted")
  plots <- test_plots(root)
  relative <- c("correct.jpg", "multiple.jpg", "far.jpg", "beyond.jpg",
    "P1/P3_override.jpg", "P2_photo.JPEG", "unmatched.jpg", "notes.TXT",
    "LICENSE", ".keep", "sorted_backup/retained.txt")
  files <- vapply(file.path(input, relative), test_file, character(1))
  hashes <- tools::md5sum(files)
  xy <- sf::st_as_sf(data.frame(x = c(501000, 500010, 501060, 501400),
    y = 5540000), coords = c("x", "y"), crs = 32633)
  ll <- sf::st_coordinates(sf::st_transform(xy, 4326))
  metadata <- data.frame(SourceFile = files[1:4],
    GPSLongitude = ll[, 1], GPSLatitude = ll[, 2])
  local_mocked_bindings(read_exif = function(...) metadata[4:1, ], .package = "exifr")

  log <- copy_photos_to_plots(input, output, plots, name_field = "label", verbose = FALSE)
  expect_equal(nrow(log), length(files))
  expect_true(all(log$copy_ok))
  expected <- c("correct_within_threshold", "multiple_within_threshold",
    "far_over_threshold", "gps_over_max_distance", "no_gps_folder",
    "no_gps_named", rep("no_gps", 5))
  expect_equal(log$status[match(files, log$source_file)], expected)
  destinations <- log$new_file[match(files, log$source_file)]
  expect_equal(basename(destinations[c(1, 3, 4, 5, 6)]),
               c("C.jpg", "C.jpg", "beyond.jpg", "A.jpg", "B.jpg"))
  expect_equal(basename(destinations[8:10]), c("notes.TXT", "LICENSE", ".keep"))
  expect_equal(unname(tools::md5sum(destinations)), unname(hashes))
  expect_equal(tools::md5sum(files), hashes)

  again <- copy_photos_to_plots(c(input, file.path(input, "P1")), output,
    plots, name_field = "label", verbose = FALSE)
  expect_equal(nrow(again), nrow(log))
  expect_equal(length(list.files(output, recursive = TRUE, all.files = TRUE)), length(files) + 1L)
})

test_that("missing GPS columns and read errors copy files without unused folders", {
  root <- test_root()
  input <- file.path(root, "input")
  files <- vapply(file.path(input, c("P1_photo.jpg", "P2/image.jpg", "sidecar.txt")),
                 test_file, character(1))
  plots <- test_plots(root)
  local_mocked_bindings(read_exif = function(...) data.frame(SourceFile = files[1]),
                        .package = "exifr")
  output <- file.path(root, "missing_tags")
  log <- copy_photos_to_plots(input, output, plots, names = FALSE,
                             folders = FALSE, verbose = FALSE)
  expect_true(all(log$copy_ok))
  expect_true(all(log$status == "no_gps"))
  expect_equal(basename(list.dirs(output, recursive = FALSE)), "no_gps")
  expect_equal(as.numeric(file.info(log$new_file)$mtime),
               as.numeric(file.info(log$source_file)$mtime), tolerance = 1)

  local_mocked_bindings(read_exif = function(...) stop("metadata unavailable"),
                        .package = "exifr")
  output2 <- file.path(root, "read_error")
  expect_warning(log2 <- copy_photos_to_plots(input, output2, plots,
    names = FALSE, folders = FALSE, verbose = FALSE), "EXIF could not be read")
  expect_equal(nrow(log2), length(files))
  expect_true(all(log2$copy_ok))
  expect_equal(basename(list.dirs(output2, recursive = FALSE)), "no_gps")
})

test_that("invalid coordinates use no-GPS rules and deleted copies can be restored", {
  root <- test_root()
  input <- file.path(root, "input")
  files <- vapply(file.path(input, c("a.jpg", "b.jpg")), test_file, character(1))
  plots <- test_plots(root)
  local_mocked_bindings(read_exif = function(...) data.frame(SourceFile = files,
    GPSLatitude = c(120, 50), GPSLongitude = c(15, NA_real_)), .package = "exifr")
  output <- file.path(root, "output")
  log <- copy_photos_to_plots(input, output, plots, verbose = FALSE)
  expect_true(all(log$status == "no_gps"))
  unlink(log$new_file[1])
  again <- copy_photos_to_plots(input, output, plots, verbose = FALSE)
  expect_equal(nrow(again), 3L)
  expect_true(all(file.exists(log$new_file)))
  expect_error(copy_photos_to_plots(input, input, plots, verbose = FALSE), "Input roots")
  empty <- file.path(root, "empty")
  dir.create(empty)
  unused <- file.path(root, "unused")
  expect_equal(nrow(copy_photos_to_plots(empty, unused, plots, verbose = FALSE)), 0L)
  expect_false(dir.exists(unused))
})

test_that("stamping preserves source files, relative folders and the caller's device", {
  skip_if_not(capabilities("jpeg"))
  root <- test_root()
  input <- file.path(root, "input")
  dir.create(file.path(input, "nested"), recursive = TRUE)
  photo <- file.path(input, "nested", "photo.jpg")
  jpeg::writeJPEG(array(0.7, c(160, 240, 3)), photo)
  Sys.setFileTime(photo, as.POSIXct("2024-04-22 12:34:56", tz = "UTC"))
  note <- test_file(file.path(input, "nested", "notes.txt"), "keep me")
  corrupt <- test_file(file.path(input, "broken.jpg"), "not a JPEG")
  sources <- c(photo, note, corrupt)
  hashes <- tools::md5sum(sources)
  output <- file.path(input, "dated")
  grDevices::pdf(file.path(root, "existing_device.pdf"))
  device <- grDevices::dev.cur()
  log <- tryCatch({
    result <- suppressMessages(write_into_image(input, output, "%d.%m.%Y"))
    expect_equal(grDevices::dev.cur(), device)
    result
  }, finally = grDevices::dev.off(device))
  expect_equal(nrow(log), 3L)
  expect_equal(log$status[match(sources, log$file)], c("stamped", "copied", "unprocessed"))
  stamped <- log[log$file == photo, ]
  expect_equal(stamped$stamp_text, format(file.info(photo)$mtime, "%d.%m.%Y"))
  expect_equal(stamped$date_source, "file:mtime")
  expect_equal(dim(jpeg::readJPEG(stamped$output_file)), c(160L, 240L, 3L))
  expect_equal(tools::md5sum(sources), hashes)
  expect_equal(unname(tools::md5sum(log$output_file[match(c(note, corrupt), log$file)])),
               unname(hashes[2:3]))
  expect_true(file.exists(file.path(output, "unprocessed", "broken.jpg")))
  expect_true(file.exists(file.path(output, "nested", "notes.txt")))
  again <- suppressMessages(write_into_image(input, output))
  expect_equal(nrow(again), 3L)
  expect_false(any(again$output_file %in% log$output_file))
})

test_that("EXIF capture dates and displayed orientation survive stamping", {
  skip_if_not(capabilities("jpeg"))
  root <- test_root()
  input <- file.path(root, "input")
  dir.create(input)
  photo <- file.path(input, "rotated.jpg")
  jpeg::writeJPEG(array(0.65, c(120, 200, 3)), photo)
  uint <- function(x, size) as.raw(floor(x / 256^(seq_len(size) - 1L)) %% 256)
  entry <- function(tag, type, count, value) c(uint(tag, 2), uint(type, 2),
                                             uint(count, 4), uint(value, 4))
  tiff <- c(charToRaw("II"), uint(42, 2), uint(8, 4), uint(2, 2),
    entry(274, 3, 1, 6), entry(34665, 4, 1, 38), uint(0, 4),
    uint(1, 2), entry(36867, 2, 20, 56), uint(0, 4),
    charToRaw("2024:04:22 12:34:56"), as.raw(0))
  payload <- c(charToRaw("Exif"), as.raw(c(0, 0)), tiff)
  segment_size <- length(payload) + 2L
  app1 <- c(as.raw(c(255, 225)), rev(uint(segment_size, 2)), payload)
  bytes <- readBin(photo, "raw", n = file.info(photo)$size)
  writeBin(c(bytes[1:2], app1, bytes[-(1:2)]), photo)
  original_hash <- tools::md5sum(photo)
  output <- file.path(root, "output")
  log <- suppressMessages(write_into_image(input, output, "%d.%m.%Y %H:%M"))
  expect_equal(log$status, "stamped")
  expect_equal(log$stamp_text, "22.04.2024 12:34")
  expect_equal(log$date_source, "exif:DateTimeOriginal")
  expect_equal(dim(jpeg::readJPEG(log$output_file)), c(200L, 120L, 3L))
  metadata <- plotphotos:::.stamp_exif(log$output_file)
  expect_equal(metadata$orientation, 1L)
  expect_equal(format(metadata$dt, "%Y-%m-%d %H:%M:%S"), "2024-04-22 12:34:56")
  expect_equal(tools::md5sum(photo), original_hash)
  expect_false(dir.exists(file.path(output, "unprocessed")))
})

test_that("stamping handles empty folders and reserves its log without data loss", {
  root <- test_root()
  input <- file.path(root, "input")
  output <- file.path(root, "output")
  dir.create(input)
  expect_equal(nrow(suppressMessages(write_into_image(input, output))), 0L)
  expect_false(dir.exists(output))
  expect_error(write_into_image(input, input), "must be different")
  expect_error(write_into_image(input, root), "must not contain")
  source_log <- test_file(file.path(input, "processing_log.csv"), "original,input,log")
  log <- suppressMessages(write_into_image(input, output))
  expect_equal(log$status, "copied")
  expect_false(identical(basename(log$output_file), "processing_log.csv"))
  expect_equal(unname(tools::md5sum(log$output_file)), unname(tools::md5sum(source_log)))
  expect_true(file.exists(file.path(output, "processing_log.csv")))
  expect_false(dir.exists(file.path(output, "unprocessed")))
})

test_that("stamping keeps all files when existing outputs block directory paths", {
  root <- test_root()
  input <- file.path(root, "input")
  output <- file.path(root, "output")
  sources <- vapply(file.path(input, "group", c("notes.txt", "second.txt", "broken.jpg")),
                     test_file, character(1))
  blockers <- vapply(file.path(output, c("group", "unprocessed")), test_file, character(1))
  original_hashes <- tools::md5sum(c(sources, blockers))
  log <- suppressMessages(write_into_image(input, output))
  expect_equal(sort(log$status), c("copied", "copied", "unprocessed"))
  expect_true(all(file.exists(log$output_file)))
  expect_equal(length(unique(dirname(log$output_file[log$status == "copied"]))), 1L)
  expect_equal(tools::md5sum(c(sources, blockers)), original_hashes)
  expect_equal(unname(tools::md5sum(log$output_file)),
               unname(tools::md5sum(log$file)))
})
