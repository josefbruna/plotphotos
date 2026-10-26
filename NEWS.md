# plotphotos 0.3.0

CRAN-preparation release; not yet submitted or runtime-validated in the editing environment.

* Rewrite both function help pages with complete arguments, routing/status meanings,
  file-safety limitations, date/metadata behavior, and runnable temporary examples.
* Add package-level help, an explicit acknowledgement of extensive ChatGPT assistance
  under Josef Brůna's authorship, and separate package and AI-tool citations.
* Replace private-network examples with portable examples and document clean
  reinstall steps for an installed-help "Internal Server Error".
* Harden log replacement, protect input trees from custom log destinations, and
  prevent symbolic-link traversal cycles and writes through escaping output links.
* Reserve the stamping failure category separately from an input directory with
  the same name. Preserve originals on per-file failures.
* Sanitize Windows reserved filenames and shorten overly long output stems.
* Compute GPS distances with sf's spherical engine instead of assuming one UTM
  zone, restoring the caller's engine setting afterwards. Require POINT plots.
* Read EXIF headers without loading an entire JPEG solely for metadata extraction.
* Remove development-only dependencies; add compact multi-platform CI and
  regression coverage for installed help, path safety, and metadata edge cases.
* Keep both public function signatures and the 2000-pixel / quality-80 defaults.

# plotphotos 0.2.1

* Add optional Lanczos resizing, a 2000-pixel maximum long side, and JPEG quality 80.
* Update existing EXIF dimension tags after resizing or orientation changes.

# plotphotos 0.2.0

* Retain unmatched files without GPS, distant GPS photographs, and non-JPEG files.
* Create category directories only when used.
* Add a separate date-annotation workflow that preserves original input files.
