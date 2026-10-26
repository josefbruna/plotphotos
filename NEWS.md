# plotphotos 0.3.1

Help-index cleanup and improved diagnostics; runtime validation is still pending.

* Keep exactly two help topics, one per public function. Remove the redundant
  package overview and its two aliases. Internal helpers remain undocumented.
* Preserve Josef Brůna's authorship and the extensive ChatGPT acknowledgement in
  the README, both function pages, and the separate package/tool citations.
* Test installed help retrieval, text conversion, dynamic HTML conversion, and
  the real R help request handler rather than only static source rendering.
* Check the exact exported functions and help topics so duplicate topics cannot
  silently reappear. Add a fixed Linux R 4.3.3 CI job.
* Explain that working Description/NEWS pages do not validate function help and
  that generating static HTML alone cannot establish a fix for a server error.
* No changes to photo sorting, resizing, stamping, or other processing logic.
* Remove the obsolete man/plotphotos-package.Rd file from existing checkouts.

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
