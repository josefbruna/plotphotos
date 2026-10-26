# plotphotos 0.3.1 - draft, not yet submitted

This file is excluded from the built package. Update it with real results before
using it in a CRAN submission. Do not submit this unchecked draft as a clean check.

## Purpose and contribution

The package organizes field photographs and accompanying files by plot locations,
folder IDs, or filename IDs, while retaining files without GPS. A second workflow
annotates dated, optionally resized JPEG copies without overwriting originals.
Both workflows keep explicit processing logs. The package code contains no network/API calls or AI service integration. Tests and runnable examples use temporary files.

## Authorship and AI assistance

Josef Brůna is the author and maintainer. OpenAI's ChatGPT provided extensive
assistance with code development, debugging, tests, and documentation. This is
explicitly acknowledged in the README, both function help pages, and citation information;
OpenAI is not listed as a package author. Original Institute of Botany copyright
and MIT licensing have been retained. Specific model snapshots were not recorded.
The maintainer must review and approve the final release.

## Validation status for this revision

- Source and documentation review: performed; help index and regression checks updated; this is not an R execution result.
- R runtime tests, examples, installed-help rendering, and R CMD check: NOT RUN.
- Windows, macOS, Linux R-release, R-devel, R-oldrel, and R 4.3.3 CI: configured, NOT RUN.
- PDF manual build: NOT RUN.
- Real-photo batch on the maintainer's Windows installation: NOT RUN.
- CRAN name availability, incoming checks, URL checks, win-builder: NOT VERIFIED.
- CRAN submission: NOT MADE.

The editing container had no R or Rscript, and could not download an R installation.
Earlier test counts reported for 0.2.x are not results for 0.3.1 and are not repeated
as evidence. Replace the list above with actual versions/platforms and check
results once the included checks have been run. Explain remaining NOTEs accurately.

## Unresolved installed-help report

Function help returned "Internal Server Error" on the maintainer's machine,
while Description and NEWS worked. The extra package topic is now removed, but
the root cause has not been established. Do not submit until the installed-help
checks pass and the reported failure is resolved. The separate read-only
help diagnostic compares the affected session with a fresh Rscript process.

## Final maintainer checks

Confirm josef.bruna@ibot.cas.cz is the desired public maintainer address; confirm
copyright authority; inspect both function help pages after a fresh installation;
review package and AI citations; review the complete logs from R CMD check
--as-cran; and build the submission tarball with R CMD build.
