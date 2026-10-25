## Submission notes

This is an update of r4subpharma from 0.1.0 (on CRAN) to 0.2.0. It bridges the
'pharmaverse' clinical reporting stack and the R4SUB (Ready for Submission)
ecosystem, converting 'metacore' metadata and ADaM datasets into standardized
R4SUB evidence via 'r4subcore' so that submission readiness can be scored
without changing an existing pharmaverse pipeline. See NEWS.md for the changes
since the last CRAN release.

The imported package 'r4subcore' is available on CRAN. All suggested packages
('metacore', 'pharmaverseadam', 'r4subdata', 'r4subprofile', 'r4subscore',
'knitr', 'rmarkdown', 'testthat') are on CRAN. Examples and tests that use the
suggested packages are guarded with requireNamespace().

## Test environments

* local: Windows 11 x64, R 4.5.x
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are no reverse dependencies on CRAN at this time.
