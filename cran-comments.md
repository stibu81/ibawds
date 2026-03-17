## Issues on CRAN

* noSuggest: R CMD check fails if suggested packages are not installed.
  This release addresses this issue by making deldir mandatory and by
  skipping some tests if suggested packages are missing.
* Failing tests on r-devel-linux-x86_64-fedora-clang: This seems to be
  an issue due to CRAN not being able to install nanoparquet. But this 
  would also be solved by this release as the failing tests would be
  skipped should the problem persist. 

## Summary

* Checks on my system (R 4.5.3, Ubuntu 24.04.4) show no issues at all.
* Checks on rhub & win-builder show no issues at all.
* Issues on https://cloud.r-project.org/web/checks/check_results_ibawds.html
  are addressed by this release.