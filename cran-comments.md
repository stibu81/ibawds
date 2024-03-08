## Summary

Checks on my system (Ubuntu 22.04) show no issues at all.
The checks on rhub win-builder did not show any issues that seem problemati
for a realease on CRAN. The notes that I comment below have appeared during the last submission
to CRAN (1 & 2) or on some systems on rhub (2-4).

## Note 1

* checking HTML version of manual ... NOTE
Skipping checking math rendering: package 'V8' unavailable

I don't understand this one. My package does not depend on V8.

## Note 2

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
  
Not sure what this means. Only appears in Windows on rhub, but not on win-builder.


## Note 3

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
This likely is due to a test that renders a latex file. This test should be
skipped on CRAN such that I dont expect this note to appear.


## Note 4

Examples with CPU (user + system) or elapsed time > 5s
user system elapsed
cluster_with_centers 2.926 0.086 11.545

Only on Fedora and Ubuntu. This example creates a plot and the rendering seems
to take time.
