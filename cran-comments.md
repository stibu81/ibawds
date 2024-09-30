## Summary

Checks on my system (Ubuntu 24.04.1) show no issues at all.
The checks on rhub & win-builder did not show any issues that seem problematic
for a realease on CRAN. During the last submission to CRAN, the following
note appeared (Fedora only):

* checking data for non-ASCII characters ... NOTE
  Note: found 1 marked UTF-8 string

I'm not sure what triggers this. Some of the datasets included in
the package contain Umlauts in German labels, which might be the reason
for the note. Since this note has never appeared in any of my GitHub Actions,
win builder and rhub, I assume that it is not a relevant issue.

