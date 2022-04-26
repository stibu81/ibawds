## Summary

The checks on rhub did not show any issues that seem problematic for a realease
on CRAN. The notes that I comment below have appeared during the last submission
to CRAN (1 & 2) or on some systems on rhub (2-4).

## Note 1

Check: Rd cross-references

Result: NOTE

Undeclared package ‘knitr’ in Rd xrefs 

I don't understand this one. knitr is suggested and the note appears only
on r-devel-linux-x86_64-fedora-clang. This might therefore be a false
positive.

## Note 2

Check: data for non-ASCII characters

Result: NOTE

Note: found 1 marked UTF-8 string 

I assume this is due to the fact that some of the labels in one of the
datasets contains umlauts. The data is used for a German language Data Science
course, which is the reason for the German labels. I have checked with 
Encoding() that the entries with umlauts are correctly marked as UTF-8.


## Note 3

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
This is due to a test that renders a latex file. This test should be skipped
on CRAN such that I dont expect this note to appear.


## Note 4

* checking CRAN incoming feasibility ... NOTE
Found the following (possibly) invalid URLs:
  URL: https://archive-beta.ics.uci.edu/ml/datasets/wine+quality
    From: man/wine_quality.Rd
    Status: Error
    Message: libcurl error code 60:
      	SSL certificate problem: unable to get local issuer certificate
      	(Status without verification: OK)
      	
I have checked the URL works. However, the browser shows a 404 very briefly
and then the page. This seems to be an issue of the page that I cannot fix.
The note only appears on Fedora Linux, R-devel, clang, gfortran.
