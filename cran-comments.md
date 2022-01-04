## Summary

My submission has been rejected due to a warning and two notes. I managed to
fix the warning, but I cannot fix the notes.

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

