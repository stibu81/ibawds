# Check Spelling in the Evaluation of the Papers or the Slide Decks

Evaluation of the student papers, lecture slides and some exercises are
all done in the form of Rmd files. These function find all the relevant
Rmd-files in a directory and check the spelling using the package
[`spelling`](https://docs.ropensci.org/spelling//reference/spell_check_files.html).

## Usage

``` r
spell_check_evaluation(path = ".", students = NULL, use_wordlist = TRUE)

spell_check_slides(path = ".", use_wordlist = TRUE)
```

## Arguments

- path:

  path to the top level directory of the evaluations for
  `spell_check_evaluation()` or the top level of a lecture for
  `spell_check_slides()`

- students:

  an optional character vector with student names. If given, only the
  evaluation for these students will be checked.

- use_wordlist:

  should a list of words be excluded from the spell check? The package
  contains separate word lists for evaluations and slides/exercises with
  words that have typically appeared in these documents in the past.
  When spell checking the paper evaluations, the names of the students
  will always be excluded from spell check, even if `use_wordlist` is
  `FALSE`.

## Details

`spell_check_evaluation()` finds Rmd-files with evaluations in
subfolders starting from the current working directory or the directory
given by `path`. The file names must be of the form
"Beurteilung_Student.Rmd", where "Student" must be replaced by the
student's name. By default, words contained in a wordlist that is part
of the package as well as all the students' names are excluded from the
spell check, but this can be turned off by setting
`use_wordlist = FALSE`. (Note that the students' names will still be
excluded.)

`spell_check_slides()` finds Rmd-files with evaluations in subfolders
starting from the current working directory or the directory given by
`path`. In order to exclude a file from the spell check, it must be
marked with the html-comment

    <!-- nospellcheck -->

The comment must appear either in the first line of the file or in the
first two lines after the end of the YAML-header.

By default, words contained in a wordlist that is part of the package
are excluded from the spell check, but this can be turned off by setting
`use_wordlist = FALSE`.
