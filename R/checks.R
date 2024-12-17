# nocov start

#' Check Spelling in the Evaluation of the Papers or the Slide Decks
#'
#' Evaluation of the student papers, lecture slides and some exercises are all
#' done in the form of Rmd files. These function find all the relevant
#' Rmd-files in a directory and check the spelling using the package
#' [`spelling`][spelling::spell_check_files()].
#'
#' @param path path to the top level directory of the evaluations for
#' `spell_check_evaluation()` or the top level of a lecture for
#' `spell_check_slides()`
#' @param students an optional character vector with student names. If given,
#'  only the evaluation for these students will be checked.
#' @param use_wordlist should a list of words be excluded from the spell
#'  check? The package contains separate word lists for evaluations and
#'  slides/exercises with words that have typically appeared in these documents
#'  in the past. When spell checking the paper evaluations, the names of the
#'  students will always be excluded from spell check, even if `use_wordlist`
#'  is `FALSE`.
#'
#' @details
#' `spell_check_evaluation()` finds Rmd-files with evaluations in subfolders
#' starting from the current working directory or the directory given by
#' `path`. The file names must be of the form "Beurteilung_Student.Rmd", where
#' "Student" must be replaced by the student's name. By default, words contained
#' in a wordlist that is part of the package as well as all the students' names
#' are excluded from the spell check, but this can be turned off by setting
#' `use_wordlist = FALSE`. (Note that the students' names will still be
#' excluded.)
#'
#' `spell_check_slides()` finds Rmd-files with evaluations in subfolders
#' starting from the current working directory or the directory given by
#' `path`. In order to exclude a file from the spell check, make sure it's first
#' line contains the term "nospellcheck", typically in the form of an
#' html-comment:
#' ```
#' <!-- nospellcheck -->
#' ```
#' By default, words contained in a wordlist that is part of the package are
#' excluded from the spell check, but this can be turned off by setting
#' `use_wordlist = FALSE`.
#'
#' @export

spell_check_evaluation <- function(path = ".",
                                   students = NULL,
                                   use_wordlist = TRUE) {

  rlang::check_installed("spelling")

  # find the evaluation files recursively, ignore the template
  eval_files <- list.files(path, "Beurteilung_.*\\.Rmd",
                           recursive = TRUE,
                           full.names = TRUE) %>%
    stringr::str_subset("Beurteilung_Template\\.Rmd", negate = TRUE)

  if (!is.null(students)) {
    eval_files <- eval_files %>%
      stringr::str_subset(
        paste0("Beurteilung_", students, ".Rmd", collapse = "|")
      )
  }

  # abort, if there are not files to check
  if (length(eval_files) == 0) {
    cli::cli_abort("No evaluation files found.")
  }

  # extract the names of the students and use them as the ignore list.
  ignore_list <- eval_files %>%
    stringr::str_extract("Beurteilung_(.*)\\.Rmd", group = 1)

  # add words from the wordlist to the ignore list, if requested
  if (use_wordlist) {
    ignore_list <- c(read_wordlist("evaluation"), ignore_list)
  }

  # run the spell check
  spelling::spell_check_files(eval_files, lang = "de_CH",
                              ignore = ignore_list)

}


#' @rdname spell_check_evaluation
#' @export

spell_check_slides <- function(path = ".",
                               use_wordlist = TRUE) {

  rlang::check_installed("spelling")

  rmd_files <- find_rmd_files(path, ignore_nospellcheck = TRUE)

  # ignore words from the wordlist if requested
  ignore_list <- if (use_wordlist) {
    read_wordlist("slides")
  } else {
    character(0)
  }

  # run the spell check
  spelling::spell_check_files(rmd_files, lang = "de_CH",
                              ignore = ignore_list)

}


# find all the Rmd files in a directory and its subdirectories
find_rmd_files <- function(path,
                           ignore_nospellcheck = FALSE,
                           error_call = rlang::caller_env()) {
  rmd_files <- list.files(path, "\\.Rmd",
                            recursive = TRUE,
                            full.names = TRUE) %>%
    # make sure that only slides and exercises are included
    # we must normalise the path, because we might be in a subfolder already
    # in which case the filter would not work with the relative path.
    normalizePath() %>%
    # slides are either inside the folder of a lecture or in the global folder
    stringr::str_subset("(\\d{2}_.*|global)/")

  # files that contain "nospellcheck" in the first line must be ignored.
  if (ignore_nospellcheck) {
    ignore_files <- is_no_spell_check(rmd_files)
    rmd_files <- rmd_files[!ignore_files]
  }

  # abort, if there are not files to check
  if (length(rmd_files) == 0) {
    cli::cli_abort("No RMarkdown files found.", call = error_call)
  }

  rmd_files
}


# read the wordlist for the spell checks
read_wordlist <- function(type = c("slides", "evaluation")) {
  type <- match.arg(type)
  system.file("extdata", paste0(type, "_wordlist"),
              package = "ibawds") %>%
  readLines() %>%
  # remove comments
  stringr::str_subset("^#", negate = TRUE) %>%
  stringr::str_trim()
}

# helper function: check if a file should be ignored in the spell check
is_no_spell_check <- function(files) {

  files %>%
    vapply(readLines, n = 1, character(1)) %>%
    stringr::str_detect("nospellcheck")
}

# nocov end


#' Check That an URL Can Be Reached
#'
#' Send a request to an URL and return a logical indicating whether the request
#' was successful.
#'
#' @param url the URL to send the request to
#'
#' @returns
#' a logical indicating whether the request was successful.
#'
#' @export

check_url <- function(url) {

  response <- try(
    httr2::request(url) %>%
      httr2::req_method("HEAD") %>%
      httr2::req_perform(),
    silent = TRUE
  )

  !inherits(response, "try-error")
}
