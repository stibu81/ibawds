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
  eval_files <- list.files(path, "Beurteilung_.*\\.(R|q)md",
                           recursive = TRUE,
                           full.names = TRUE) %>%
    stringr::str_subset("Beurteilung_Template\\.(R|q)md", negate = TRUE)

  if (!is.null(students)) {
    eval_files <- eval_files %>%
      stringr::str_subset(
        paste0("Beurteilung_", students, ".(R|q)md", collapse = "|")
      )
  }

  # abort, if there are not files to check
  if (length(eval_files) == 0) {
    cli::cli_abort("No evaluation files found.")
  }

  # extract the names of the students and use them as the ignore list.
  ignore_list <- eval_files %>%
    stringr::str_extract("Beurteilung_(.*)\\.(R|q)md", group = 1)

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


# find all the Rmd & qmd files in a directory and its subdirectories
find_rmd_files <- function(path,
                           ignore_nospellcheck = FALSE,
                           error_call = rlang::caller_env()) {

  rmd_files <- list.files(path, "\\.(R|q)md",
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

  rlang::check_installed("httr2")

  # try a HEAD request first, because it's faster. But some servers do not
  # support it, so we will try a GET afterwards, if this fails.
  response <- try(
    httr2::request(url) %>%
      httr2::req_method("HEAD") %>%
      # do not throw error when an http error is encountered, because
      # this should only catch urls that don't exist.
      httr2::req_error(is_error = \(x) FALSE) %>%
      httr2::req_perform(),
    silent = TRUE
  )

  # a try error implies that the resolution of the URL failed
  # => return failure
  if (inherits(response, "try-error")) {
    return(FALSE)
  }

  # if we have success, return success
  if (httr2::resp_status(response) < 300) {
    return(TRUE)
  }

  # otherwise, try a GET request and return
  # this time, we do not catch HTTP errors with httr2::req_error, such that
  # these also result in a try error.
  response <- try(
    httr2::request(url) %>%
      httr2::req_method("GET") %>%
      httr2::req_perform(),
    silent = TRUE
  )

  !inherits(response, "try-error")
}


#' Check All Links in the Slide Deck
#'
#' Check links in all files of a slide deck using [check_links_in_file()].
#'
#' @param path path to the top level directory of a lecture
#'
#' @return
#' a tibble listing the links that did not work.
#'
#' @export

check_links_in_slides <- function(path) {

  rmd_files <- find_rmd_files(path, ignore_nospellcheck = FALSE)

  progress_bar_options <- list(
    show_after = 0,
    format = paste("{cli::pb_bar} Processing file",
                   "{cli::pb_current + 1}/{cli::pb_total} [{cli::pb_elapsed}]")
  )
  link_checks <- purrr::map(rmd_files, check_links_in_file,
                            .progress = progress_bar_options) %>%
    magrittr::set_names(basename(rmd_files)) %>%
    dplyr::bind_rows(.id = "file") %>%
    dplyr::filter(!.data$reachable) %>%
    dplyr::select(url, file) %>%
    dplyr::arrange(url, file)

  link_checks
}


#' Check All Links in a Text File
#'
#' Find and check all http(s) URLs in an text file.
#' Only links starting with `http://` or `https://` are found and checked.
#'
#' @param file the path to the file to be checked.
#'
#' @returns
#' a tibble with two columns:
#' * `url`: the URL that was found and checked
#' * `reachable`: whether the URL could be reached
#'
#' @export

check_links_in_file <- function(file) {

  # the progress bar in read_lines() must be suppressed in order for the
  # progress bar from purrr::map() in check_links_in_slides() to work properly.
  urls <- readr::read_lines(file, progress = FALSE) %>%
    extract_urls() %>%
    unique()
  url_check <- urls %>%
    vapply(check_url, logical(1), USE.NAMES = FALSE)

  dplyr::tibble(
    url = urls,
    reachable = url_check
  )
}


# helper function to extract all urls from a character vector
extract_urls <- function(x) {

  urls <- x %>%
    stringr::str_extract_all("https?://[A-Za-z0-9./_%#?!=&$*()~+:;,-]+") %>%
    unlist()

  # since urls may contain parentheses, the pattern also extracts them.
  # However, if an url is followed by a closing parenthesis that is not part
  # of the url, it is also extractd. => remove trailing closing parentheses
  # that have no matching opening parenthesis.
  n_opening <- stringr::str_count(urls, "\\(")
  n_closing <- stringr::str_count(urls, "\\)")
  rm_pattern <- stringr::str_dup("\\)[^)]*", pmax(n_closing - n_opening, 0))

  urls <- stringr::str_remove(urls, paste0(rm_pattern, "$"))

  # remove punctuation at the end of the urls
  stringr::str_remove(urls, "[,;.:?!]*$")
}

