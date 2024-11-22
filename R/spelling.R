# nocov start

#' Check Spelling in the Evaluation of the Papers
#'
#' Evaluation of the student papers is done in the form of an Rmd file. This
#' function finds all the evaluations in a directory and checks the spelling.
#'
#' @param path path to the top level directory of the evaluations.
#' @param students an optional character vector with student names. If given,
#'  only the evaluation of these students will be checked.
#' @param use_wordlist should a list of words be excluded from the spell
#'  check? The package contains a list of words that have typically appeared
#'  in the evaluations of the past. The names of the students will always be
#'  excluded from spell check, even if `use_wordlist` is `FALSE`.
#'
#' @export

spell_check_evaluation <- function(path = ".",
                                   students = NULL,
                                   use_wordlist = TRUE) {

  # find the evaluation files recursively, ignore the template
  eval_files <- list.files(path, "Beurteilung_.*\\.Rmd",
                           recursive = TRUE,
                           full.names = TRUE) %>%
    stringr::str_subset("Beurteilung_Template\\.Rmd", negate = TRUE)

  if (!is.null(students)) {
    eval_files <- eval_files %>%
      stringr::str_subset(paste0("Beurteilung_", students, ".Rmd", collapse = "|"))
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
    ignore_list <- system.file("extdata", "evaluation_wordlist",
                               package = "ibawds") %>%
      readLines() %>%
      c(ignore_list)
  }

  # run the spell check
  spelling::spell_check_files(eval_files, lang = "de_CH",
                              ignore = ignore_list)

}

# nocov end
