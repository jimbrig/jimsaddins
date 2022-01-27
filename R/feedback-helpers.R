#' Feedback Message Utility Functions
#'
#' @description
#' A set of helper functions for providing verbose feedback to the developer using
#' this packages functions.
#'
#' @param x The string passed to various `msg_` functions.
#' @param bullet What to use for the message's `bullet`. Defaults to `cli::symbol$bullet`
#'
#' @name feedback
#' @family Feedback Utilities
#' @keywords feedback
#'
#' @seealso
#'   - [usethis::ui-questions()]
#'   - [cli::list_symbols()]
NULL

#' @rdname feedback
msg_field <- function(x) {
  x <- crayon::green(x)
  x <- glue::glue_collapse(x, sep = ", ")
  x
}

#' @rdname feedback
msg_value <- function(x) {

  if (is.character(x)) {
    x <- encodeString(x, quote = "'")
  }
  x <- crayon::yellow(x)
  x <- glue::glue_collapse(x, sep = ", ")
  x

}

#' @rdname feedback
msg_done <- function(x) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = parent.frame())
  msg_bullet(x, crayon::green(cli::symbol$tick))
}

#' @rdname feedback
msg_bullet <- function(x, bullet = cli::symbol$bullet) {
  bullet <- paste0(bullet, " ")
  x <- indent(x, bullet, "  ")
  inform(x)
}

#' @rdname feedback
msg_err <- function(x) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = parent.frame())
  msg_bullet(x, crayon::red(cli::symbol$cross))
}

#' @rdname feedback
msg_path <- function(x) {
  is_directory <- fs::is_dir(x) | grepl("/$", x)
  x_rel <- fs::path_rel(x, start = here::here())
  x_tidy <- fs::path_tidy(x_rel)
  out <- ifelse(is_directory, paste0(x_tidy, "/"), x_tidy)
  msg_value(out)
}

#' @rdname feedback
msg_info <- function(x) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = parent.frame())
  msg_bullet(x, crayon::yellow(cli::symbol$info))
}

#' @rdname feedback
msg_code <- function(x) {
  x <- encodeString(x, quote = "`")
  x <- crayon::silver(x)
  x <- glue::glue_collapse(x, sep = ", ")
  x
}

#' @rdname feedback
msg_feedback <- function(x) {
  x <- crayon::green(x)
  x <- glue_collapse(x, sep = ", ")
  x
}

#' Inform
#'
#' @description
#' A wrapper around [rlang::inform()] for providing feedback to developers using
#' this packages functions.
#'
#' @inheritDotParams rlang::inform
#'
#' @family Feedback Utilities
#' @seealso [rlang::inform()]
#'
#' @return feedback in console
#' @export
#'
inform <- function(...) {
  rlang::inform(paste0(...))
}

#' Indent
#'
#' @description
#' Indentation around various `msg_` feedback functions.
#'
#' @param x The string passed to various `msg_` functions.
#' @param first what to indent with - defaults to `"  "`.
#' @param indent indentation of next line - defaults to `first`
#'
#' @family Feedback Utilities
#'
#' @export
#' @return string
indent <- function(x, first = "  ", indent = first) {
  x <- gsub("\n", paste0("\n", indent), x)
  paste0(first, x)
}

