#' Tidy importFrom Roxygen Statements
#'
#' statements, in the `R` directory of the current working directory will be
#' moved to a file `R/imports.R`, cleaned and sorted.
#'
#' @export
#'
#' @family importFrom
#'
#' @examples
#' \dontrun{
#' tidy_importFrom()
#' }
#'
#' @importFrom fs dir_exists file_exists file_create dir_ls
#' @importFrom purrr map walk2
tidy_importFrom <- function() {

  if (!fs::dir_exists("R")) stop(msg_err("Error: no R directory found. Exiting..."))

  if (!fs::file_exists("R/imports.R")) {
    fs::file_create("R/imports.R")
    msg_done("Created file {msg_path('R/imports.R')}")
  }

  files <- fs::dir_ls("R", type = "file", regexp = "*.[rR]$")
  lines <- purrr::map(files, readLines)
  imports <- process_imports(imports)

  if (lines[["R/imports.R"]][1] != "NULL" || is.na(lines[["R/imports.R"]][1])) {
    lines[["R/imports.R"]] <- c("NULL", lines[["R/imports.R"]])
  }

  lines[["R/imports.R"]] <- c(imports, lines[["R/imports.R"]])

  purrr::walk2(lines, files, writeLines)

  return(invisible())
}

#' Process Imports
#'
#' This helper function processes `importFrom` roxygen2 tags for use in the
#' [tidy_importFrom()] funciton/add-in.
#'
#' @param imports list of `importFrom` statements.
#'
#' @return vector of processed import statements
#' @export
#'
#' @keywords internal
#' @importFrom purrr map imap
#' @importFrom stringr str_squish str_extract str_remove str_split str_wrap
process_imports <- function(imports) {

  x <- unlist(imports, use.names = FALSE)
  x <- stringr::str_squish(x)

  package <- stringr::str_extract(x, "([^ ]+)")

  functions <- stringr::str_remove(x, "([^ ]+) ")
  functions <- stringr::str_split(functions, " ")
  functions <- split(functions, package)
  functions <- purrr::map(functions, unlist)
  functions <- purrr::map(functions, unique)
  functions <- purrr::map(functions, sort)
  functions <- purrr::map(functions, paste, collapse = " ")
  functions <- purrr::imap(functions, ~ stringr::str_wrap(.x, 80 - 16 - nchar(.y)))
  functions <- purrr::map(functions, stringr::str_split, "\n")
  functions <- purrr::map(functions, unlist)

  unlist(functions, use.names = FALSE)

}

#' Generate `importFrom` Roxygen Tags
#'
#' @param script R script to generate tags for.
#'
#' @export
#'
#' @export
#'
#' @family importFrom
#'
#' @seealso [dreamRs/prefixer](https://github.com/dreamRs/prefixer)
#'
#' @importFrom stringr str_extract_all str_split
generate_importFrom <- function(script) {

  imp <- stringr::str_extract_all(string = script, pattern = "[[:alnum:]\\.]+::[[:alnum:]\\._]+")

  if (length(imp[[1]]) > 0) {
    imp <- stringr::str_split(imp[[1]], pattern = "::")
    imp <- lapply(X = imp, FUN = matrix, nrow = 1)
    imp <- do.call("rbind", imp)
    imp <- as.data.frame(unique(imp))
    imp <- tapply(X = imp[[2]], INDEX = imp[[1]], FUN = paste, collapse = " ", simplify = FALSE)
  } else {
    character(0)
  }

}


#' Generate `importFrom` Roxygen Tag from a Function
#' Works only if functions used are prefixed in the body.
#'
#' @param fun A function.
#' @param quiet Logical, display output to console ?
#'
#' @return Invisible character string
#' @export
#' @family importFrom
#'
#' @examples
#'
#' my_fun <- function(path) {
#'   utils::read.table(file = path, header = FALSE, sep = "\t")
#' }
#' import_from(my_fun)
import_from <- function(fun, quiet = FALSE) {
  body_ <- as.character(body(fun))
  body_ <- paste(body_, collapse = "\n")
  res <- generate_importFrom(body_)
  if (!quiet)
    cat(paste0(paste(res, collapse = "\n"),"\n"))
  invisible(paste(res, collapse = "\n"))
}

#' Addin to generate `@importFrom` Roxygen Tags for R Functions
#'
#' @noRd
#' @keywords internal
#' @family importFrom
#'
#' @importFrom rstudioapi getSourceEditorContext insertText
#' @importFrom stringr str_which
rImportFrom <- function() {
  script <- rstudioapi::getSourceEditorContext()$contents
  script_ <- paste(script, collapse = "\n")
  if.env <- new.env()
  try_parse <- try(eval(parse(text = script_), envir = if.env), silent = TRUE)
  if (class(try_parse) == "try-error") {
    warning("Something went wrong, does your script contains only functions ?")
    return(invisible())
  }
  if_insert <- lapply(
    X = ls(if.env),
    FUN = function(x) {
      if (is.function(if.env[[x]])) {
        tag_if <- import_from(if.env[[x]], quiet = TRUE)
        if (nchar(tag_if) > 0) {
          list(
            importFrom = paste0(tag_if, "\n"),
            num_row = stringr::str_which(
              string = script,
              pattern = paste0("^", x, "[:space:]*(<-|=)[:space:]*function")
            )
          )
        } else {
          NULL
        }
      }
    }
  )
  if_insert <- dropNullsOrEmpty(if_insert)
  rstudioapi::insertText(
    location = Map(c, sapply(if_insert, `[[`, "num_row"), 1),
    text = sapply(if_insert, `[[`, "importFrom")
  )
}

#' Check `importFrom` Statements in \code{NAMESPACE}
#'
#' @description
#' Checks if the functions declared in the `NAMESPACE`'s `importFrom`
#' statements are actually used in the package functions.
#'
#' @param path Path to package directory.
#'
#' @return Functions not used invisibly.
#' @export
#' @family importFrom
#'
#'
#' @examples
#' \dontrun{
#'
#' # Execute in a package directory
#' check_import_from()
#'
#' }
#' @importFrom stringi stri_subset stri_split_regex stri_subset_regex stri_detect_fixed stri_c
check_import_from <- function(path = ".") {
  if (!file.exists(file.path(path, "NAMESPACE")))
    stop("No NAMESPACE to check.", call. = FALSE)

  # search funs in importFrom
  namespace <- readLines(con = file.path(path, "NAMESPACE"), warn = FALSE)
  importFrom <- stringi::stri_subset(str = namespace, regex = "^importFrom")
  importFrom <- stringi::stri_split_regex(str = importFrom, pattern = "\\(|,|\\)")
  importFrom <- lapply(importFrom, `[`, 3)
  importFrom <- unlist(importFrom)

  # Read all R scripts
  r_scripts <- list.files(path = file.path(path, "R"), pattern = "(R|r)$")
  r_scripts <- lapply(
    X = file.path(path, "R", r_scripts),
    FUN = readLines, warn = FALSE
  )
  r_scripts <- unlist(r_scripts)
  r_scripts <- stringi::stri_subset_regex(str = r_scripts, pattern = "^#", negate = TRUE)

  # Check if funs appears
  appears <- lapply(
    X = importFrom,
    FUN = function(x) {
      any(stringi::stri_detect_fixed(str = r_scripts, pattern = x))
    }
  )
  appears <- as.logical(appears)
  appears <- importFrom[!appears]
  if (length(appears) == 0) {
    message("All functions in @importFrom are used !")
    return(invisible(character(0)))
  } else {
    warning(paste("These functions do not seem to be used:", stringi::stri_c(appears, collapse = ", ")), call. = FALSE)
    return(invisible(appears))
  }
}

