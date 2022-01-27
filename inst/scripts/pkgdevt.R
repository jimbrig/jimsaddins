
#  ------------------------------------------------------------------------
#
# Title : jimsaddins R Package Development Script
#    By : Jimmy Briggs
#  Date : 2022-01-27
#
#  ------------------------------------------------------------------------

# library R packages ------------------------------------------------------

library(devtools)
library(usethis)
library(pkgbuild)
library(pkgload)
library(pkgdown)
library(testthat)
library(knitr)
library(pak)
library(purrr)
library(desc)
library(chameleon)
library(attachment)
library(templateeR)

# initialize package ------------------------------------------------------

usethis::create_package("jimsaddins")
usethis::use_namespace()
usethis::use_roxygen_md()
usethis::use_git()
# usethis::use_tibble() # #' @return a [tibble][tibble::tibble-package]
# usethis::use_pipe()
# usethis::use_tidy_eval()
devtools::document()


# github ------------------------------------------------------------------

# set description and title first so included in GH repo
desc::desc_set(
  "Description" = "A collection of RStudio Addins.",
  "Title" = "RStudio Addins"
)

usethis::use_github(private = FALSE)

# github labels -----------------------------------------------------------

templateeR::use_gh_labels()

# git-cliff ---------------------------------------------------------------

templateeR::use_git_cliff()
templateeR::use_git_cliff_action()

# package docs ------------------------------------------------------------

usethis::use_readme_rmd()
usethis::use_mit_license()
usethis::use_package_doc()
usethis::use_news_md()


# functions ---------------------------------------------------------------

c(
  # add function file names here:
  "utils",
  "addin-remove_comments",
  "addin-open_project",
  "addin-wrap_roxygen",
  # "addin-find_todos",
  "addin-importFrom",
  "addin-prefix_functions",
  "addin-script_header",
  "addin-format_markdown",
  "addin-addin_manager",
  "addin-edit_rstudio_preferences",
  "addin-search_cran",
  "addin-youtube",
  "addin-markdown_collapsible_details"
) |> purrr::walk(usethis::use_r, open = FALSE)


# addins ------------------------------------------------------------------

c(
  "rImportFrom",
  "tidy_importFrom",
  "remove_comments",
  "wrap_roxygen"
) |>
  purrr::walk(usethis::use_addin, open = FALSE)

usethis::use_addin("wrap_roxygen")

# tests -------------------------------------------------------------------

c(
  # add function test file names here:

) |> purrr::walk(usethis::use_test)

# data --------------------------------------------------------------------

c(
  # add data prep script names here:

) |> purrr::walk(usethis::use_data_raw)

# vignettes ---------------------------------------------------------------

c(
  # add vignette names here:
  "jimsaddins"

) |> purrr::walk(usethis::use_vignette)





