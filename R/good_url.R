#'
#' Verify a URL
#'
#' This function will attempt to access a URL, and if successful, return `TRUE`.
#' Success is defined as getting a 200 status code.
#'
#' @param url Character.  A string giving the URL.
#' @param quiet Logical.  If `TRUE` stay quiet.
#' @param ... Extra arguments to be passed to `httr::GET`.
#' @return A logical; `TRUE` if the URL was accessible.
#'
#' @author Bryan A. Hanson
#' @importFrom httr GET status_code
#' @export
#'
#' @examples
#' good <- good_url("https://example.com")
#'
#' @tests tinytest
#' expect_true(good_url("https://example.com"))
#' expect_false(good_url("https://xjfoidw.com"))
#'

good_url <- function(url, quiet = TRUE, ...) {
  good <- tryCatch(
    # main event / { } enclose the expr (1st argument)
    {
      status <- status_code(GET(url, ...))
      good <- FALSE
      if (status == 200L) good <- TRUE
      return(good)
    },
    # if an error occurs
    error = function(cond) {
      if (!quiet) {
        errmess <- "Could not access the URL"
        message("\nError message from R: ", cond$message, "\n")
        message(errmess)
      }
      good <- FALSE
      return(good)
    },
    # if a warning occurs
    warning = function(cond) {
      if (!quiet) message("\nError message from R: ", cond$message, "\n")
      good <- FALSE
      return(good)
    }
  ) # end of tryCatch
  return(good)
}
