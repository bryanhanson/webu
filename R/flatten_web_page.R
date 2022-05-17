#'
#' Grab a Web Page and Flatten to a Simple Character Vector
#'
#' Convert a web page to a simple character vector which is fast to search.
#' If looking for something specific use the methods in `rvest`.
#'
#' @param url Valid URL string.  User should check validity before calling this function.
#' @return A character vector containing the contents of the web page (xml is stripped out).
#'         Lines containing only whitespace + newlines are removed as well.
#'
#' @importFrom xml2 read_html as_list
#' @export
#'
#' @examples
#' url <- "https://chemospec.org/posts/2020-01-01-Intro-F4S/2021-01-01-Intro-F4S.html"
#' res <- flatten_web_page(url)
#'
#' @tests tinytest
#' url <- "https://chemospec.org/posts/2020-01-01-Intro-F4S/2021-01-01-Intro-F4S.html"
#' res <- flatten_web_page(url)
#' expect_inherits(res, "character")
#'
flatten_web_page <- function(url) {
  webpage <- read_html(url)
  flat <- unlist(xml2::as_list(webpage), use.names = FALSE)
  # remove any lines that are simply "\n", possibly with whitespace on either side
  toss <- grepl("^[[:blank:]]*(\\n)+[[:blank:]]*$", flat)
  flat <- flat[!toss]
}
