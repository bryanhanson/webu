
##### Helper Functions for Searching PyPi.org

#'
#' Set up a data frame to hold results
#'
#' @noRd
#'
.setup_data_frame <- function(author = TRUE) {
  if (!author) DF <- data.frame(name = NA, description = NA, repository = NA, maintainer_email = NA)
  if (author) DF <- data.frame(name = NA, description = NA, repository = NA, author_email = NA, maintainer_email = NA)
  DF
}


#'
#' Search for a Topic on PyPi.org
#'
#' Search PyPi.org for ONE topic.
#'
#' @param topic Character.  Length one.
#' @return Data frame. Elements are name, description, repository, author_email, maintainer_email.
#'
#' @author Bryan A. Hanson
#' @noRd
#'
.search_pypi_topic <- function(topic) {
  DF <- .setup_data_frame()
  resp <- .get_pypi_response(topic, pg = 1) # get the first page
  DF <- rbind(DF, .process_pypi_response(resp, topic, 1L))
  pg <- 2L
  while (TRUE) { # get the rest of the pages
    resp <- .get_pypi_response(topic, pg)
    if (.check_for_404(resp)) break # we ran out of pages
    DF <- rbind(DF, .process_pypi_response(resp, topic, pg))
    pg <- pg + 1L
  }
  DF[-1, ]
}

#'
#' Run a Query on PyPi for a Topic
#'
#' @param topic Character. Length one string.
#' @param pg Integer.  The page number of the web page containing responses.
#' @return The result of `GET`, class `response`.
#' 
#' @author Bryan A. Hanson
#' @importFrom utils URLencode
#' @noRd
#'
.get_pypi_response <- function(topic, pg) {
  # need to encode in case there are spaces and other special characters
  q_string <- URLencode(paste0("https://pypi.org/search/?q=", topic, "&page=", pg))
  response <- GET(q_string)
}

#'
#' Process the response from PyPi
#'
#' The response from PyPi.org searching is a web page
#' which is (apparently) identical to what you get when searching manually.
#' From here one can get the name of the package, and then one has to search 
#' for the details on that package in a separate step.
#'
#' @param response An object of class `response`, from a call to `GET`.
#' @return A data frame with elements name, description, repository, author_email, maintainer_email
#' 
#' @author Bryan A. Hanson
#' @noRd
#' @importFrom rvest read_html html_elements html_text2
#' @importFrom magrittr %>%
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
.process_pypi_response <- function(response, topic, pg) {
  DF <- .setup_data_frame()
  pnames <- read_html(response) %>% html_elements(".package-snippet__name") %>% html_text2()
  message("\nProcessing ", length(pnames), " packages on page ", pg, " matching topic '", topic, "'...\n")
  pb <- txtProgressBar(min = 0, max = length(pnames), style = 3)
  for (i in 1:length(pnames)) {
    setTxtProgressBar(pb, i)
    deets <- .get_pypi_pkg_details(pnames[i])
    DF <- rbind(DF, deets)
  }
  DF[-1,]
}

#'
#' Get the Details for a Package on PyPi
#'
#' @param pkg Character.  The name of a package on PyPi.org
#' @return Character.  A vector giving info about the package.
#'
#' @author Bryan A. Hanson
#' @noRd
#'
.get_pypi_pkg_details <- function(pkg) {
  url <- paste0("https://pypi.org/pypi/", pkg, "/json")
  ans <- c(pkg, rep(NA_character_, 4))
  if (good_url(url)) {
    json <- fromJSON(url)
  # summary is better than description for our purposes (description is typically in markdown and long)
  # assembling the answer this way as it seems sometimes the fields are NULL
  ans[2] <- ifelse(is.null(json$info$summary), NA, json$info$summary)
  ans[3] <- ifelse(is.null(json$info$package_url), NA, json$info$package_url)
  ans[4] <- ifelse(is.null(json$info$maintainer_email), NA, json$info$maintainer_email)
  ans[5] <- ifelse(is.null(json$info$author_email), NA, json$info$author_email)
  }
ans
}


# Returns TRUE if the page is 404
.check_for_404 <- function(response) {
  ans <- FALSE
  if (response$status == "404") ans <- TRUE
  ans
} 
