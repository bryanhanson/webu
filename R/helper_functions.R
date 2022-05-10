
### Helper Functions
### Not exported, no Rd file

#' Report if there are rate-limit issues
#' There is a limit of how often you can hit GH:
#' https://developer.github.com/v3/#rate-limiting
#'
#' @importFrom jsonlite fromJSON
#' @noRd
#'
.reportAccessIssue <- function(response) {
  # https://docs.github.com/en/rest/reference/rate-limit
  message("\nI tried: ", response$url)
  cat("API access type:", response$headers$"x-ratelimit-resource", "\n")
  cat("API access limit:", response$headers$"x-ratelimit-limit", "\n")
  cat("API access used:", response$headers$"x-ratelimit-used", "\n")
  cat("API access remaining:", response$headers$"x-ratelimit-remaining", "\n")
  reset <- as.integer(response$headers$"x-ratelimit-reset")
  reset <- as.POSIXct(reset, origin = "1970-01-01", tz = "UCT")
  cat_string <- paste("Access resets at:", reset, "UTC\n", sep = " ")
  cat(cat_string)
}

#' Get the results of an access attempt
#' @noRd
#'
.checkAccess <- function(j) {
  return(j$message)
}

#' Get the commit date
#' @noRd
#'
.commitDate <- function(j) {
  return(j$commit$author$date)
}

#' Get the issue date
#' @noRd
#'
.issueDate <- function(j) {
  return(j$updated_at)
}

#' Extract fields
#' @noRd
#'
.extractFields <- function(x) {
  ni <- lengths(x["items"])
  DF <- data.frame(name = NA, description = NA, repository = NA)
  # sometimes length(x[[3]]) comes back as zero, so skip it
  # this occurs as the last page on some queries
  for (i in 1:ni) {
    if (length(x[[3]]) == 0L) break
    tmp <- unlist(x[[3]][[i]][c("name", "description", "html_url")], use.names = FALSE)
    DF <- rbind(DF, tmp)
  }
  DF[2:nrow(DF), ]
}

#' Get the Number of Github Pages in a Repo
#'
#' @importFrom httr GET headers
#' @importFrom stringr str_extract_all str_extract
#' @noRd
#'
.getPageCount <- function(response) {
  link <- headers(response)$link
  pages <- unlist(stringr::str_extract_all(link, "page=[0-9]+"))
  last_page <- pages[length(pages)]
  last_page <- as.integer(stringr::str_extract(last_page, "[0-9]+"))
  # next line handles the case where there is only one page total
  if (length(last_page) == 0L) last_page <- NULL
}

#' Access the Github API
#'  GET is what counts against access rate
#' https://docs.github.com/en/rest/guides/traversing-with-pagination
#'
#' @importFrom httr authenticate add_headers
#' @noRd
#'
.getResponse <- function(topic, pg, username, token) {
  # Set up query string (https://stackoverflow.com/a/48412541/633251)
  q_string <- paste0(
    "https://api.github.com/search/repositories?q=topic:", topic,
    "&page=", pg
  )
  response <- GET(q_string,
                  config = list(
                    authenticate(username, token$credentials$access_token),
                    add_headers("Accept: application/vnd.github.mercy-preview+json")
  ))
}

#' Process a Response
#'
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @noRd
#'
.processResponse <- function(response) {
  json <- content(response, "text")
  json <- jsonlite::fromJSON(json, simplifyVector = FALSE) # returns a list
  if (!is.null(.checkAccess(json))) {
    .reportAccessIssue(response)
    stop("Github access rate exceeded, try again later")
  }

  ans <- .extractFields(json)
}
