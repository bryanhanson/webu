
##### Helper Functions for Working with Github

#' Report if there are rate-limit issues
#' There is a limit of how often you can hit GH:
#' https://developer.github.com/v3/#rate-limiting
#'
#' @importFrom jsonlite fromJSON
#' @noRd
#'
.report_github_access_issue <- function(response) {
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
.check_github_access <- function(j) {
  return(j$message)
}

#' Get the commit date
#' @noRd
#'
.get_github_commit_date <- function(j) {
  return(j$commit$author$date)
}

#' Get the issue date
#' @noRd
#'
.get_github_issue_date <- function(j) {
  return(j$updated_at)
}

#' Extract fields
#' @noRd
#'
.extract_github_fields <- function(x) {
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
.get_github_page_count <- function(response) {
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
.get_github_response <- function(topic, pg, username, token) {
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

#' Process a Response from Github
#'
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @noRd
#'
.process_github_response <- function(response) {
  json <- content(response, "text")
  json <- jsonlite::fromJSON(json, simplifyVector = FALSE) # returns a list
  if (!is.null(.check_github_access(json))) {
    .report_github_access_issue(response)
    stop("Github access rate exceeded, try again later")
  }

  ans <- .extract_github_fields(json)
}

#'
#' Search All Github Repositories for Topics of Interest
#'
#' @param topics Character.  Strings to search on Github.
#'        Currently, only a single topic like "NMR" can be used.  Although the
#'        Github documentation implies that a Boolean query string such as
#'        "NMR+OR+IR" can be used, I cannot make it work.
#' @param username Character.  Github username.
#' @param token Character.  Github authentification token.
#' @return Data frame.
#'
#' @author Bryan A. Hanson
#' @noRd
#'
.search_github_repos <- function(topics, username, token) {
  DF <- data.frame(name = NA, desc = NA, url = NA)

  for (i in 1:length(topics)) {
    DF2 <- .search_github_topic(topics[i], username, token)
    DF <- rbind(DF, DF2)
  }

  DF <- unique(DF[2:nrow(DF), ])
  DF <- DF[-1, ]
}

#'
#' Search for Topics on Github
#'
#' Search Github for topics.
#'
#' @param topic Character.  Length one.
#'        Currently, only a single topic like "NMR" can be used.  Although the
#'        Github documentation implies that a Boolean query string such as
#'        "NMR+OR+IR" can be used, I cannot make it work.
#' @param username Character.  Github username.
#' @param token Character.  Github authentification token.
#' @return Data frame.
#'
#' @author Bryan A. Hanson
#' @noRd
#'
.search_github_topic <- function(topic, username, token) {
  DF <- data.frame(name = NA, description = NA, repository = NA)
  resp <- .get_github_response(topic, pg = 1, username, token) # get the first page
  DF <- rbind(DF, .process_github_response(resp))
  pgs <- .get_github_page_count(resp) # find out how many page(s) total are available
  if (!is.null(pgs)) {
    for (i in 2:pgs) { # get the rest of the pages
      resp <- .get_github_response(topic, pg = i, token)
      DF <- rbind(DF, .process_github_response(resp))
    }
  }
  DF[-1, ]
}
