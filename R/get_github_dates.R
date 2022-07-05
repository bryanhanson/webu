#'
#' Get the Most Recent Commit or Issue Date from a Github Repository
#'
#' Access a Github repository and retrieve the date of the most recent commit or issue.
#'
#' @param url Character.  A string giving the URL of a Github repository.
#' @param what Character. Either `commits` or `issues`.
#' @return A date in Y-M-D format (ISO 8601 essentially) and class `Date`,
#' unless there is a problem, in which case `NA`.
#'
#' @author Bryan A. Hanson
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd_hms ymd date
#' @export
#'
#' @examples
#' tst <- get_github_dates("https://github.com/bryanhanson/ChemoSpec")
#'
#' @tests tinytest
#' expect_inherits(get_github_dates("https://github.com/bryanhanson/ChemoSpec", "commits"), "Date")
#' expect_inherits(get_github_dates("https://github.com/bryanhanson/ChemoSpec", "issues"), "Date")
#'
get_github_dates <- function(url, what = "commits") { # url not vectorized

  if (!grepl("github\\.com", url)) {
    return(NA_character_)
  }
  if (!what %in% c("commits", "issues")) stop("'what' should be either 'commits' or 'issues'")
  if (what == "commits") func <- .get_github_commit_date
  if (what == "issues") func <- .get_github_issue_date

  # Prep the access string
  splitURL <- unlist(strsplit(url, "/"))
  if (length(splitURL) != 5L) {
    return(NA_character_)
  } # repo may be missing in some cases
  owner <- splitURL[4]
  repo <- splitURL[5]

  # Access the API & extract most recent commit or issue date
  # GET is what counts against access rate
  # We get back a page of (up to) 30 results -- most recent first, which is what we need
  # https://developer.github.com/v3/#pagination
  gh_string <- paste0("https://api.github.com/repos/", owner, "/", repo, "/", what)
  response <- GET(gh_string)
  json <- content(response, "text")
  json <- fromJSON(json, simplifyVector = FALSE) # returns a list
  if (!is.null(.check_github_access(json))) {
    .report_github_access_issue(response)
    stop("Github access rate exceeded, try again later")
  }
  alldates <- unlist(lapply(json, func))
  alldates <- ymd_hms(alldates)
  alldates <- alldates[order(as.Date(alldates), decreasing = TRUE)]
  ghdate <- alldates[1] # ghdate = most recent commit date
  ghdate <- date(ghdate) # just Y-M-D, no time
  ghdate
}
