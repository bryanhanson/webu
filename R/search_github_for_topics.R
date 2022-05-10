#'
#' Search Github Repos for Selected Topics
#'
#' Search Github repositories for selected topics.  You will need to
#' set up an access token at Github to avoid severe
#' limits to the rate at which you can search.
#'
#' @param topics Character.  A vector of topics to search.
#' @param username Character.  Github username.
#' @param token Character.  Github authentification token.
#' @param browse Logical.  Shall the repos found be opened in a browser?
#' @return Invisibly, a data frame containing the repo data.
#'
#' @author Bryan A. Hanson
#'
#' @importFrom httr BROWSE
#' @export
#'
search_github_for_topics <- function(topics = NULL, browse = FALSE, username = NULL, token = NULL) {
  if (is.null(topics)) stop("You must provide topic(s) to search")
  if (is.null(username)) stop("You must provide a username")
  if (is.null(token)) stop("You must provide a token")

  chk <- check_for_github_token(token)
  # Search Github and remove packages already known to us
  res <- .search_gh_repos(topics, username, token)

  # Open the pages in a browser
  if (browse) {
    for (i in 1:nrow(res)) {
      if (grepl("^https?://", res$url[i], ignore.case = TRUE)) BROWSE(res$url[i])
    }
  }
  invisible(res)
}
