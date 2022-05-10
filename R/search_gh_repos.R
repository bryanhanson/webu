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
.search_gh_repos <- function(topics, username, token) {
  DF <- data.frame(name = NA, desc = NA, url = NA)

  for (i in 1:length(topics)) {
    DF2 <- .search_gh_topic(topics[i], username, token)
    DF <- rbind(DF, DF2)
  }

  DF <- unique(DF[2:nrow(DF), ])
  DF <- DF[-1, ]
}
