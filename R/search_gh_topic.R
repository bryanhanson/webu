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
.search_gh_topic <- function(topic, username, token) {
  DF <- data.frame(name = NA, description = NA, repository = NA)
  resp <- .getResponse(topic, pg = 1, username, token) # get the first page
  DF <- rbind(DF, .processResponse(resp))
  pgs <- .getPageCount(resp) # find out how many page(s) total are available
  if (!is.null(pgs)) {
    for (i in 2:pgs) { # get the rest of the pages
      resp <- .getResponse(topic, pg = i, token)
      DF <- rbind(DF, .processResponse(resp))
    }
  }
  DF[-1, ]
}
