#'
#' Search PyPi for Selected Topics
#'
#' Search PyPi.org for selected topics.  PyPi.org does not require authentification.
#'
#' @param topics Character.  A vector of topics to search.
#' @param browse Logical.  Shall the repos found be opened in a browser?
#'        This could be a lot of tabs!
#' @return Invisibly, a data frame containing the repo data.  Elements are:
#' \itemize{
#'   \item name  Name of the project.
#'   \item description Short summary of the project.
#'   \item repository The url to the repository on PyPi.org.
#'   \item author_email E-mail of the author.
#'   \item maintainer_email E-mail of the maintainer.
#' }
#'
#' @author Bryan A. Hanson
#'
#' @importFrom httr BROWSE
#' @export
#'
#' @examples
#' \dontrun{
#' # This returns less than 10 pages
#' res <- search_pypi_for_topics("narwhal", browse = TRUE)
#' }
#' 
search_pypi_for_topics <- function(topics = NULL, browse = FALSE) {
  if (is.null(topics)) stop("You must provide topic(s) to search")

  DF <- .setup_data_frame()

  for (i in 1:length(topics)) {
    tmp <- .search_pypi_topic(topics[i])
    DF <- rbind(DF, tmp)
    if (i != length(topics)) {
      message("\n\nGiving the server a 30 second courtesy break...\n")
      Sys.sleep(30)
    }
  }
  DF <- DF[-1,]
  # Optionally open the pages in a browser
  if (browse) {
    for (i in 1:nrow(DF)) BROWSE(DF$repository[i])
  }
  invisible(DF)
}
