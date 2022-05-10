#'
#' Verify a Github Token is Available
#'
#' You will need to set up an access token at Github to avoid severe
#' limits to the rate at which you can search.
#'
#' @section Obtaining a Token:
#' One first needs to create an "app" at Github following the instructions
#' [here](https://github.com/settings/developers).
#' Then do the following steps:
#' ```
#' library("httr")
#' myapp <- oauth_app("app_name", key = "app_key", secret = "app_secret")
#' github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
#' ```
#'
#' @param token Character.  A string giving the token.
#' @return Logical.  `TRUE` if the token is found in the workspace.
#'
#' @author Bryan A. Hanson
#'
#' @importFrom httr set_config config
#' @export
#' @aliases set_up_github_token check_for_github_token
#'
check_for_github_token <- function(token = NULL) {
  if (is.null(token)) stop("You must provide a token")

  # Make sure we have a token to avoid rate limits
  # The token is generated interactively via "Web Application Flow",
  # and is deposited in the local workspace.
  # See developer.github.com/apps/building-oauth-apps/authorizing-oauth-apps/#web-application-flow
  # This token has classes 'Token2.0', 'Token', 'R6' <Token2.0>
  local_token_found <- FALSE
  local_token_found <- exists(quote(token))
  if (!local_token_found) stop("Did not find a local token")
  if (local_token_found) set_config(config(token = token))
  return(TRUE)
}
