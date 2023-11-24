#' Get or set your API token
#'
#' @references <https://squidle.org/api/help?template=api_help_page.html>
#' @param token string: token. If not provided, and the `SQUIDLE_API_TOKEN` environment variable is set (e.g. via `Sys.setenv(SQUIDLE_API_TOKEN = "xyz")`) that will be used
#'
#' @return The API token (invisibly unless the `token` parameter was not provided)
#'
#' @export
sq_api_token <- function(token) {
    tm <- missing(token)
    if (tm) {
        ## if the token is set in sq_opt, return it
        tok <- sq_opt("token")
        if (!is.null(tok) && nzchar(tok) && !is.na(tok)) {
            return(tok)
        } else {
            ## otherwise look in env vars
            token <- Sys.getenv("SQUIDLE_API_TOKEN")
        }
    }
    if (is.null(token) || !nzchar(token) || is.na(token)) warning("invalid API token")
    sq_set_opt(token = token)
    if (tm) token else invisible(token)
}

