sq_check_token <- function() {
    tok <- sq_opt("token")
    if (is.null(tok) || !nzchar(tok) || is.na(tok)) {
        tok <- Sys.getenv("SQUIDLE_API_TOKEN")
        if (!is.null(tok) && nzchar(tok) && !is.na(tok)) sq_set_opt(token = tok)
    }
    if (is.null(tok) || !nzchar(tok) || is.na(tok)) stop("invalid API token")
    tok
}
