#' Get or set the Squidle+ instance to use
#'
#' @param url string: the URL of the instance to use. By default `squidleplus` uses "https://squidle.org"
#'
#' @return If called with no arguments, the current instance URL will be returned as a string. If called with a \code{url} argument, the instance will be set to that and then the instance URL returned.
#'
#' @examples
#'
#' ## current instance
#' sq_instance()
#'
#' ## set new instance
#' sq_instance("https://my.squidleinstance.com")
#'
#' @export
sq_instance <- function(url) {
    if (!missing(url)) {
        ## set the instance url
        assert_that(is.string(url))
        sq_set_opt(base_url = url)
    }
    sq_opt("base_url")
}
