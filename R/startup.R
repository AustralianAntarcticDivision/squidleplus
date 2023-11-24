.onLoad <- function(libname, pkgname) {
    ## if we are re-loading the package during an existing session, we don't want to override existing options
    existing_options <- sq_opts()
    default_options <- list(
        token = Sys.getenv("SQUIDLE_API_TOKEN"),
        base_url = "https://squidle.org"
    )
    if (!is.null(existing_options)) {
        for (nm in names(existing_options)) {
            default_options[[nm]] <- existing_options[[nm]]
        }
    }
    options(list(squidleplus = default_options))
    invisible()
}
