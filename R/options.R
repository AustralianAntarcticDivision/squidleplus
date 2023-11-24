## internal helper functions for dealing with package options

sq_opts <- function() getOption("squidleplus")
sq_opt <- function(optname) sq_opts()[[optname]]
sq_set_opt <- function(...) {
    opts <- sq_opts()
    newopts <- list(...)
    for (nm in names(newopts)) opts[[nm]] <- newopts[[nm]]
    options(list(squidleplus = opts))
}
