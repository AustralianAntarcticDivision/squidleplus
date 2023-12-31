% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requests.R
\name{sq_v}
\alias{sq_v}
\title{Convert an expression into an API query value}
\usage{
sq_v(expr)
}
\arguments{
\item{expr}{expression: an expression of the form `name op value`. For an operator `op` that is not valid R syntax, use `\%op\%`. See [sq_req_query()] for Squidle operators}
}
\value{
A list object, most likely to be used with [sq_req_query()]
}
\description{
Convert an expression into an API query value
}
\examples{
sq_v(key == "Batemans201011")
sq_v(key \%has\% "Batemans201011") ## operator that is not a valid R operator
sq_v(campaign \%has\% sq_v(key == "Batemans201011")) ## nested

}
\references{
<https://squidle.org/api/help?template=api_help_page.html#api_query>
}
