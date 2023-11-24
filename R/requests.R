#params={"q": json.dumps({"filters":[{"name":"api_token","op":"eq","val":api_token}],"single":True})}
#user_id = requests.get(url+"/api/users", params=params).json().get("id")

#' Construct a request object
#'
#' @param endpoint string: e.g. `/api/media`
#' @param query list: as returned by [sq_req_query()]
#'
#' @return An `httr2_request` object
#'
#' @examples
#' \dontrun{
#'   ## search for all media that contains annotations with the name that
#'   ##  includes "ecklonia" (ignoring case)
#'   res <- sq_req("api/media", query = sq_req_query(filters = sq_v(annotations %any%
#'                  sq_v(annotations %any% sq_v(label %has% sq_v(name %ilike% "%ecklonia%"))))))
#'   sq_req_content(res)
#' }
#' @export
sq_req <- function(endpoint, query) {
    sq_check_token()
    request(paste0(sub("[/]+$", "", sq_opt("base_url")), "/", sub("^[/]+", "", endpoint))) %>%
        req_headers("auth-token" = sq_opt("token"), "Content-type" = "application/json", "Accept" = "application/json") %>%
        ##req_body_json(body) %>%
        req_url_query(q = to_json(query)) %>%
        req_error(is_error = sq_req_error_handler) %>%
        req_perform
}

#' Extract response content
#'
#' @param res httr2_response: 
#'
#' @return An R object
#'
#' @export
sq_req_content <- function(res) from_json(resp_body_string(res))

sq_req_error_handler <- function(resp) stop("request failed: ", resp_body_string(resp))

## Pagination
##
## /api/resource?results_per_page=...&page=...
##
## Responses to most GET requests (with the exception of the export endpoints) are paginated by default, with at most ten objects per page. To request a specific page, add a page=N query parameter to the request URL, where N is a positive integer (the first page is page one). If no page query parameter is specified, the first page will be returned.
##
## In order to specify the number of results per page, add the query parameter results_per_page=N where N is a positive integer. If results_per_page is greater than the maximum number of results per page as configured by the server, then the query parameter will be ignored.
##
## In addition to the "objects" list, the response JSON object will have a "page" key with the current page number, a "num_pages" key with total number of pages into which the set of matching instances is divided, and a "num_results" key with total number of instances which match the requested search.


#' Construct a squidleplus API query
#'
#' The query parameter q must be a JSON string of the form `{"filters":[...],"limit":...,"offset":...,"order_by":[...],"group_by":[...],"single":...}`. All parameters are optional.
#'
#' The <operatorname> strings recognized by the API include:
#' * `==`, `eq`, `equals`, `equals_to` - "equal to" operator, where val can be numeric or string
#' * `!=`, `neq`, `does_not_equal`, `not_equal_to` - "not equal to" operator, where val can be numeric or string
#' * `>`, `gt`, `<`, `lt` - "less/greater than" operator, where val is typically numeric
#' * `>=`, `ge`, `gte`, `geq`, `<=`, `le`, `lte`, `leq` - "less/greater than or equal to" operator, where val is typically numeric
#' * `in`, `not_in` - "is in" operator, where val is typically a list of values
#' * `is_null`, `is_not_null` - "is null" check, where there is no val param
#' * `like`, `ilike`: - "like" (case sensitive) or "ilike" (case insensitive) comparison, where val is a string. Wildcards can be included as a % symbol (or %25 url-encoded).
#' * `has` - for nesting operators on a relation that is a single object (i.e.: MANYTOONE)
#' * `any` - for nesting operators on a relation that is a list of objects (i.e.: ONETOMANY/MANYTOMANY)
#'
#' For geometry columns, there are also some spatial operators for <operatorname>, which include:
#' * `geo_at` - Geometry / Geography type <fieldname> is close to a nominated lat/lon. The val takes the form: {"lat":...,"lon":...,"dist":...} where dist is optional and defaults to 0.00005. For Geometry types dist is in srid units (eg: degrees), for Geography types it is meters. Hint: assume Geometry types most of the time.
#' * `geo_in_bbox` - Geometry / Geography type <fieldname> is within a bounding box. The val takes the form: [p1,p2], which is a list containing two elements being two diagonal points defining the bounding box, eg: [bottom-left, top-right]. p1,p2 are position objects of the form, {"lat":...,"lon":...}
#' * `geo_in_poly` - Geometry / Geography type <fieldname> is within a polygon. The val is a polygon list of points of the form: [p1,p2,p3,...,pn,p1], where p1-pn are position objects of the form {"lat":..., "lon":...}. Note the polygon is closed, being that it starts and ends wth p1.
#'
#' @references <https://squidle.org/api/help?template=api_help_page.html#api_query>
#' @param filters list: a list of one of the following forms:
#' * `list(name = "fieldname", op = "operatorname", val = "argument")` where <operatorname> is one of the strings described below, <argument> is a value to be used as the second argument to the given operator and <fieldname> is the name of the field of the model to which to apply the operator, which can be either a MODEL COLUMN, RELATED MODEL, ASSOCIATION PROXY or HYBRID ATTRIBUTE for a resource
#' * `list(name = "fieldname", op = "operatorname", field = "fieldname")` where the first <fieldname> and <operatorname> are as above and the second <fieldname> is the field of the model that should be used as the second argument to the operator. The first <fieldname> may specify a field on a related model, if it is a string of the form <relationname>__<fieldname>. Alternatively if the field name is the name of a relation and the operator is "has" or "any", the "val" argument can be a dictionary with the arguments representing another, nested filter to be applied as the argument for "has" or "any"
#' Filters can also exclude results, for example `list(not = <filterobject>)`.
#' Filter objects can also be arbitrary boolean formulas, for example `list(or = <filterobject>, list(and = <filterobject>, ...), ...)`
#' Filter objects can also be defined on hybrid attributes of a model. Hybrid property filter objects are defined in the same way as column fields, but to filter by a hybrid method with one or more input arguments (<arg1>, <arg2>, ...,  <argN>), the object can be defined as:
#' `list(name = list(method = "method_name", args = list(<arg1>, <arg2>, ..., <argN>)), op = "operatorname", val = "argument")`
#' The returned list of matching instances will include only those instances that satisfy all of the given filters. If a filter is poorly formatted (for example, op is set to '==' but val is not set), the server responds with 400 Bad Request.
#' @param limit integer: a positive integer which specifies the maximum number of objects to return
#' @param offset integer: a positive integer which specifies the offset into the result set of the returned list of instances
#' @param order_by list: a list of objects of the form:
#' * `list(field = "fieldname", direction = "directionname")` where <fieldname> is a string corresponding to the name of a field of the requested model and <directionname> is either "asc" for ascending order or "desc" for descending order. <fieldname> may alternately specify a field on a related model, if it is a string of the form <relationname>__<fieldname>. Ordering can also be defined on hybrid attributes of a model. Hybrid property order_by objects are defined in the same way as column fields, but to filter by a hybrid method with one or more input arguments (<arg1>, <arg2>, ..., <argN>), the object can be defined as: `list(field = list(method = "method_name", args = list(<arg1>, <arg2>, ..., <argN>)), direction = "directionname")`
#' @param group_by list: a list of the form `list(field = "fieldname")` where `fieldname` is a string corresponding to the name of a field of the requested model. `fieldname` may alternately specify a field on a related model, if it is a string of the form `relationname__fieldname`
#' @param single logical: whether a single result is expected as a result of the search. If this is true and either no results or multiple results meet the criteria of the search, the server responds with an error message
#'
#' @return A list object that will be converted to a JSON-formatted string when added to an API query
#'
#' @seealso [sq_query()] [sq_v()]
#'
#' @examples
#'
#' ## get all media_collections that contain media from a campaign matching the key "Batemans201011"
#' sq_req_query(filters =
#'   sq_v(media %any% sq_v(deployment %has% sq_v(campaign %has% sq_v(key == "Batemans201011")))))
#' ## equivalent to
#' ## {"filters":[{"name":"media", "op":"any",
#' ##    "val":{"name":"deployment", "op":"has",
#' ##      "val":{"name":"campaign", "op":"has",
#' ##        "val":{"name":"key", "op":"eq", "val":"Batemans201011"}}}}]}
#'
#' ## search for all media that contains annotations with the name that
#' ##  includes "ecklonia" (ignoring case)
#' sq_req_query(filters = sq_v(annotations %any% sq_v(annotations %any%
#'                sq_v(label %has% sq_v(name %ilike% "%ecklonia%")))))
#'
#' @export
sq_req_query <- function(filters, limit, offset, order_by, group_by, single) {
    Filter(Negate(is.null), list(filters = if (!missing(filters)) list(filters) else NULL,
                                 limit = if (!missing(limit)) limit else NULL,
                                 offset = if (!missing(offset)) offset else NULL,
                                 order_by = if (!missing(order_by)) order_by else NULL,
                                 group_by = if (!missing(group_by)) group_by else NULL,
                                 single = if (!missing(single)) single else NULL))
}


#' Convert an expression into an API query value
#'
#' @references <https://squidle.org/api/help?template=api_help_page.html#api_query>
#' @param expr expression: an expression of the form `name op value`. For an operator `op` that is not valid R syntax, use `\%op\%`. See [sq_req_query()] for Squidle operators
#'
#' @return A list object, most likely to be used with [sq_req_query()]
#'
#' @examples
#' sq_v(key == "Batemans201011")
#' sq_v(key %has% "Batemans201011") ## operator that is not a valid R operator
#' sq_v(campaign %has% sq_v(key == "Batemans201011")) ## nested
#'
#' @export
sq_v <- function(expr) {
    ex <- substitute(expr)
    ## nested expressions
    if (length(ex) == 2 && identical(get(as.character(ex[1]), envir = asNamespace("squidleplus")), sq_v)) ex <- substitute(eval(expr))
    if (length(ex) != 3) stop("expr should be a simple expression such as `key == \"foo\"`")
    op <- sub("^%", "", sub("%$", "", as.character(ex[1])))
    v1 <- as.character(ex[2])
    v2 <- as.character(ex[3])
    ## nested
    try({
        v2e <- str2expression(v2)[[1]]
        if (length(v2e) == 2 && identical(get(as.character(v2e[1]), envir = asNamespace("squidleplus")), sq_v)) {
            v2 <- eval(v2e)##sq_v(parse(text = as.character(v2e[2]))[[1]][1]
        }
    }, silent = TRUE)
    list(name = v1, op = op, val = v2)
}


#' Convert an R object to a JSON string and vice-versa
#'
#' @param x object: thing to convert
#'
#' @return For `to_json` a string, converted to JSON using `jsonlite::toJSON(x, auto_unbox = TRUE)`. For `from_json` an R object, converted using `jsonlite::fromJSON(x)`
#'
#' @seealso [jsonlite::toJSON()] [jsonlite::fromJSON()]
#'
#' @export
to_json <- function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE)
}

#' @rdname to_json
#' @export
from_json <- function(x) jsonlite::fromJSON(x)
