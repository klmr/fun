#' Create a closure over a given environment for the specified formals and body.
#'
#' Helper function to facilitate the creation of functions from quoted
#' expressions at runtime.
#'
#' @param formals list of formal arguments; list names correspond to parameter
#'  names, list values correspond to quoted parameter defaults
#' @param body quoted expression to use as function body
#' @param env the environment in which to define the function
#' @return A function defined in \code{env}.
#'
#' @examples
#' x = new.env()
#' closure(list(a = quote(expr = )), call('+', quote(a), 1), x)
#' # function (a)
#' # a + 1
#' # <environment: 0x7feec6390b8>
closure = function (formals, body, env)
    eval(call('function', as.pairlist(formals), body), env)
