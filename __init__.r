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

#' Compose functions \code{g} and \code{f}.
#'
#' @param g a function taking as its argument the return value from \code{f}
#' @param f a function with arbitrary arguments
#' @return A fuction which takes the same arguments as \code{f} and returns the
#' same return type as \code{g}
#'
#' @note Functions are applied in the inverse order of
#' \code{\link{functional::Compose}}:
#' \url{http://tolstoy.newcastle.edu.au/R/e9/help/10/02/4529.html}
#'
#' @note The semantics of \code{compose} are given by the equivalence
#' \code{compose(g, f)(...) = g(f(...))}.
#'
#' @note All three forms of this function (\code{compose}, \code{\%.\%} and
#' \code{\%|>\%}) are exactly identical. The only difference is the order of the
#' arguments, which is reversed in \code{\%|>\%}. \code{\%|>\%} is thus the
#' higher-order function counterpart to \code{\link{\%>\%}}.
compose = function (g, f)
    function (...) g(f(...))

#' @rdname compose
`%.%` = compose

#' @rdname compose
`%|>%` = function (f, g) compose(g, f)

#' Pipe operator like in F#, Bash …
#' @seealso \code{\link{magrittr::\%>\%}}
`%>%` = magrittr::`%>%`
