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

#' Test whether a value is “falsy”.
#'
#' \code{isFALSE(x)} tests whether \code{x} is a falsy value.
#' @param x the object to be tested
#' @return Returns either \code{TRUE} or \code{FALSE}, depending on whether
#' \code{x} is falsy.
#'
#' @details
#' An object is considered “falsy” when it is either the single value
#' \code{FALSE} or no value at all, i.e. \code{NULL} / a vector of length 0.
isFALSE = function (x)
    identical(`attributes<-`(x, NULL), FALSE) || length(x) == 0

#' Fall back to an alternative value if none given
#'
#' \code{a \%||\% b} evaluates to \code{a} unless \code{a} is falsy, in which
#' case it evaluates to \code{b}.
#' @param value the value
#' @param alternative an alternative value
#' @return Returns \code{value}, unless it isn’t a value, or \code{FALSE}, in
#' which case \code{alternative} is returned.
#' @seealso isFALSE
`%||%` = function (value, alternative)
    if(isFALSE(value)) alternative else value

# FIXME: Add vectorized variant `%|%`, and `%or%`, which also handles empty
# strings and errors.

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
