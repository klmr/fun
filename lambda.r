modules::import('.', 'closure')

#' Define a single-argument function
#'
#' \code{x -> expression} defines an anonymous function of \code{x}.
#'
#' @param params the formal argument names, separated by \code{~}, and
#' optionally surrounded by parentheses, e.g. \code{(a ~ b)}
#' @param body an expression to be executed, same as a \code{function}’s body
#'
#' @note \strong{Warning} This breaks \code{<-} assignment because it overloads
#' the operator. \code{=} assignment \strong{must} be used instead.
#' The rationale for introducing this redefinition and breaking existing code is
#' two-fold:
#'
#' \enumerate{
#'  \item R is a functional programming language, yet has no concise way of
#'      defining an anonymous function. This seriously prevents good code. There
#'      are ways of alleviating this (e.g. by using the \code{.} function) but
#'      none of these approaches the ease of using anonymous functions in other
#'      modern functional (or even non-functional) languages.
#'  \item Even though there’s a general preference in the R community of using
#'      \code{<-} over \code{=} for assignment, the truth is that there is no
#'      single compelling argument for this preference, other than history, and
#'      the need for concise lambdas clearly overrides a whimsical preference of
#'      one redundant operator over another.
#' }
#'
#' @usage params -> body
#' @name lambda
#' @aliases ->
#' @examples
#' sapply(1 : 4, x -> 2 * x)
#' # [1] 2 4 6 8
#' mapply(x ~ y -> x + y, 1 : 4, 5 : 8)
#' # [1]  6  8 10 12
`<-` = function (body, params) {
    vars = all.vars(substitute(params))
    formals = setNames(replicate(length(vars), quote(expr = )), vars)
    closure(formals, substitute(body), parent.frame())
}
