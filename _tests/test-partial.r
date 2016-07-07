test_that('partial works with positional arguments', {
    expect_that(p(rnorm, 5), has_formals(n = , sd = 1))
    expect_that(p(rnorm, 1, 2), has_formals(n = ))
    # The following seems counter-intuitive, but fixed positional arguments are
    # filled in *after* the first argument, and the resulting following call is
    # in fact invalid.
    # It’s not possible to test for this inside `partial` due to the existence
    # of `...` arguments.
    expect_that(p(rnorm, 1, 2, 3), has_formals(n = ))
    expect_that(p(rnorm, 1, 2, 3)(1), throws_error('unused argument \\(3\\)'))
})

test_that('partial works with named arguments', {
    expect_that(p(rnorm), has_formals(n = , mean = 0, sd = 1))
    expect_that(p(rnorm, mean = 5), has_formals(n = , sd = 1))
    expect_that(p(rnorm, sd = 2), has_formals(n = , mean = 0))
    expect_that(p(rnorm, mean = 1, 2), throws_error('named and unnamed arguments'))
    expect_that(p(rnorm, n = 1, mean = 1, sd = 2), has_formals())
})

test_that('primitive functions work with positional arguments', {
    expect_that(p(`-`, 1)(10), equals(9))
    expect_that(p(`-`, 1)(1 : 3), equals(c(0, 1, 2)))
})

test_that('primitive functions work with named arguments', {
    expect_that(p(sum, na.rm = TRUE)(1, 2, NA), equals(3))
    expect_that(p(sum, na.rm = TRUE)(c(1, 2, NA)), equals(3))
})

test_that('primitive functions work with mixed arguments', {
    expect_that(p(sum, 1, na.rm = TRUE)(2, NA), equals(3))
})

test_that('S3 dispatch works', {
    pp = p(print, digits = 2)
    expect_that(pp(1.234), prints('[1] 1.2'))
    # Un-exported method `stats:::print.lm`
    plm = p(print, digits = 2)
    model = lm(speed ~ dist, cars)
    expect_that(plm(model), prints_some('\\(Intercept\\).*dist'))
    expect_that(plm(model), prints_some('8\\.28\\s+0\\.17'))
})

test_that('Non-standard evaluation works', {
    # TODO: Add test cases for NSE
})

test_that('function is defined in correct environment', {
    expect_that(environment(p(rnorm, 1)), is_identical_to(environment(rnorm)))
    # Test that non-exported objects can be accessed:
    # `.libPaths` accesses the un-exported `.lib.loc` inside its environment.
    lib_loc = .libPaths()
    expect_that(p(.libPaths, lib_loc)(), equals(lib_loc))
})

test_that('... works', {
    f = function (text, ...)
        unname(unlist(read.table(text = text, stringsAsFactors = FALSE, ...)[1, ]))

    expect_that(p(f, sep = ':')('1:2'), equals(c(1, 2)))
    expect_that(p(f, text = '1/2')(sep = '/'), equals(c(1, 2)))
    expect_that(p(f, text = '1 2')(), equals(c(1, 2)))
    expect_that(p(f, text = '1 2')(sep = ','), equals('1 2'))

    # Test with `...` before named arguments.
    sum1 = function (..., na.rm = FALSE) sum(..., na.rm = na.rm)

    expect_that(p(sum1, na.rm = TRUE)(1, 2, NA), equals(3))
    expect_that(p(sum1, na.rm = TRUE)(c(1, 2, NA)), equals(3))
    expect_that(p(sum1, 1, 2, NA)(), equals(NA_real_))
    expect_that(p(sum1, 1, 2, NA)(na.rm = TRUE), equals(3))
})

test_that('missing arguments work', {
    f = function (x, y)
        missing(x)

    expect_that(p(f)(), equals(TRUE))
    expect_that(p(f, TRUE)(1), equals(FALSE))
    expect_that(p(f, x = TRUE)(), equals(FALSE))
    expect_that(p(f, x = TRUE)(1), equals(FALSE))
    expect_that(p(f, y = FALSE)(), equals(TRUE))
    expect_that(p(f, y = FALSE)(TRUE), equals(FALSE))

    # The following, commented-out test fails because non-positional fixed
    # arguments simply fill the remaining arguments, from left to right, after
    # the arguments given in the call. As a consequence, `TRUE` drops into the
    # first argument, `x`. The only way to prevent this is to replace
    # `match.call` inside `partial` with a version that contains all
    # arguments, even unset ones. This will yield the expected call with the
    # first argument always unset. Alas, R *still* does not consider this
    # value “missing”. We therefore accept these (unwanted) semantics for now.
    #
    # expect_that(p(f, TRUE)(), equals(TRUE))
    #
    # Assert the opposite to catch any regressions changing this behaviour.
    expect_that(p(f, TRUE)(), equals(FALSE))

    g = function (a, b)
        if (missing(a)) b + 1 else if (missing(b)) a + 1 else a + b

    expect_that(p(g, b = 2)(5), equals(7))
    expect_that(p(g, a = 2)(5), equals(7))
    expect_that(p(g, 2)(5), equals(7))
    expect_that(p(g)(2, 5), equals(7))
})

test_that('stack frame can be inspected', {
    .hidden = 1
    expect_that(p(ls, all.names = TRUE)(), equals(ls(all.names = TRUE)))
    # TODO: Ensure `missing`, `match.call` etc. work
})
