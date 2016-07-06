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
    # TODO: test that non-exported objects can be accessed.
})

test_that('... works', {
    # TODO: Add test case for `...`
})

test_that('stack frame can be inspected', {
    .hidden = 1
    expect_that(p(ls, all.names = TRUE)(), equals(ls(all.names = TRUE)))
    # TODO: Ensure `missing`, `match.call` etc. work
})
