## Functional tools for R

This module provides a basic toolbox for working with functions in R.


```r
fun = modules::import('klmr/functional')
```

### Creating functions

Use `closure` to create a function:


```r
twice = fun$closure(alist(x =), quote(x * 2), globalenv())
twice
```

```
## function (x) 
## x * 2
```

```r
twice(10)
```

```
## [1] 20
```

### Function composition

Use `compose` to compose functions:


```r
four_times = fun$compose(twice, twice)
four_times(2)
```

```
## [1] 8
```

Alternatively, use one of the function composition operators:


```r
# Traditional order of arguments, same as `compose`:
sd1 = sqrt %.% var
sd1(CO2$uptake)
```

```
## [1] 10.81441
```

```r
# Alternative order:
sd2 = var %|>% sqrt
sd2(CO2$uptake)
```

```
## [1] 10.81441
```

### Function chaining

As made popular by ‹magrittr› and ‹dplyr›:


```r
CO2$uptake %>% var %>% sqrt
```

```
## [1] 10.81441
```

Note the similarity in usage between `%>%` and `%|>%`.

### Partial function application

Partial function application via `partial` (or its shortcut `p`) creates a new
function with fewer arguments.


```r
modules::import('klmr/functional', attach = 'p')
```


```r
minus1 = p(`-`, 1)
sapply(1 : 5, minus1)
```

```
## [1] 0 1 2 3 4
```

These higher-order functions become powerful when combined, and easily construct
complex anonymous functions.


```r
strrev =
    p(strsplit, '') %|>%
    p(lapply, rev) %|>%
    p(lapply, p(paste, collapse = '')) %|>%
    unlist
strrev(c('foo', 'bar'))
```

```
## [1] "oof" "rab"
```

### A concise lambda syntax


```r
modules::import('klmr/functional/lambda')

sapply(1 : 4, x -> 2 * x)
```

```
## [1] 2 4 6 8
```

```r
mapply(x ~ y -> x + y, 1 : 4, 5 : 8)
```

```
## [1]  6  8 10 12
```

* `var -> expr` is equivalent to `function (var) expr`.

* `x ~ y -> expr` is equivalent to `function (x, y) expr`. An arbitrary number
  of arguments is supported.

**Note:** Importing this submodule changes the semantics of `<-` (since `<-` and
`->` refer to the same operator in R). Consequently, this module can only be
used in code that uses `=` for assignment.
