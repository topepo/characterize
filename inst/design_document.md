Design notes for the characterize package
================

The characterize package returns statistics that describe interesting
aspects of the data, usually related to the structure or complexity of
the fitted model.

The primary function is `characterize()`, which has methods for
tidymodels containers (e.g., parsnip or workflow objects) and the
underlying model fit objects.

When `characterize()` is invoked, an exhaustive list of functions
(prefixed with `.pluck_`) is executed for the object. The default
methods for these methods return empty tibbles with appropriate columns.
If there is a specific method, it is executed, and a tibble is returned
with a data structure similar to the output produced by the yardstick
package.

For example:

``` r
library(tidymodels)
```

    ── Attaching packages ────────────────────────────────────── tidymodels 1.0.0 ──

    ✔ broom        1.0.1          ✔ recipes      1.0.4     
    ✔ dials        1.1.0          ✔ rsample      1.1.1.9000
    ✔ dplyr        1.0.10         ✔ tibble       3.1.8     
    ✔ ggplot2      3.4.0          ✔ tidyr        1.2.1     
    ✔ infer        1.0.3          ✔ tune         1.0.1.9002
    ✔ modeldata    1.0.1.9000     ✔ workflows    1.1.2     
    ✔ parsnip      1.0.3          ✔ workflowsets 1.0.0     
    ✔ purrr        1.0.0          ✔ yardstick    1.1.0.9000

    ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ✖ purrr::discard() masks scales::discard()
    ✖ dplyr::filter()  masks stats::filter()
    ✖ dplyr::lag()     masks stats::lag()
    ✖ recipes::step()  masks stats::step()
    • Dig deeper into tidy modeling with R at https://www.tmwr.org

``` r
library(characterize)

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)
```

``` r
# Fit a simple model using parsnip 

cart_fit <- 
  decision_tree() %>%
  set_mode("regression") %>% 
  fit(mpg ~ ., data = mtcars)
cart_fit
```

    parsnip model object

    n= 32 

    node), split, n, deviance, yval
          * denotes terminal node

    1) root 32 1126.04700 20.09062  
      2) cyl>=5 21  198.47240 16.64762  
        4) hp>=192.5 7   28.82857 13.41429 *
        5) hp< 192.5 14   59.87214 18.26429 *
      3) cyl< 5 11  203.38550 26.66364 *

``` r
characterize(cart_fit)
```

    # A tibble: 2 × 3
      .metric             .estimator .estimate
      <chr>               <chr>          <dbl>
    1 num_active_features model              2
    2 num_term_nodes      model              3

## Sub-models

As with model tuning, there are cases where characteristics can be
returned for multiple sub-models. For example, glmnet model objects can
return model coefficients for each value of the `penalty` parameter. For
such models, a specific `multi_characterize()` method is provided with a
particular argument for the tuning parameter that indexes the
sub-models.

## `.pluck` methods

These methods are invoked in `characterize()` and return tibbles with
columns `statistic` and `value` (string and doubles, respectively).
Their input is a model object and, when sub-models can be exploited,
arguments for the relevant tuning parameters.

Here’s an example of a basic method for decision trees:

``` r
#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.rpart <- function(x, ...) {
  tibble::tibble(statistic = "num_term_nodes",
                 value = sum(x$frame$var == "<leaf>")
  )
}
```

The documentation for these methods includes `@keywords internal`, so
they are not listed in the help documents.

There are S3 methods for the results of `tidy()` output in a few cases
since this can be easier to work with. For example, the S3 methods for
`C5.0` trees call the `tidy()` method and then re-dispatch on that
object type.

Almost all `.pluck` methods return doubles for the `value` column. One
specific S3 generic, `.pluck_active_features()`, returns a character
vector that enumerates which predictors were actually used by the model.
In this case, the `value` column is a list column. This method is not
returned by `characterize()` but can be called directly by the user.

## Making new `.pluck` generics and/or methods

For some new numeric quantity (i.e., a new `.pluck_` generic), a new R
file should be created with an initial set of S3 methods for the
principal cases. Here is an example:

``` r
#' Compute the number of terminal nodes in a tree-based model
#' @inheritParams characterize
#' @keywords internal
#' @name pluck_num_term_nodes
#' @export
.pluck_num_term_nodes <- function(x, ...) {
  UseMethod(".pluck_num_term_nodes")
}

#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.default <- function(x, ...) {
  niente
}

#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.workflow <- function(x, ...) {
  x <- workflows::extract_fit_engine(x)
  .pluck_num_term_nodes(x, ...)
}

#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.model_fit <- function(x, ...) {
  x <- parsnip::extract_fit_engine(x)
  .pluck_num_term_nodes(x, ...)
}
```

where `niente` is a placeholder tibble used when there is no specific S3
method for the object.

The pluck methods are straightforward to create for most cases.

As a reminder, S3 methods that enable `multi_characterize()` should have
arguments for any sub-model parameters that can be iterated over. Here
is an example of the glmnet method:

``` r
#' @rdname multi_characterize
#' @export
multi_characterize.glmnet <- function(object, penalty = NULL, ...) {
  rlang::check_installed("glmnet")

  if (is.null(penalty)) {
    penalty <- object$lambda
  }

  res <-
    tibble::tibble(penalty = penalty) %>%
    dplyr::mutate(results = purrr::map(penalty, ~ characterize(object, penalty = .x)))
  res
}
```

Note that the `mutate()` call generates a list-column of results. This
is consistent with how parsnip does `multi_predict()` and can be easily
collapsed into a simple data frame using `tidyr::unnest()`

## Package dependencies and testing

Like the broom package, this package could have a large number of
“Suggests” dependencies. Using some tricks, we manage to avoid extra
dependencies.

First, we try to reply on generics like `tidy()` to do the required
computations. This approach would require the underlying package to be
installed to use the `tidy()` method, but we don’t need to invoke a more
specific generic method (see the counter-example below).

When you do need to use a package that has a non-specific function name
(like `tidy()`), we suggest using `rlang::check_installed("pkgname")`.
This will give users a good error message and will also load the
package’s namespace (but not fully attach it). This means that an S3
generic like `coef()` or `tidy()` can be written in the code for your
new `.pluck` method without any issues.

Second, if you need to use a specific function name from a package, you
can invoke it by constructing a call. For example, the ranger package
has a function `treeInfo()` that gives specific data on the splits and
other aspects of the trees. After checking for its installation, a call
object (`cl` below) is created for the code that you want to call (but
will not be explicitly written in the package sources). The call object
is executed below using an rlang function.

This code:

``` r
rlang::check_installed("ranger")
cl <- rlang::call2("treeInfo", .ns = "ranger", object = expr(mod), tree = expr(iter))
x <- rlang::eval_tidy(cl)
```

has the same effect as writing out

``` r
ranger::treeInfo(object = mod, tree = iter)
```

without requiring a formal ranger dependency.

Besides the reduced readability, the downside to doing this is that the
package can never run unit tests for these methods. There’s no way
around that. However, the tidymodels GH repository
[extratests](https://github.com/tidymodels/extratests) is designed to
facilitate unit tests with numerous dependencies. The drawback to using
extratests is that the testing coverage listed in the characterize
GitHub repo is an underestimate (and will appear to be poorly tested).
