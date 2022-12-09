
## usethis namespace: start
#' @import rlang
#' @importFrom dplyr %>%
#' @importFrom stats predict coef
#' @importFrom methods slotNames
#' @importFrom utils methods
#' @importFrom stats as.formula
## usethis namespace: end
NULL

# ------------------------------------------------------------------------------
# nocov

utils::globalVariables(
  c(".estimator", "Conditions", "Model", "committee", "rule", "statistic",
    "value", "rule_vars", "estimate", "trial", "name", "id", ".config",
    ".extracts", ".metric", "object", "tree_index")
)

# nocov end
