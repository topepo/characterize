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

# ------------------------------------------------------------------------------

#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.rpart <- function(x, ...) {
  tibble::tibble(statistic = "num_term_nodes",
                 value = sum(x$frame$var == "<leaf>")
  )
}


#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.tidy_C50 <- function(x, trials = max(x$trial), ...) {
  if (any(names(x) == "rule_num")) { #<- a rule-based model
    return(niente)
  }
  x <- x[x$trial <= trials,]
  tibble::tibble(statistic = "num_term_nodes",
                 value = nrow(x)
  )
}

#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.C5.0 <- function(x, trials =  x$trials["Actual"], ...) {
  .pluck_num_term_nodes(make_tidy_c5(x), trials = trials)
}

#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.ranger <- function(x, ...) {
  nt <- 1:x$num.trees
  nodes <- purrr::map_int(nt, ~ ranger_nodes(.x, x))
  tibble::tibble(statistic = "num_term_nodes",
                 value = sum(nodes)
  )
}

ranger_nodes <- function(iter, mod) {
  rlang::check_installed("ranger")
  cl <- rlang::call2("treeInfo", .ns = "ranger", object = expr(mod), tree = expr(iter))
  x <- rlang::eval_tidy(cl)
  sum(x$terminal)
}


#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.xgb.Booster <- function(x, nrounds = x$niter, ...) {
  rlang::check_installed("xgboost")
  cl <- rlang::call2("xgb.model.dt.tree", .ns = "xgboost", model = expr(x),
                     trees = seq.int(0, nrounds - 1))
  nodes <- rlang::eval_tidy(cl)
  tibble::tibble(statistic = "num_term_nodes",
                 value = sum(nodes$Feature == "Leaf")
  )
}

#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.partynode <- function(x, ...) {
  rlang::check_installed("partykit")
  cl <- rlang::call2("nodeids", .ns = "partykit", obj = expr(x), terminal = TRUE)
  nodes <- rlang::eval_tidy(cl)
  tibble::tibble(statistic = "num_term_nodes",
                 value = length(nodes)
  )
}
#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.party <- function(x, ...) {
  .pluck_num_term_nodes.partynode(x)
}

#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.cforest <- function(x, ...) {
  nodes <- purrr::map_dfr(x$nodes, .pluck_num_term_nodes.partynode)
  tibble::tibble(statistic = "num_term_nodes",
                 value = sum(nodes$value)
  )
}

#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.bagger <- function(x, ...) {

  if ("MARS" %in% x$base_model) {
    return(niente)
  }

  res <- purrr::map_dfr(x$model_df$model, .pluck_num_term_nodes)
  tibble::tibble(statistic = "num_term_nodes",
                 value = sum(res$value))
}

#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.bart <- function(x, ...) {
  tibble::tibble(statistic = "num_term_nodes",
                 # Row-wise, varcount shows how many times each predictor was
                 # used in a tree. This is the maximum number of splits so this
                 # minus 1 is the number of terminal nodes. Sum this across the
                 # ensemble.
                 value = sum(sum(apply(x$varcount, 1, sum) - 1)))
}

# ------------------------------------------------------------------------------

#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.lgb_trees <- function(x, trees = max(x$trees), ...) {
  dat <- dplyr::filter(x, trees <= !!trees)

  tibble::tibble(statistic = "num_term_nodes",
                 value = sum(!is.na(dat$leaf_count), na.rm = TRUE))
}


#' @rdname pluck_num_term_nodes
#' @export
.pluck_num_term_nodes.lgb.Booster <- function(x, trees = x$params$num_iterations, ...) {
  .pluck_num_term_nodes(lgb_trees(x), trees = trees)
}

