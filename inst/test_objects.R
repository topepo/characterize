pkgs <-
  c("baguette", "bonsai", "brulee", "butcher", "C50", "caret", "dbarts",
    "discrim", "earth", "lobstr", "mda", "plsmod", "rlang", "rpart", "rules",
    "sessioninfo", "tidymodels")

load_pkg <- function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))
}

load_res <- lapply(pkgs, load_pkg)

# ------------------------------------------------------------------------------

tidymodels_prefer()

# ------------------------------------------------------------------------------

# Model fits begin with `fit_`.
# Expected results lists start with `exp_`. There are only created when one of
# the 'engine' packages is required to compute the results.

# ------------------------------------------------------------------------------

data(ames)
ames$Sale_Price <- log10(ames$Sale_Price)
ames <-
  ames %>%
  mutate(Sale_Price <- log10(Sale_Price)) %>%
  slice(1:100) %>%
  select(Sale_Price, Neighborhood, Longitude, Latitude)

data("penguins")
penguins <- penguins[complete.cases(penguins),]

set.seed(1)
cls_dat <- sim_classification(50)
reg_dat <- sim_regression(50)
mnl_dat <-
  sim_multinomial(
    100,
    ~  -0.5    +  0.6 * abs(A),
    ~ ifelse(A > 0 & B > 0, 1.0 + 0.2 * A / B, - 2),
    ~ -0.6 * A + 0.50 * B -  A * B)

# ------------------------------------------------------------------------------
# bagger

## C5.0

set.seed(1)
fit_cls_bag_c5 <-
  bagger(class ~ ., data = cls_dat, base_model = "C5.0", times = 3) %>%
  butcher()

# The number of terminal nodes and active predictors computed with tidy method in
# the rules pkgs

tidy_cls_bag_c5 <-
  purrr::map(fit_cls_bag_c5$model_df$model, tidy) %>%
  purrr::list_rbind()

get_rule_vars <- function(x) {
  unique(all.vars(parse_expr(x)))
}

var_names <- map(tidy_cls_bag_c5$rule, get_rule_vars)
var_names <- sort(unique(unlist(var_names)))

exp_cls_bag_c5 <- list()
exp_cls_bag_c5$num_term_nodes <- nrow(tidy_cls_bag_c5)
exp_cls_bag_c5$num_features_active <- length(var_names)
exp_cls_bag_c5$features_active <- var_names

## rpart

set.seed(1)
fit_cls_bag_rpart <-
  bagger(class ~ ., data = cls_dat, base_model = "CART", times = 3) %>%
  butcher()

set.seed(1)
fit_reg_bag_rpart <-
  bagger(Sale_Price ~ ., data = ames, base_model = "CART", times = 3) %>%
  butcher()

## mars

set.seed(1)
fit_cls_bag_mars <-
  bagger(class ~ ., data = cls_dat, base_model = "MARS", times = 3) %>%
  butcher()

set.seed(1)
fit_reg_bag_mars <-
  bagger(Sale_Price ~ ., data = ames, base_model = "MARS", times = 3) %>%
  butcher()

## nnet

set.seed(1)
fit_cls_bag_nnet <-
  bagger(class ~ ., data = cls_dat, base_model = "nnet", times = 3) %>%
  butcher()

set.seed(1)
fit_reg_bag_nnet <-
  bagger(outcome ~ ., data = reg_dat, base_model = "nnet", times = 3) %>%
  butcher()

# ------------------------------------------------------------------------------
# bart

# TODO stop butcher from removing `varcount`; needed for 'num_term_nodes'

set.seed(1)
fit_cls_dbart <-
  dbarts::bart(cls_dat[-1], y.train = cls_dat$class, verbose = FALSE)
lobstr::obj_size(fit_cls_dbart)

# we need 'varcount' to measure the number of terminal nodes but butcher removes it
cls_varcount <- fit_cls_dbart$varcount
fit_cls_dbart <- butcher(fit_cls_dbart)
lobstr::obj_size(fit_cls_dbart)

fit_cls_dbart$varcount <- cls_varcount
lobstr::obj_size(fit_cls_dbart)


set.seed(1)
fit_reg_dbart <-
  dbarts::bart(reg_dat[-1], y.train = reg_dat$outcome, verbose = FALSE)

reg_varcount <- fit_reg_dbart$varcount
fit_reg_dbart <- butcher(fit_reg_dbart)
fit_reg_dbart$varcount <- reg_varcount

# ------------------------------------------------------------------------------
# brulee_linear_reg

set.seed(12)
fit_reg_bru_lin <-
  brulee_linear_reg(outcome ~ ., data = reg_dat) %>%
  butcher()

# ------------------------------------------------------------------------------
# brulee_logistic_reg

set.seed(12)
fit_cls_bru_logit <-
  brulee_logistic_reg(class ~ ., data = cls_dat, hidden_units = 3) %>%
  butcher()

# ------------------------------------------------------------------------------
# brulee_mlp

set.seed(12)
fit_cls_bru_mlp <-
  brulee_mlp(class ~ ., data = cls_dat, hidden_units = 3) %>%
  butcher()

set.seed(12)
fit_reg_bru_mlp <-
  brulee_mlp(outcome ~ ., data = reg_dat, hidden_units = 3) %>%
  butcher()

# ------------------------------------------------------------------------------
# brulee_multinomial_reg

set.seed(12)
fit_cls_bru_multi <-
  brulee_multinomial_reg(class ~ ., data = mnl_dat) %>%
  butcher()

# ------------------------------------------------------------------------------
# C5.0

set.seed(1)
fit_cls_tree_c5 <-
  C5.0(class ~ ., data = cls_dat, trials = 1) %>%
  butcher()

# The number of terminal nodes and active predictors computed with tidy method in
# the rules pkgs

tidy_cls_tree_c5 <- tidy(fit_cls_tree_c5)

var_names <- map(tidy_cls_tree_c5$rule, ~ all.vars(parse_expr(.x)))
var_names <- sort(unique(unlist(var_names)))

rule_sizes <- map_int(tidy_cls_tree_c5$rule, characterize:::rule_size)
mean_size <- mean(rule_sizes)

exp_cls_tree_c5 <- list()
exp_cls_tree_c5$num_term_nodes <- nrow(tidy_cls_tree_c5)
exp_cls_tree_c5$num_features_active <- length(var_names)
exp_cls_tree_c5$features_active <- var_names
exp_cls_tree_c5$mean_rule_size <- mean_size

###

set.seed(1)
fit_cls_tree_bst_c5 <-
  C5.0(class ~ ., data = cls_dat, trials = 3) %>%
  butcher()

tidy_cls_tree_bst_c5 <- tidy(fit_cls_tree_bst_c5)

var_names <- map(tidy_cls_tree_bst_c5$rule, ~ all.vars(parse_expr(.x)))
var_names <- sort(unique(unlist(var_names)))

rule_sizes <- map_int(tidy_cls_tree_bst_c5$rule, characterize:::rule_size)
mean_size <- mean(rule_sizes)

exp_cls_tree_bst_c5 <- list()
exp_cls_tree_bst_c5$num_term_nodes <- nrow(tidy_cls_tree_bst_c5)
exp_cls_tree_bst_c5$num_features_active <- length(var_names)
exp_cls_tree_bst_c5$features_active <- var_names
exp_cls_tree_bst_c5$mean_rule_size <- mean_size

###

set.seed(1)
fit_cls_rule_c5 <-
  C5.0(class ~ ., data = cls_dat, trials = 1, rules = TRUE) %>%
  butcher()

tidy_cls_rule_c5 <- tidy(fit_cls_rule_c5)

var_names <- map(tidy_cls_rule_c5$rule, ~ all.vars(parse_expr(.x)))
var_names <- sort(unique(unlist(var_names)))

rule_sizes <- map_int(tidy_cls_rule_c5$rule, characterize:::rule_size)
mean_size <- mean(rule_sizes)

exp_cls_rule_c5 <- list()
exp_cls_rule_c5$num_rules <- nrow(tidy_cls_rule_c5)
exp_cls_rule_c5$num_features_active <- length(var_names)
exp_cls_rule_c5$features_active <- var_names
exp_cls_rule_c5$mean_rule_size <- mean_size

###

set.seed(1)
fit_cls_rule_bst_c5 <-
  C5.0(class ~ ., data = cls_dat, trials = 3, rules = TRUE) %>%
  butcher()

tidy_cls_rule_bst_c5 <- tidy(fit_cls_rule_bst_c5)

var_names <- map(tidy_cls_rule_bst_c5$rule, ~ all.vars(parse_expr(.x)))
var_names <- sort(unique(unlist(var_names)))

rule_sizes <- map_int(tidy_cls_rule_bst_c5$rule, characterize:::rule_size)
mean_size <- mean(rule_sizes)

exp_cls_rule_bst_c5 <- list()
exp_cls_rule_bst_c5$num_rules <- nrow(tidy_cls_rule_bst_c5)
exp_cls_rule_bst_c5$num_features_active <- length(var_names)
exp_cls_rule_bst_c5$features_active <- var_names
exp_cls_rule_bst_c5$mean_rule_size <- mean_size

# ------------------------------------------------------------------------------
# cforest

# ------------------------------------------------------------------------------
# cubist

# ------------------------------------------------------------------------------
# earth

# ------------------------------------------------------------------------------
# fda

# ------------------------------------------------------------------------------
# formula

# ------------------------------------------------------------------------------
# gam

# ------------------------------------------------------------------------------
# gen.ridge

# ------------------------------------------------------------------------------
# glm

# ------------------------------------------------------------------------------
# glmnet

# ------------------------------------------------------------------------------
# hardhat_blueprint

# ------------------------------------------------------------------------------
# hurdle

# ------------------------------------------------------------------------------
# keras.engine.sequential.Sequential

# ------------------------------------------------------------------------------
# kknn

# ------------------------------------------------------------------------------
# ksvm

# ------------------------------------------------------------------------------
# lda

# ------------------------------------------------------------------------------
# lda_diag

# ------------------------------------------------------------------------------
# lgb.Booster

# ------------------------------------------------------------------------------
# LiblineaR

# ------------------------------------------------------------------------------
# lm

# ------------------------------------------------------------------------------
# mda

# ------------------------------------------------------------------------------
# mixo_pls

# ------------------------------------------------------------------------------
# mixo_plsda

# ------------------------------------------------------------------------------
# mixo_spls

# ------------------------------------------------------------------------------
# mixo_splsda

# ------------------------------------------------------------------------------
# multinom

# ------------------------------------------------------------------------------
# multnet

# ------------------------------------------------------------------------------
# naive_bayes

# ------------------------------------------------------------------------------
# NaiveBayes

# ------------------------------------------------------------------------------
# nnet

# ------------------------------------------------------------------------------
# nullmodel

# ------------------------------------------------------------------------------
# party

# ------------------------------------------------------------------------------
# partynode

# ------------------------------------------------------------------------------
# qda

# ------------------------------------------------------------------------------
# qda_diag

# ------------------------------------------------------------------------------
# randomForest

# ------------------------------------------------------------------------------
# ranger

# ------------------------------------------------------------------------------
# rda

# ------------------------------------------------------------------------------
# recipe

# ------------------------------------------------------------------------------
# rpart

# ------------------------------------------------------------------------------
# sda

# ------------------------------------------------------------------------------
# stanreg

# ------------------------------------------------------------------------------
# terms

# ------------------------------------------------------------------------------
# xgb.Booster

# ------------------------------------------------------------------------------
# xrf

# ------------------------------------------------------------------------------
# zeroinfl

# ------------------------------------------------------------------------------

save_names <- ls(pattern = "(^fit_)|(^exp_)")
save(list = save_names, file = "tests/testthat/test_cases.RData", compress = TRUE)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

# ------------------------------------------------------------------------------

if (!interactive()) {
  q("no")
}
