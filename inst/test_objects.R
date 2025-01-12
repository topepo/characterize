pkgs <- c(
  "baguette",
  "bonsai",
  "brulee",
  "bundle",
  "butcher",
  "C50",
  "caret",
  "Cubist",
  "dbarts",
  "discrim",
  "earth",
  "glmnet",
  "kknn",
  "klaR",
  "lightgbm",
  "lobstr",
  "MASS",
  "mda",
  "mgcv",
  "mixOmics",
  "naivebayes",
  "nnet",
  "plsmod",
  "pscl",
  "randomForest",
  "ranger",
  "rlang",
  "rpart",
  "rstanarm",
  "rules",
  "sda",
  "sessioninfo",
  "sparsediscrim",
  "tidymodels",
  "xgboost",
  "xrf"
)

load_pkg <- function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))
}

load_res <- lapply(pkgs, load_pkg)

# These modeling packages are in Suggests so we can create them in the tests:
# earth, kernlab, partykit, rpart
# The test data below are mirrored in testthat/tests/helpers.R

# ------------------------------------------------------------------------------

tidymodels_prefer()

# ------------------------------------------------------------------------------

# Model fits begin with `fit_`.
# Expected results lists start with `exp_`. There are only created when one of
# the 'engine' packages is required to compute the results.

# ------------------------------------------------------------------------------

data(ames, package = "modeldata")
ames$Sale_Price <- log10(ames$Sale_Price)
ames <- ames %>%
  mutate(Sale_Price <- log10(Sale_Price)) %>%
  slice(1:100) %>%
  select(Sale_Price, Neighborhood, Longitude, Latitude)

data("penguins", package = "modeldata")
penguins <- penguins[complete.cases(penguins), ]

set.seed(1)
cls_dat <- sim_classification(50)
reg_dat <- sim_regression(50)
mnl_dat <- sim_multinomial(
  100,
  ~-0.5 + 0.6 * abs(A),
  ~ifelse(A > 0 & B > 0, 1.0 + 0.2 * A / B, -2),
  ~-0.6 * A + 0.50 * B - A * B
)

count_dat <- reg_dat
count_dat$outcome <- rpois(nrow(reg_dat), exp(reg_dat$outcome / 10))

reg_x <- as.matrix(reg_dat[, -1])
cls_x <- as.matrix(cls_dat[, -1])
mnl_x <- as.matrix(mnl_dat[, -3])

penalties <- 10^(-1:-3)

# ------------------------------------------------------------------------------
# bagger

## C5.0

set.seed(1)
fit_cls_bag_c5 <- bagger(
  class ~ .,
  data = cls_dat,
  base_model = "C5.0",
  times = 3
) %>%
  butcher()

# The number of terminal nodes and active predictors computed with tidy method in
# the rules pkgs

tidy_cls_bag_c5 <- purrr::map(fit_cls_bag_c5$model_df$model, tidy) %>%
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
fit_cls_bag_rpart <- bagger(
  class ~ .,
  data = cls_dat,
  base_model = "CART",
  times = 3
) %>%
  butcher()

set.seed(1)
fit_reg_bag_rpart <- bagger(
  Sale_Price ~ .,
  data = ames,
  base_model = "CART",
  times = 3
) %>%
  butcher()

## mars

set.seed(1)
fit_cls_bag_mars <- bagger(
  class ~ .,
  data = cls_dat,
  base_model = "MARS",
  times = 3
) %>%
  butcher()

set.seed(1)
fit_reg_bag_mars <- bagger(
  Sale_Price ~ .,
  data = ames,
  base_model = "MARS",
  times = 3
) %>%
  butcher()

## nnet

set.seed(1)
fit_cls_bag_nnet <- bagger(
  class ~ .,
  data = cls_dat,
  base_model = "nnet",
  times = 3
) %>%
  butcher()

set.seed(1)
fit_reg_bag_nnet <- bagger(
  outcome ~ .,
  data = reg_dat,
  base_model = "nnet",
  times = 3
) %>%
  butcher()

# ------------------------------------------------------------------------------
# bart

# TODO stop butcher from removing `varcount`; needed for 'num_term_nodes'

set.seed(1)
fit_cls_dbart <- dbarts::bart(
  cls_dat[-1],
  y.train = cls_dat$class,
  verbose = FALSE
)
lobstr::obj_size(fit_cls_dbart)

# we need 'varcount' to measure the number of terminal nodes but butcher removes it
cls_varcount <- fit_cls_dbart$varcount
fit_cls_dbart <- butcher(fit_cls_dbart)
lobstr::obj_size(fit_cls_dbart)

fit_cls_dbart$varcount <- cls_varcount
lobstr::obj_size(fit_cls_dbart)

set.seed(1)
fit_reg_dbart <- dbarts::bart(
  reg_dat[-1],
  y.train = reg_dat$outcome,
  verbose = FALSE
)

reg_varcount <- fit_reg_dbart$varcount
fit_reg_dbart <- butcher(fit_reg_dbart)
fit_reg_dbart$varcount <- reg_varcount

# ------------------------------------------------------------------------------
# brulee_linear_reg

set.seed(12)
fit_reg_bru_lin <- brulee_linear_reg(outcome ~ ., data = reg_dat) %>%
  butcher()

# ------------------------------------------------------------------------------
# brulee_logistic_reg

set.seed(12)
fit_cls_bru_logit <- brulee_logistic_reg(
  class ~ .,
  data = cls_dat,
  hidden_units = 3
) %>%
  butcher()

# ------------------------------------------------------------------------------
# brulee_mlp

set.seed(12)
fit_cls_bru_mlp <- brulee_mlp(class ~ ., data = cls_dat, hidden_units = 3) %>%
  butcher()

set.seed(12)
fit_reg_bru_mlp <- brulee_mlp(outcome ~ ., data = reg_dat, hidden_units = 3) %>%
  butcher()

# ------------------------------------------------------------------------------
# brulee_multinomial_reg

set.seed(12)
fit_cls_bru_multi <- brulee_multinomial_reg(class ~ ., data = mnl_dat) %>%
  butcher()

# ------------------------------------------------------------------------------
# C5.0

set.seed(1)
fit_cls_tree_c5 <- C5.0(class ~ ., data = cls_dat, trials = 1) %>%
  butcher()

set.seed(1)
fit_cls_tree_c5_3 <- C5.0(class ~ ., data = cls_dat, trials = 3) %>%
  butcher()

# The number of terminal nodes and active predictors computed with tidy method in
# the rules pkgs

tidy_cls_tree_c5 <- tidy(fit_cls_tree_c5)

var_names <- map(tidy_cls_tree_c5$rule, ~all.vars(parse_expr(.x)))
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
fit_cls_tree_bst_c5 <- C5.0(class ~ ., data = cls_dat, trials = 3) %>%
  butcher()

tidy_cls_tree_bst_c5 <- tidy(fit_cls_tree_bst_c5)

var_names <- map(tidy_cls_tree_bst_c5$rule, ~all.vars(parse_expr(.x)))
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
fit_cls_rule_c5 <- C5.0(class ~ ., data = cls_dat, trials = 1, rules = TRUE) %>%
  butcher()

tidy_cls_rule_c5 <- tidy(fit_cls_rule_c5)

var_names <- map(tidy_cls_rule_c5$rule, ~all.vars(parse_expr(.x)))
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
fit_cls_rule_bst_c5 <- C5.0(
  class ~ .,
  data = cls_dat,
  trials = 3,
  rules = TRUE
) %>%
  butcher()

tidy_cls_rule_bst_c5 <- tidy(fit_cls_rule_bst_c5)

var_names <- map(tidy_cls_rule_bst_c5$rule, ~all.vars(parse_expr(.x)))
var_names <- sort(unique(unlist(var_names)))

rule_sizes <- map_int(tidy_cls_rule_bst_c5$rule, characterize:::rule_size)
mean_size <- mean(rule_sizes)

exp_cls_rule_bst_c5 <- list()
exp_cls_rule_bst_c5$num_rules <- nrow(tidy_cls_rule_bst_c5)
exp_cls_rule_bst_c5$num_features_active <- length(var_names)
exp_cls_rule_bst_c5$features_active <- var_names
exp_cls_rule_bst_c5$mean_rule_size <- mean_size

# ------------------------------------------------------------------------------
# cubist

set.seed(1)
fit_reg_cb <- cubist(ames[, -1], ames$Sale_Price, committees = 1) %>%
  butcher()

set.seed(1)
fit_reg_cb_10 <- cubist(ames[, -1], ames$Sale_Price, committees = 10) %>%
  butcher()

# The number of terminal nodes and active predictors computed with tidy method in
# the rules pkgs

tidy_reg_cb <- tidy(fit_reg_cb)

var_names_1 <- map(tidy_reg_cb$rule, ~all.vars(parse_expr(.x)))
var_names_1 <- sort(unique(unlist(var_names_1)))

var_names_2 <- map(tidy_reg_cb$estimate, ~.x %>% pluck("term")) %>%
  unlist() %>%
  unique()

var_names_2 <- var_names_2[var_names_2 != "(Intercept)"]
var_names <- sort(unique(c(var_names_1, var_names_2)))

num_param <- sum(map_int(tidy_reg_cb$estimate, nrow))

rule_sizes <- map_int(tidy_reg_cb$rule, characterize:::rule_size)
mean_size <- mean(rule_sizes)

exp_reg_cb <- list()
exp_reg_cb$num_rules <- nrow(tidy_reg_cb)
exp_reg_cb$num_features_active <- length(var_names)
exp_reg_cb$features_active <- var_names
exp_reg_cb$mean_rule_size <- mean_size
exp_reg_cb$num_param <- num_param

###

set.seed(1)
fit_reg_cb_ens <- cubist(ames[, -1], ames$Sale_Price, committees = 3) %>%
  butcher()

# The number of terminal nodes and active predictors computed with tidy method in
# the rules pkgs

tidy_reg_cb_ens <- tidy(fit_reg_cb_ens)

var_names_1 <- map(tidy_reg_cb_ens$rule, ~all.vars(parse_expr(.x)))
var_names_1 <- sort(unique(unlist(var_names_1)))

var_names_2 <- map(tidy_reg_cb_ens$estimate, ~.x %>% pluck("term")) %>%
  unlist() %>%
  unique()

var_names_2 <- var_names_2[var_names_2 != "(Intercept)"]
var_names <- sort(unique(c(var_names_1, var_names_2)))

num_param <- sum(map_int(tidy_reg_cb_ens$estimate, nrow))

rule_sizes <- map_int(tidy_reg_cb_ens$rule, characterize:::rule_size)
mean_size <- mean(rule_sizes)

exp_reg_cb_ens <- list()
exp_reg_cb_ens$num_rules <- nrow(tidy_reg_cb_ens)
exp_reg_cb_ens$num_features_active <- length(var_names)
exp_reg_cb_ens$features_active <- var_names
exp_reg_cb_ens$mean_rule_size <- mean_size
exp_reg_cb_ens$num_param <- num_param

# ------------------------------------------------------------------------------
# fda

set.seed(1)
fit_cls_fda_earth <- mda::fda(
  class ~ .,
  data = cls_dat,
  method = earth::earth
) %>%
  butcher()

set.seed(1)
fit_cls_fda_poly <- mda::fda(class ~ ., data = cls_dat) %>%
  butcher()

# ------------------------------------------------------------------------------
# gam

set.seed(1)
fit_reg_gam <- gam(
  Sale_Price ~ Neighborhood + s(Longitude) + Latitude,
  data = ames
) %>%
  butcher()

set.seed(1)
fit_cls_gam <- gam(
  class ~
    two_factor_1 +
      two_factor_2 +
      s(non_linear_1) +
      s(non_linear_2, non_linear_3),
  data = cls_dat,
  family = binomial
) %>%
  butcher()

# ------------------------------------------------------------------------------
# gen.ridge

set.seed(1)
fit_reg_gen_ridge <- gen.ridge(as.matrix(reg_dat[, -1]), reg_dat$outcome) %>%
  butcher()

# ------------------------------------------------------------------------------
# glm

fit_reg_glm <- glm(Sale_Price ~ Neighborhood + Longitude, data = ames) %>%
  butcher()

fit_cls_glm <- glm(class ~ ., data = cls_dat, family = binomial) %>%
  butcher()

# ------------------------------------------------------------------------------
# glmnet

fit_reg_glmnet <- glmnet(reg_x, reg_dat$outcome, lambda = penalties) %>%
  butcher()

exp_reg_glmnet <- fit_reg_glmnet %>%
  tidy()

##

fit_cls_glmnet <- glmnet(
  cls_x,
  cls_dat$class,
  lambda = penalties,
  family = "binomial"
) %>%
  butcher()

exp_cls_glmnet <- fit_cls_glmnet %>%
  tidy()

##

fit_mnl_glmnet <- glmnet(
  mnl_x,
  mnl_dat$class,
  lambda = penalties,
  family = "multinomial"
) %>%
  butcher()

exp_mnl_glmnet <- fit_mnl_glmnet %>%
  tidy()

##

fit_count_glmnet <- glmnet(
  reg_x,
  count_dat$outcome,
  lambda = penalties,
  family = "poisson"
) %>%
  butcher()

exp_count_glmnet <- fit_count_glmnet %>%
  tidy()

# ------------------------------------------------------------------------------
# hurdle

fit_reg_hurdle <- hurdle(outcome ~ ., data = count_dat) %>%
  butcher()

# ------------------------------------------------------------------------------
# keras.engine.sequential.Sequential

# ------------------------------------------------------------------------------
# kknn

conflicted::conflict_prefer("contr.dummy", "kknn")

fit_reg_kknn <- kknn::kknn(Sale_Price ~ ., train = ames, ames) %>%
  butcher()

fit_cls_kknn <- kknn::kknn(class ~ ., train = cls_dat, cls_dat) %>%
  butcher()

fit_mnl_kknn <- kknn::kknn(class ~ ., train = mnl_dat, mnl_dat) %>%
  butcher()

# ------------------------------------------------------------------------------
# lda

fit_cls_lda <- lda(class ~ ., data = cls_dat) %>%
  butcher()

fit_mnl_lda <- lda(mnl_dat[, -3], grouping = mnl_dat$class) %>%
  butcher()

# ------------------------------------------------------------------------------
# lda_diag

fit_cls_lda_diag <- lda_diag(class ~ ., data = cls_dat) %>%
  butcher()

fit_mnl_lda_diag <- lda_diag(mnl_dat[, -3], y = mnl_dat$class) %>%
  butcher()

# ------------------------------------------------------------------------------
# lgb.Booster

# TODO add exp results here

set.seed(1)
fit_reg_lightgbm <- train_lightgbm(
  reg_dat[, -1],
  reg_dat$outcome,
  quiet = TRUE
) %>%
  butcher() %>%
  bundle()

set.seed(1)
fit_cls_lightgbm <- train_lightgbm(
  cls_dat[, -1],
  cls_dat$class,
  quiet = TRUE
) %>%
  butcher() %>%
  bundle()

set.seed(1)
fit_mnl_lightgbm <- train_lightgbm(
  mnl_dat[, -3],
  mnl_dat$class,
  quiet = TRUE
) %>%
  butcher() %>%
  bundle()

# ------------------------------------------------------------------------------
# LiblineaR

# ------------------------------------------------------------------------------
# lm

fit_reg_lm <- lm(Sale_Price ~ Neighborhood + Longitude, data = ames) %>%
  butcher()

# ------------------------------------------------------------------------------
# mda

fit_cls_mda <- mda::mda(class ~ ., data = cls_dat) %>%
  butcher()

fit_mnl_mda <- mda::mda(class ~ ., data = mnl_dat) %>%
  butcher()

# ------------------------------------------------------------------------------
# mixo_pls

fit_reg_pls <- mixOmics::pls(reg_dat[, -1], reg_dat$outcome, ncomp = 2) %>%
  butcher()

# ------------------------------------------------------------------------------
# mixo_plsda

fit_cls_plsda <- mixOmics::plsda(cls_dat[, -1], cls_dat$class, ncomp = 2) %>%
  butcher()

fit_mnl_plsda <- mixOmics::plsda(mnl_dat[, -3], mnl_dat$class, ncomp = 2) %>%
  butcher()

# ------------------------------------------------------------------------------
# mixo_spls

fit_reg_spls <- mixOmics::spls(
  reg_dat[, -1],
  reg_dat$outcome,
  ncomp = 2,
  keepX = rep(3, 20)
) %>%
  butcher()

# ------------------------------------------------------------------------------
# mixo_splsda

fit_cls_splsda <- mixOmics::splsda(cls_dat[, -1], cls_dat$class, ncomp = 2) %>%
  butcher()

fit_mnl_splsda <- mixOmics::splsda(mnl_dat[, -3], mnl_dat$class, ncomp = 2) %>%
  butcher()

# ------------------------------------------------------------------------------
# multinom

fit_cls_multinom <- nnet::multinom(class ~ ., data = mnl_dat, trace = FALSE) %>%
  butcher()

# ------------------------------------------------------------------------------
# naive_bayes

fit_cls_naive_bayes <- naive_bayes(class ~ ., data = cls_dat) %>%
  butcher()

fit_mnl_naive_bayes <- naive_bayes(mnl_dat[, -3], y = mnl_dat$class) %>%
  butcher()

# ------------------------------------------------------------------------------
# NaiveBayes

fit_cls_NaiveBayes <- klaR::NaiveBayes(class ~ ., data = cls_dat) %>%
  butcher()

fit_mnl_NaiveBayes <- klaR::NaiveBayes(
  mnl_dat[, -3],
  grouping = mnl_dat$class
) %>%
  butcher()

# ------------------------------------------------------------------------------
# nnet

fit_reg_nnet <- nnet(Sale_Price ~ ., data = ames, trace = FALSE, size = 2) %>%
  butcher()

fit_cls_nnet <- nnet(class ~ ., data = cls_dat, trace = FALSE, size = 2) %>%
  butcher()

fit_mnl_nnet <- nnet(class ~ ., data = mnl_dat, trace = FALSE, size = 2) %>%
  butcher()

# ------------------------------------------------------------------------------
# nullmodel

# ------------------------------------------------------------------------------
# parsnip model fits

fit_mnl_glmnet_spec <- multinom_reg(penalty = 0.1) %>%
  set_engine("glmnet", path_values = penalties) %>%
  set_mode("classification")

fit_mnl_glmnet_parsnip <- fit_mnl_glmnet_spec %>%
  fit(class ~ ., data = mnl_dat) %>%
  butcher()

exp_mnl_glmnet_parsnip <- fit_mnl_glmnet_parsnip %>%
  extract_fit_engine() %>%
  tidy()

###

fit_reg_glmnet_spec <- linear_reg(penalty = 0.1) %>%
  set_engine("glmnet", path_values = penalties) %>%
  set_mode("regression")

fit_reg_glmnet_parsnip <- fit_reg_glmnet_spec %>%
  fit(outcome ~ ., data = reg_dat) %>%
  butcher()

exp_reg_glmnet_parsnip <- fit_reg_glmnet_parsnip %>%
  extract_fit_engine() %>%
  tidy()

# ------------------------------------------------------------------------------
# qda

fit_cls_qda <- qda(class ~ ., data = cls_dat) %>%
  butcher()

fit_mnl_qda <- qda(mnl_dat[, -3], grouping = mnl_dat$class) %>%
  butcher()

# ------------------------------------------------------------------------------
# qda_diag

fit_cls_qda_diag <- qda_diag(class ~ ., data = cls_dat) %>%
  butcher()

fit_mnl_qda_diag <- qda_diag(mnl_dat[, -3], y = mnl_dat$class) %>%
  butcher()

# ------------------------------------------------------------------------------
# randomForest

fit_reg_randomForest <- randomForest(Sale_Price ~ ., data = ames, ntree = 3) %>%
  butcher()

fit_cls_randomForest <- randomForest(class ~ ., data = cls_dat, ntree = 3) %>%
  butcher()

fit_mnl_randomForest <- randomForest(class ~ ., data = mnl_dat, ntree = 3) %>%
  butcher()

# ------------------------------------------------------------------------------
# ranger

fit_reg_ranger <- ranger(Sale_Price ~ ., data = ames, num.trees = 3) %>%
  butcher()

fit_cls_ranger <- ranger(class ~ ., data = cls_dat, num.trees = 3) %>%
  butcher()

fit_mnl_ranger <- ranger(class ~ ., data = mnl_dat, num.trees = 3) %>%
  butcher()

# ------------------------------------------------------------------------------
# rda

fit_cls_rda <- klaR::rda(class ~ ., data = cls_dat) %>%
  butcher()

fit_mnl_rda <- klaR::rda(mnl_dat[, -3], grouping = mnl_dat$class) %>%
  butcher()

# ------------------------------------------------------------------------------
# sda

fit_cls_sda <- sda::sda(cls_x, cls_dat$class, verbose = FALSE) %>%
  butcher()

fit_mnl_sda <- sda::sda(mnl_x, mnl_dat$class, verbose = FALSE) %>%
  butcher()

# ------------------------------------------------------------------------------
# stanreg

fit_reg_stan_glm <- stan_glm(
  Sale_Price ~ Neighborhood + Longitude,
  data = ames,
  chains = 2,
  iter = 100,
  refresh = 0
) %>%
  butcher()

fit_cls_stan_glm <- stan_glm(
  class ~ .,
  data = cls_dat,
  family = binomial,
  chains = 2,
  iter = 100,
  refresh = 0
) %>%
  butcher()

# ------------------------------------------------------------------------------
# fitted workflows

fit_mnl_glmnet_wflow <- workflow(class ~ ., fit_mnl_glmnet_spec) %>%
  fit(data = mnl_dat) %>%
  butcher()

exp_mnl_glmnet_wflow <- fit_mnl_glmnet_wflow %>%
  extract_fit_engine() %>%
  tidy()

###

ames_rec <- recipe(Sale_Price ~ ., data = ames) %>%
  step_dummy(all_factor_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

fit_reg_glmnet_wflow <- workflow(ames_rec, fit_reg_glmnet_spec) %>%
  fit(data = ames) %>%
  butcher()

exp_reg_glmnet_wflow <- fit_reg_glmnet_wflow %>%
  extract_fit_engine() %>%
  tidy()

# ------------------------------------------------------------------------------
# xgb.Booster

# TODO add exp results here

set.seed(1)
fit_reg_xgboost <- xgb_train(reg_dat[, -1], reg_dat$outcome, nrounds = 3) %>%
  butcher() %>%
  bundle()

set.seed(1)
fit_cls_xgboost <- xgb_train(cls_dat[, -1], cls_dat$class, nrounds = 3) %>%
  butcher() %>%
  bundle()

set.seed(1)
fit_mnl_xgboost <- xgb_train(mnl_dat[, -3], mnl_dat$class, nrounds = 3) %>%
  butcher() %>%
  bundle()

# ------------------------------------------------------------------------------
# xrf

# TODO add exp results here (or maybe not?)

set.seed(1)
fit_reg_xrf <- xrf(
  Sale_Price ~ .,
  data = ames,
  family = "gaussian",
  xgb_control = list(nrounds = 50, max_depth = 3)
) %>%
  butcher() %>%
  bundle()

set.seed(1)
fit_cls_xrf <- xrf(
  class ~ .,
  data = cls_dat,
  family = "binomial",
  xgb_control = list(nrounds = 50, max_depth = 3)
) %>%
  butcher() %>%
  bundle()

set.seed(1)
fit_mnl_xrf <- xrf(
  class ~ .,
  data = mnl_dat,
  family = "multinomial",
  xgb_control = list(nrounds = 50, max_depth = 3, num_class = 3)
) %>%
  butcher() %>%
  bundle()

# ------------------------------------------------------------------------------
# zeroinfl

fit_reg_zeroinfl <- zeroinfl(
  outcome ~ predictor_06 + predictor_07 | predictor_01 + predictor_02,
  data = count_dat
) %>%
  butcher()

# ------------------------------------------------------------------------------

save_names <- ls(pattern = "(^fit_)|(^exp_)")
save(
  list = save_names,
  file = "tests/testthat/test_cases.RData",
  compress = "bzip2"
)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

# ------------------------------------------------------------------------------

if (!interactive()) {
  q("no")
}
