library(caret)

library(tidymodels)
library(rules)
library(baguette)
library(discrim)
library(bonsai)
library(plsmod)
library(brulee)
library(butcher)
library(rlang)
library(earth)
library(mda)


tidymodels_prefer()

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

# ------------------------------------------------------------------------------

two_class_rec <-
  recipe(class ~ ., data = cls_dat) %>%
  step_normalize(all_predictors())

# ------------------------------------------------------------------------------

knn_mod <-
  nearest_neighbor(neighbors = 5) %>%
  set_mode("classification") %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

# ------------------------------------------------------------------------------

glmn_mod <-
  workflow() %>%
  add_model(logistic_reg(penalty = 0.1) %>% set_engine("glmnet")) %>%
  add_recipe(two_class_rec) %>%
  fit(data = cls_dat) %>%
  butcher()

glmnet_0.1 <- coef(glmn_mod %>% extract_fit_engine(), s = 0.1)
exp_param_glmnet_0.1 <- sum(glmnet_0.1[,1] != 0)
exp_act_feat_glmnet_0.1 <- sum(glmnet_0.1[-1,1] != 0)

glmnet_0.01 <- coef(glmn_mod %>% extract_fit_engine(), s = 0.01)
exp_param_glmnet_0.01 <- sum(glmnet_0.01[,1] != 0)
exp_act_feat_glmnet_0.01 <- sum(glmnet_0.01[-1,1] != 0)

glmn_mtn_mod <-
  multinom_reg(penalty = 0.1) %>% set_engine("glmnet") %>%
  fit(species ~ ., data = penguins) %>%
  butcher()

glmnet_mtn_0.1 <- coef(glmn_mtn_mod %>% extract_fit_engine(), s = 0.1)
exp_param_glmnet_mtn_0.1 <- sum(map_int(glmnet_mtn_0.1, ~ sum(.x[,1] != 0)))
exp_act_feat_glmnet_mtn_0.1 <- sum(map_int(glmnet_mtn_0.1, ~ sum(.x[-1,1] != 0)))

glmnet_mtn_0.01 <- coef(glmn_mtn_mod %>% extract_fit_engine(), s = 0.01)
exp_param_glmnet_mtn_0.01 <- sum(map_int(glmnet_mtn_0.01, ~ sum(.x[,1] != 0)))
exp_act_feat_glmnet_mtn_0.01 <- sum(map_int(glmnet_mtn_0.01, ~ sum(.x[-1,1] != 0)))

# ------------------------------------------------------------------------------

rpart_mod <-
  decision_tree() %>%
  set_mode("classification") %>%
  set_engine("rpart", control = rpart::rpart.control(maxcompete = 0, maxsurrogate = 0)) %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

# ------------------------------------------------------------------------------

c5_mod <-
  decision_tree() %>%
  set_mode("classification") %>%
  set_engine("C5.0") %>%
  fit(class ~ ., data = cls_dat)

exp_act_feat_c5 <- sum(C50::C5imp(c5_mod$fit, metric = "splits")$Overall > 0)

# TODO add for different # trees

# ------------------------------------------------------------------------------

c5_rules_mod <-
  decision_tree() %>%
  set_mode("classification") %>%
  set_engine("C5.0", rules = TRUE) %>%
  fit(class ~ ., data = cls_dat)

exp_act_feat_c5_rules <- sum(C50::C5imp(c5_rules_mod$fit, metric = "splits")$Overall > 0)
# TODO add for different # trees

# ------------------------------------------------------------------------------

c5_boost_mod <-
  boost_tree() %>%
  set_mode("classification") %>%
  set_engine("C5.0") %>%
  fit(class ~ ., data = cls_dat)

exp_act_feat_c5_boost <- sum(C50::C5imp(c5_boost_mod$fit, metric = "splits")$Overall > 0)


c5_rules_boost_mod <-
  boost_tree() %>%
  set_mode("classification") %>%
  set_engine("C5.0", rules = TRUE) %>%
  fit(class ~ ., data = cls_dat)

exp_act_feat_c5_rules_boost <- sum(C50::C5imp(c5_rules_boost_mod$fit, metric = "splits")$Overall > 0)

# ------------------------------------------------------------------------------

xgb_mod <-
  boost_tree() %>%
  set_mode("classification") %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

exp_act_feat_xgb <- row(xgboost::xgb.importance(model = xgb_mod$fit))
exp_act_feat_xgb_3 <- row(xgboost::xgb.importance(model = xgb_mod$fit, trees = 0:3))
exp_act_feat_xgb_9 <- row(xgboost::xgb.importance(model = xgb_mod$fit, trees = 0:9))

# ------------------------------------------------------------------------------

svm_mod <-
  svm_rbf() %>%
  set_mode("classification") %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

# ------------------------------------------------------------------------------

nnet_mod <-
  mlp() %>%
  set_mode("classification") %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

exp_param_nnet <- length(coef(nnet_mod$fit))

mtn_mod <-
  multinom_reg() %>%
  fit(species ~ ., data = penguins) %>%
  butcher()

exp_param_mtn <- length(coef(mtn_mod$fit))

# ------------------------------------------------------------------------------

brulee_mlp_mod <-
  mlp() %>%
  set_mode("classification") %>%
  set_engine("brulee") %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

exp_param_brulee_mlp <- length(unlist(brulee_mlp_mod$fit$estimates[[1]]))

# ------------------------------------------------------------------------------

xrf_mod <-
  rule_fit(trees = 10, penalty = 0.1, learn_rate = .1) %>%
  set_mode("classification") %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

# TODO I don't think that they tidy method is working correctly
xrf_tidy_0.1 <-
  tidy(xrf_mod, penalty = 0.1) %>%
  mutate(
    rule_expr = map(rule, ~ rlang::parse_expr(.x)),
    num_pred = map_int(rule_expr, ~ length(all.vars(.x)))
  )
is_rule_0.1 <- grepl("^r[0-9]", xrf_tidy_0.1$rule_id)
exp_rules_xrf_0.1 <- sum(is_rule_0.1)
exp_rule_size_xrf_0.1 <- mean(xrf_tidy_0.1$num_pred[is_rule_0.1])

xrf_tidy_0.001 <-
  tidy(xrf_mod, penalty = 0.001) %>%
  mutate(
    rule_expr = map(rule, ~ rlang::parse_expr(.x)),
    num_pred = map_int(rule_expr, ~ length(all.vars(.x)))
  )
is_rule_0.001 <- grepl("^r[0-9]", xrf_tidy_0.001$rule_id)
exp_rules_xrf_0.001 <- sum(is_rule_0.001)
exp_rule_size_xrf_0.001 <- mean(xrf_tidy_0.001$num_pred[is_rule_0.001])


# ------------------------------------------------------------------------------

ranger_mod <-
  rand_forest(trees = 3) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = 'impurity') %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

# ------------------------------------------------------------------------------

cubist_mod <-
  cubist_rules(committees = 2) %>%
  fit(Sale_Price ~ ., data = ames) %>%
  butcher()

cb_tidy_2 <-
  cubist_mod %>%
  tidy() %>%
  mutate(
    rule_expr = map(rule, rlang::parse_expr),
    num_pred = map_int(rule_expr, ~ length(all.vars(.x)))
  )

exp_num_prm_cb_2 <-
  cb_tidy_2 %>%
  select(estimate) %>%
  unnest(cols = estimate) %>%
  nrow()
exp_rule_size_cb_2 <- mean(cb_tidy_2$num_pred)
exp_num_rules_cb_2 <- nrow(cb_tidy_2)

cb_tidy_1 <-
  cubist_mod %>%
  tidy() %>%
  filter(committee == 1) %>%
  mutate(
    rule_expr = map(rule, rlang::parse_expr),
    num_pred = map_int(rule_expr, ~ length(all.vars(.x)))
  )

exp_num_prm_cb_1 <-
  cb_tidy_1 %>%
  select(estimate) %>%
  unnest(cols = estimate) %>%
  nrow()
exp_rule_size_cb_1 <- mean(cb_tidy_1$num_pred)
exp_num_rules_cb_1 <- nrow(cb_tidy_1)

# ------------------------------------------------------------------------------

earth_mod <-
  mars() %>%
  set_mode("regression") %>%
  fit(outcome ~ ., data = reg_dat) %>%
  butcher()

earth_cls_mod <-
  mars() %>%
  set_mode("classification") %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

exp_act_feat_earth_reg <- nrow(earth::evimp(earth_mod$fit))
exp_act_feat_earth_cls <- nrow(earth::evimp(earth_cls_mod$fit))
exp_num_prm_earth_reg <- length(coef(earth_mod$fit))
exp_num_prm_earth_cls <- length(coef(earth_cls_mod$fit))

# ------------------------------------------------------------------------------

bart_mod <-
  bart(trees = 5) %>%
  set_mode("regression") %>%
  fit(outcome ~ ., data = reg_dat) %>%
  butcher()

# ------------------------------------------------------------------------------

bag_cart_mod <-
  bag_tree() %>%
  set_mode("classification") %>%
  set_engine("rpart", times = 3L) %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

bag_c5_mod <-
  bag_tree() %>%
  set_mode("classification") %>%
  set_engine("C5.0", times = 3L) %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

bag_mars_reg_mod <-
  bag_mars() %>%
  set_mode("regression") %>%
  set_engine("earth", times = 3L) %>%
  fit(outcome ~ ., data = reg_dat) %>%
  butcher()

# ------------------------------------------------------------------------------

ctree_mod <-
  decision_tree() %>%
  set_mode("classification") %>%
  set_engine("partykit") %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

cforest_mod <-
  rand_forest(trees = 5) %>%
  set_mode("classification") %>%
  set_engine("partykit") %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

# ------------------------------------------------------------------------------
# TODO add multivariate model?

pls_mod <-
  parsnip::pls(num_comp = 3) %>%
  set_mode("regression") %>%
  fit(outcome ~ ., data = reg_dat) %>%
  butcher()

plsda_mod <-
  parsnip::pls(num_comp = 3) %>%
  set_mode("classification") %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

spls_mod <-
  parsnip::pls(num_comp = 3, predictor_prop = 1 / 10) %>%
  set_mode("regression") %>%
  fit(outcome ~ ., data = reg_dat) %>%
  butcher()

splsda_mod <-
  parsnip::pls(num_comp = 3, predictor_prop = 1 / 10) %>%
  set_mode("classification") %>%
  fit(class ~ ., data = cls_dat) %>%
  butcher()

# ------------------------------------------------------------------------------

mods <- ls(pattern = "(_mod)|(^exp_)")

save(
  list = mods,
  version = 2,
  file = "tests/testthat/test_cases.RData",
  compress = TRUE, compression_level = 9
)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

q("no")
