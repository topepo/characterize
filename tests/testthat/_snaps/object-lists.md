# method descriptions

    Code
      as.data.frame(list_characteristics())
    Output
              characteristic      object
      1      active_features        C5.0
      2      active_features      bagger
      3      active_features        bart
      4      active_features     cforest
      5      active_features      cubist
      6      active_features       earth
      7      active_features      glmnet
      8      active_features lgb.Booster
      9      active_features    mixo_pls
      10     active_features  mixo_plsda
      11     active_features   mixo_spls
      12     active_features mixo_splsda
      13     active_features       party
      14     active_features      ranger
      15     active_features       rpart
      16     active_features       terms
      17     active_features xgb.Booster
      18     active_features         xrf
      19      mean_rule_size        C5.0
      20      mean_rule_size      cubist
      21      mean_rule_size         xrf
      22 num_active_features        C5.0
      23 num_active_features      cubist
      24 num_active_features      glmnet
      25 num_active_features lgb.Booster
      26 num_active_features         xrf
      27      num_parameters      bagger
      28      num_parameters  brulee_mlp
      29      num_parameters      cubist
      30      num_parameters       earth
      31      num_parameters      glmnet
      32      num_parameters    mixo_pls
      33      num_parameters  mixo_plsda
      34      num_parameters   mixo_spls
      35      num_parameters mixo_splsda
      36      num_parameters    multinom
      37      num_parameters        nnet
      38      num_parameters         xrf
      39           num_rules        C5.0
      40           num_rules      cubist
      41           num_rules         xrf
      42 num_support_vectors        ksvm
      43      num_term_nodes        C5.0
      44      num_term_nodes      bagger
      45      num_term_nodes        bart
      46      num_term_nodes     cforest
      47      num_term_nodes lgb.Booster
      48      num_term_nodes       party
      49      num_term_nodes   partynode
      50      num_term_nodes      ranger
      51      num_term_nodes       rpart
      52      num_term_nodes xgb.Booster

---

    Code
      object_list()
    Output
      [1] "`C5.0`, `bagger`, `bart`, `brulee_mlp`, `cforest`, `cubist`, `earth`, `glmnet`, `ksvm`, `lgb.Booster`, `mixo_pls`, `mixo_plsda`, `mixo_spls`, `mixo_splsda`, `multinom`, `nnet`, `party`, `partynode`, `ranger`, `rpart`, `terms`, `xgb.Booster`, `xrf`"

