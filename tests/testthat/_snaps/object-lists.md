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
      8      active_features    mixo_pls
      9      active_features  mixo_plsda
      10     active_features   mixo_spls
      11     active_features mixo_splsda
      12     active_features       party
      13     active_features      ranger
      14     active_features       rpart
      15     active_features       terms
      16     active_features xgb.Booster
      17     active_features         xrf
      18      mean_rule_size        C5.0
      19      mean_rule_size      cubist
      20      mean_rule_size         xrf
      21 num_active_features        C5.0
      22 num_active_features      cubist
      23 num_active_features      glmnet
      24 num_active_features         xrf
      25      num_parameters      bagger
      26      num_parameters      cubist
      27      num_parameters       earth
      28      num_parameters      glmnet
      29      num_parameters    mixo_pls
      30      num_parameters  mixo_plsda
      31      num_parameters   mixo_spls
      32      num_parameters mixo_splsda
      33      num_parameters    multinom
      34      num_parameters        nnet
      35      num_parameters         xrf
      36           num_rules        C5.0
      37           num_rules      cubist
      38           num_rules         xrf
      39 num_support_vectors        ksvm
      40      num_term_nodes        C5.0
      41      num_term_nodes      bagger
      42      num_term_nodes        bart
      43      num_term_nodes     cforest
      44      num_term_nodes       party
      45      num_term_nodes   partynode
      46      num_term_nodes      ranger
      47      num_term_nodes       rpart
      48      num_term_nodes xgb.Booster

---

    Code
      object_list()
    Output
      [1] "`C5.0`, `bagger`, `bart`, `cforest`, `cubist`, `earth`, `glmnet`, `ksvm`, `mixo_pls`, `mixo_plsda`, `mixo_spls`, `mixo_splsda`, `multinom`, `nnet`, `party`, `partynode`, `ranger`, `rpart`, `terms`, `xgb.Booster`, `xrf`"

