# cubist

    Code
      characterize(cubist_mod)
    Output
      # A tibble: 4 x 3
        .metric             .estimator .estimate
        <chr>               <chr>          <dbl>
      1 num_features_active model           3   
      2 num_parameters      model          13   
      3 num_rules           model           6   
      4 mean_rule_size      model           1.33

---

    Code
      characterize(cubist_mod, committees = 1)
    Output
      # A tibble: 4 x 3
        .metric             .estimator .estimate
        <chr>               <chr>          <dbl>
      1 num_features_active model            3  
      2 num_parameters      model            9  
      3 num_rules           model            4  
      4 mean_rule_size      model            1.5

