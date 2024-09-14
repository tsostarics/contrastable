# Readme example unchanged

    Code
      glimpse_contrasts(set_contrasts(my_data, contrast_schemes, verbose = FALSE),
      contrast_schemes, add_namespace = TRUE, show_all_factors = TRUE, verbose = FALSE)
    Output
        factor n  level_names                        scheme reference  intercept
      1    cyl 3      4, 6, 8 contrastable::scaled_sum_code         6 grand mean
      2   carb 6 1, 2, 3,....    contrastable::helmert_code      <NA> grand mean
      3     vs 2         0, 1  contrastable::treatment_code         1    mean(1)
      4   gear 3      3, 4, 5             stats::contr.poly      <NA> grand mean

