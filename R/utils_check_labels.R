.reset_comparison_labels <- function(contr_mat, coding_fx = NULL) {
  # Early exit if this isn't a valid contrast matrix
  .is_valid_contrmat(contr_mat)
  n <- nrow(contr_mat)
  levelnames <- rownames(contr_mat)



  # Default column names will be the rows with positive values
  default_colnames <- unname(apply(contr_mat, 2L,
                                   \(x) rev(levelnames[x > 0L])[1L]))


  if (is.null(coding_fx)) {
    # Need to explicitly check if this is polynomial coding since the labels
    # for that one are handled differently
    if (n <= 95 && identical(unname(contr_mat),
                             unname(stats::contr.poly(n)),
                             ignore.environment = TRUE)) {
      colnames(contr_mat) <- colnames(stats::contr.poly(n))
      return(contr_mat)
    }

    colnames(contr_mat) <- default_colnames
    return(contr_mat)
  }

  colnames(contr_mat) <-
    switch(coding_fx,
           "polynomial_code"      = colnames(polynomial_code(n)),
           "orth_polynomial_code" = colnames(polynomial_code(n)),
           "raw_polynomial_code"  = colnames(polynomial_code(n)),
           "contr.poly"           = colnames(polynomial_code(n)),
           "contr.helmert"        = paste0("(<", levelnames[-1L], ")/", seq_along(levelnames)[-1L]),
           "helmert_code"         = paste0("<", levelnames[-1L]),
           "reverse_helmert_code" = paste0(">", levelnames[-n]),
           "backward_difference_code" =  paste(levelnames[-1], levelnames[-n], sep = "-"),
           "forward_difference_code"  =  paste(levelnames[-n], levelnames[-1], sep = "-"),
           "cumulative_split_code" = vapply(seq_len(length(levelnames)-1L),
                                            \(i) paste0(levelnames[i],
                                                        "|",
                                                        levelnames[i+1L]),
                                            character(1)),
           default_colnames
    )

  contr_mat
}
