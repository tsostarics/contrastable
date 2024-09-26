# Check that implementation of contrast functions remains the same

    Code
      lapply(schemes, function(s) {
        fx <- sym(s)
        cmat <- rlang::inject(enlist_contrasts(tstdata_2, x ~ !!fx))
        as.character(MASS::fractions(cmat[[1L]]))
      })
    Output
      $sum_code
        2   
      1 "-1"
      2 "1" 
      
      $treatment_code
        2  
      1 "0"
      2 "1"
      
      $scaled_sum_code
        2     
      1 "-1/2"
      2 "1/2" 
      
      $cumulative_split_code
        1|2   
      1 "-1/2"
      2 "1/2" 
      
      $helmert_code
        <2    
      1 "-1/2"
      2 "1/2" 
      
      $reverse_helmert_code
        >1    
      1 "-1/2"
      2 "1/2" 
      
      $backward_difference_code
        2-1   
      1 "-1/2"
      2 "1/2" 
      
      $forward_difference_code
        1-2   
      1 "-1/2"
      2 "1/2" 
      

---

    Code
      lapply(schemes, function(s) {
        fx <- sym(s)
        cmat <- suppressWarnings(rlang::inject(enlist_contrasts(tstdata_2, x ~ !!fx +
          2)))
        as.character(MASS::fractions(cmat[[1L]]))
      })
    Output
      $sum_code
        1   
      1 "1" 
      2 "-1"
      
      $treatment_code
        1  
      1 "1"
      2 "0"
      
      $scaled_sum_code
        1     
      1 "1/2" 
      2 "-1/2"
      
      $cumulative_split_code
        1|2   
      1 "1/2" 
      2 "-1/2"
      
      $helmert_code
        <2    
      1 "1/2" 
      2 "-1/2"
      
      $reverse_helmert_code
        >1    
      1 "1/2" 
      2 "-1/2"
      
      $backward_difference_code
        2-1   
      1 "1/2" 
      2 "-1/2"
      
      $forward_difference_code
        1-2   
      1 "1/2" 
      2 "-1/2"
      

---

    Code
      lapply(schemes, function(s) {
        fx <- sym(s)
        cmat <- rlang::inject(enlist_contrasts(tstdata_2, x ~ !!fx * 2))
        as.character(MASS::fractions(cmat[[1L]]))
      })
    Output
      $sum_code
        2   
      1 "-2"
      2 "0" 
      
      $treatment_code
        2   
      1 "-1"
      2 "0" 
      
      $scaled_sum_code
        2   
      1 "-1"
      2 "0" 
      
      $cumulative_split_code
        1|2 
      1 "-1"
      2 "0" 
      
      $helmert_code
        <2  
      1 "-1"
      2 "0" 
      
      $reverse_helmert_code
        >1  
      1 "-1"
      2 "0" 
      
      $backward_difference_code
        2-1 
      1 "-1"
      2 "0" 
      
      $forward_difference_code
        1-2 
      1 "-1"
      2 "0" 
      

---

    Code
      lapply(schemes, function(s) {
        fx <- sym(s)
        cmat <- rlang::inject(enlist_contrasts(tstdata_5, x ~ !!fx))
        as.character(MASS::fractions(cmat[[1L]]))
      })
    Output
      $sum_code
        2    3    4    5   
      1 "-1" "-1" "-1" "-1"
      2 "1"  "0"  "0"  "0" 
      3 "0"  "1"  "0"  "0" 
      4 "0"  "0"  "1"  "0" 
      5 "0"  "0"  "0"  "1" 
      
      $treatment_code
        2   3   4   5  
      1 "0" "0" "0" "0"
      2 "1" "0" "0" "0"
      3 "0" "1" "0" "0"
      4 "0" "0" "1" "0"
      5 "0" "0" "0" "1"
      
      $scaled_sum_code
        2      3      4      5     
      1 "-1/5" "-1/5" "-1/5" "-1/5"
      2 "4/5"  "-1/5" "-1/5" "-1/5"
      3 "-1/5" "4/5"  "-1/5" "-1/5"
      4 "-1/5" "-1/5" "4/5"  "-1/5"
      5 "-1/5" "-1/5" "-1/5" "4/5" 
      
      $cumulative_split_code
        1|2    2|3    3|4    4|5   
      1 "4/5"  "0"    "0"    "0"   
      2 "-4/5" "6/5"  "0"    "0"   
      3 "0"    "-6/5" "6/5"  "0"   
      4 "0"    "0"    "-6/5" "4/5" 
      5 "0"    "0"    "0"    "-4/5"
      
      $helmert_code
        <2     <3     <4     <5    
      1 "-1/2" "-1/3" "-1/4" "-1/5"
      2 "1/2"  "-1/3" "-1/4" "-1/5"
      3 "0"    "2/3"  "-1/4" "-1/5"
      4 "0"    "0"    "3/4"  "-1/5"
      5 "0"    "0"    "0"    "4/5" 
      
      $reverse_helmert_code
        >1     >2     >3     >4    
      1 "4/5"  "0"    "0"    "0"   
      2 "-1/5" "3/4"  "0"    "0"   
      3 "-1/5" "-1/4" "2/3"  "0"   
      4 "-1/5" "-1/4" "-1/3" "1/2" 
      5 "-1/5" "-1/4" "-1/3" "-1/2"
      
      $backward_difference_code
        2-1    3-2    4-3    5-4   
      1 "-4/5" "-3/5" "-2/5" "-1/5"
      2 "1/5"  "-3/5" "-2/5" "-1/5"
      3 "1/5"  "2/5"  "-2/5" "-1/5"
      4 "1/5"  "2/5"  "3/5"  "-1/5"
      5 "1/5"  "2/5"  "3/5"  "4/5" 
      
      $forward_difference_code
        1-2    2-3    3-4    4-5   
      1 "4/5"  "3/5"  "2/5"  "1/5" 
      2 "-1/5" "3/5"  "2/5"  "1/5" 
      3 "-1/5" "-2/5" "2/5"  "1/5" 
      4 "-1/5" "-2/5" "-3/5" "1/5" 
      5 "-1/5" "-2/5" "-3/5" "-4/5"
      

---

    Code
      lapply(schemes, function(s) {
        fx <- sym(s)
        cmat <- rlang::inject(enlist_contrasts(tstdata_5, x ~ !!fx * 3))
        as.character(MASS::fractions(cmat[[1L]]))
      })
    Output
      $sum_code
        2    3    4    5   
      1 "-1" "-2" "-1" "-1"
      2 "1"  "-1" "0"  "0" 
      3 "0"  "0"  "0"  "0" 
      4 "0"  "-1" "1"  "0" 
      5 "0"  "-1" "0"  "1" 
      
      $treatment_code
        2   3    4   5  
      1 "0" "-1" "0" "0"
      2 "1" "-1" "0" "0"
      3 "0" "0"  "0" "0"
      4 "0" "-1" "1" "0"
      5 "0" "-1" "0" "1"
      
      $scaled_sum_code
        2   3    4   5  
      1 "0" "-1" "0" "0"
      2 "1" "-1" "0" "0"
      3 "0" "0"  "0" "0"
      4 "0" "-1" "1" "0"
      5 "0" "-1" "0" "1"
      
      $cumulative_split_code
        1|2    2|3    3|4     4|5   
      1 "4/5"  "6/5"  "-6/5"  "0"   
      2 "-4/5" "12/5" "-6/5"  "0"   
      3 "0"    "0"    "0"     "0"   
      4 "0"    "6/5"  "-12/5" "4/5" 
      5 "0"    "6/5"  "-6/5"  "-4/5"
      
      $helmert_code
        <2     <3     <4    <5 
      1 "-1/2" "-1"   "0"   "0"
      2 "1/2"  "-1"   "0"   "0"
      3 "0"    "0"    "0"   "0"
      4 "0"    "-2/3" "1"   "0"
      5 "0"    "-2/3" "1/4" "1"
      
      $reverse_helmert_code
        >1  >2    >3     >4    
      1 "1" "1/4" "-2/3" "0"   
      2 "0" "1"   "-2/3" "0"   
      3 "0" "0"   "0"    "0"   
      4 "0" "0"   "-1"   "1/2" 
      5 "0" "0"   "-1"   "-1/2"
      
      $backward_difference_code
        2-1  3-2  4-3 5-4
      1 "-1" "-1" "0" "0"
      2 "0"  "-1" "0" "0"
      3 "0"  "0"  "0" "0"
      4 "0"  "0"  "1" "0"
      5 "0"  "0"  "1" "1"
      
      $forward_difference_code
        1-2 2-3 3-4  4-5 
      1 "1" "1" "0"  "0" 
      2 "0" "1" "0"  "0" 
      3 "0" "0" "0"  "0" 
      4 "0" "0" "-1" "0" 
      5 "0" "0" "-1" "-1"
      

---

    Code
      lapply(schemes, function(s) {
        fx <- sym(s)
        cmat <- suppressWarnings(rlang::inject(enlist_contrasts(tstdata_5, x ~ !!fx +
          3)))
        as.character(MASS::fractions(cmat[[1L]]))
      })
    Output
      $sum_code
        1    2    4    5   
      1 "1"  "0"  "0"  "0" 
      2 "0"  "1"  "0"  "0" 
      3 "-1" "-1" "-1" "-1"
      4 "0"  "0"  "1"  "0" 
      5 "0"  "0"  "0"  "1" 
      
      $treatment_code
        1   2   4   5  
      1 "1" "0" "0" "0"
      2 "0" "1" "0" "0"
      3 "0" "0" "0" "0"
      4 "0" "0" "1" "0"
      5 "0" "0" "0" "1"
      
      $scaled_sum_code
        1      2      4      5     
      1 "4/5"  "-1/5" "-1/5" "-1/5"
      2 "-1/5" "4/5"  "-1/5" "-1/5"
      3 "-1/5" "-1/5" "-1/5" "-1/5"
      4 "-1/5" "-1/5" "4/5"  "-1/5"
      5 "-1/5" "-1/5" "-1/5" "4/5" 
      
      $cumulative_split_code
        1|2    2|3    3|4    4|5   
      1 "4/5"  "0"    "0"    "0"   
      2 "-4/5" "6/5"  "0"    "0"   
      3 "0"    "-6/5" "6/5"  "0"   
      4 "0"    "0"    "-6/5" "4/5" 
      5 "0"    "0"    "0"    "-4/5"
      
      $helmert_code
        <2     <3     <4     <5    
      1 "-1/2" "-1/3" "-1/4" "-1/5"
      2 "1/2"  "-1/3" "-1/4" "-1/5"
      3 "0"    "2/3"  "-1/4" "-1/5"
      4 "0"    "0"    "3/4"  "-1/5"
      5 "0"    "0"    "0"    "4/5" 
      
      $reverse_helmert_code
        >1     >2     >3     >4    
      1 "4/5"  "0"    "0"    "0"   
      2 "-1/5" "3/4"  "0"    "0"   
      3 "-1/5" "-1/4" "2/3"  "0"   
      4 "-1/5" "-1/4" "-1/3" "1/2" 
      5 "-1/5" "-1/4" "-1/3" "-1/2"
      
      $backward_difference_code
        2-1    3-2    4-3    5-4   
      1 "-4/5" "-3/5" "-2/5" "-1/5"
      2 "1/5"  "-3/5" "-2/5" "-1/5"
      3 "1/5"  "2/5"  "-2/5" "-1/5"
      4 "1/5"  "2/5"  "3/5"  "-1/5"
      5 "1/5"  "2/5"  "3/5"  "4/5" 
      
      $forward_difference_code
        1-2    2-3    3-4    4-5   
      1 "4/5"  "3/5"  "2/5"  "1/5" 
      2 "-1/5" "3/5"  "2/5"  "1/5" 
      3 "-1/5" "-2/5" "2/5"  "1/5" 
      4 "-1/5" "-2/5" "-3/5" "1/5" 
      5 "-1/5" "-2/5" "-3/5" "-4/5"
      

