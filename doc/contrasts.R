## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(contrastable)
library(dplyr)
library(MASS)

## ----mdl-data-----------------------------------------------------------------
mdl_data <- 
  mtcars |> 
  as_tibble() |> 
  mutate(cyl = factor(cyl), 
         twolevel = factor(rep(c("a", "b"), times = nrow(mtcars) / 2)),
         gear = factor(gear),
         carb = factor(carb))

## ----inspect-contrasts--------------------------------------------------------
options("contrasts") # Show defaults for unordered and ordered
contrasts(mdl_data$twolevel)
contrasts(mdl_data$carb) # Note the reference level

## ----default-model-summary----------------------------------------------------
summary(lm(mpg ~ carb, data = mdl_data))

## ----manual-change-contrasts--------------------------------------------------
mdl_data2 <- mdl_data
contrasts(mdl_data2$carb) <- contr.sum(6)

contrasts(mdl_data2$carb) # Note the reference level
summary(lm(mpg ~ carb, data = mdl_data2))

## ----intro-set-contrasts------------------------------------------------------
mdl_data3 <- set_contrasts(mdl_data, carb ~ sum_code)

contrasts(mdl_data2$carb) # matrix from before
contrasts(mdl_data3$carb) # new matrix, note the column names

## ----set-reference------------------------------------------------------------
mdl_data4 <- set_contrasts(mdl_data, carb ~ sum_code + "3")  

contrasts(mdl_data4$carb)

## ----intercept-example--------------------------------------------------------
# mean of group means:
group_means <- summarize(mdl_data4, grp_mean = mean(mpg), .by = "carb")
group_means
mean(group_means$grp_mean)

# model coefficients
coef(lm(mpg ~ carb, data = mdl_data4))

## ----set-intercept------------------------------------------------------------
mdl_data5 <- set_contrasts(mdl_data, carb ~ sum_code + 3 * 3)

contrasts(mdl_data5$carb)
coef(lm(mpg ~ carb, data = mdl_data5))

## ----set-labels---------------------------------------------------------------
mdl_data6 <- set_contrasts(mdl_data, 
                           carb ~ sum_code + 3 * 3 | c("1-3",
                                                       "2-3",
                                                       "4-3",
                                                       "6-3",
                                                       "8-3"))

contrasts(mdl_data5$carb)
coef(lm(mpg ~ carb, data = mdl_data6))

## ----manual-matrix------------------------------------------------------------
mdl_data7 <- mdl_data

my_contrasts <- matrix(c(2, 1, 0, 1, 1, 1, 
                         1, 2, 0, 1, 1, 1, 
                         1, 1, 0, 2, 1, 1, 
                         1, 1, 0, 1, 2, 1,
                         1, 1, 0, 1, 1, 2),
                       nrow = 6)
dimnames(my_contrasts) <- 
  list(
    c("1", "2", "3", "4", "6", "8"), 
    c("1-3", "2-3", "4-3", "6-3", "8-3")
  )


contrasts(mdl_data7$carb) <- my_contrasts
contrasts(mdl_data7$carb)

## ----matrix-example-fractions-------------------------------------------------
mdl_data8 <- set_contrasts(mdl_data, carb ~ helmert_code * 4)

MASS::fractions(contrasts(mdl_data8$carb))

## ----set-contrasts-multiple-vars----------------------------------------------
mdl_data9 <- set_contrasts(mdl_data,
                           carb ~ helmert_code * 4,
                           cyl ~ scaled_sum_code + 4,
                           twolevel ~ scaled_sum_code + "a",
                           gear ~ helmert_code)

MASS::fractions(contrasts(mdl_data9$carb))
MASS::fractions(contrasts(mdl_data9$cyl))
MASS::fractions(contrasts(mdl_data9$twolevel))
MASS::fractions(contrasts(mdl_data9$gear))

## ----intro-enlist-contrasts---------------------------------------------------
enlist_contrasts(mdl_data,
                 carb ~ helmert_code * 4,
                 cyl ~ scaled_sum_code + 4)

## ----example-functions--------------------------------------------------------
# all equivalent to carb ~ sum_code
foo <- sum_code
enlist_contrasts(mdl_data, carb ~ contr.sum)
enlist_contrasts(mdl_data, carb ~ sum_code)
enlist_contrasts(mdl_data, carb ~ foo)

## ----example-as-is------------------------------------------------------------
enlist_contrasts(mdl_data, carb ~ I(contr.sum))

## ----parentheses-handling-----------------------------------------------------
enlist_contrasts(mdl_data, carb ~ contr.sum())

## ----function-must-exist,eval = FALSE-----------------------------------------
#  foo <- contr.sum(6) # foo is a matrix
#  enlist_contrasts(mdl_data, carb ~ foo()) # foo is not a function

## ----lhs-plus-----------------------------------------------------------------
# equivalent to: enlist_contrasts(mdl_data, cyl ~ sum_code, gear ~ sum_code)
enlist_contrasts(mdl_data, cyl + gear ~ sum_code)

## ----tidyselect-where---------------------------------------------------------
enlist_contrasts(mdl_data, where(is.factor) ~ sum_code)
# see also enlist_contrasts(mdl_data, where(is.unordered) ~ sum_code)
# see also enlist_contrasts(mdl_data, where(is.numeric) ~ sum_code)

## ----tidyselect-all-of--------------------------------------------------------
these_vars <- c("cyl", "gear")
enlist_contrasts(mdl_data, all_of(these_vars) ~ sum_code)

## ----tidyselect-forbidden, eval = FALSE---------------------------------------
#  enlist_contrasts(mdl_data,
#                   cyl ~ sum_code,
#                   where(is.factor) ~ sum_code) # cyl is a factor for mdl_data
#  
#  enlist_contrasts(mdl_data,
#                   cyl ~ sum_code,
#                   cyl + gear ~ sum_code) # cyl can't be specified twice

## ----use-matrix---------------------------------------------------------------
my_matrix <- contr.sum(6) / 2
enlist_contrasts(mdl_data, carb ~ my_matrix)

## ----use-list-----------------------------------------------------------------
my_contrasts <- list(carb ~ contr.sum, gear ~ scaled_sum_code)

enlist_contrasts(mdl_data, my_contrasts)
mdl_data12 <- set_contrasts(mdl_data, my_contrasts)

## ----intro-glimpse-contrasts--------------------------------------------------
my_contrasts <- list(carb ~ contr.sum,
                     gear ~ treatment_code + 4,
                     twolevel ~ scaled_sum_code * "b",
                     cyl ~ helmert_code)
mdl_data$twolevel

enlist_contrasts(mdl_data, cyl ~ scaled_sum_code + 6 * 6)

mdl_data13 <- set_contrasts(mdl_data, my_contrasts)

glimpse_contrasts(mdl_data13, my_contrasts)

## ----glimpse-warnings---------------------------------------------------------
glimpse_contrasts(mdl_data, my_contrasts)

## ----glimpse-manual-----------------------------------------------------------
glimpse_contrasts(mdl_data13, 
                  carb ~ contr.sum,
                  gear ~ treatment_code + 4,
                  twolevel ~ scaled_sum_code * "b",
                  cyl ~ helmert_code)

## ----glimpse-check-default----------------------------------------------------
glimpse_contrasts(mdl_data)  # no warnings
glimpse_contrasts(mdl_data13) # warnings

## ----glimpse-mismatch-warnings------------------------------------------------
glimpse_contrasts(mdl_data, 
                  carb ~ contr.sum, 
                  gear ~ treatment_code * 4,
                  cyl ~ contr.treatment | c("diff1", "diff2"))

## ----drop-trends--------------------------------------------------------------
enlist_contrasts(mdl_data, carb ~ contr.poly)
enlist_contrasts(mdl_data, carb ~ contr.poly - 3:5)

## ----drop-trends-reinstated-matrix--------------------------------------------
mdl_data14 <- set_contrasts(mdl_data, carb ~ contr.sum)
contrasts(mdl_data14$carb)
contrasts(mdl_data14$carb) <- contrasts(mdl_data14$carb)[, 1:2]
contrasts(mdl_data14$carb)

## ----drop-trends-hypotheses-floats--------------------------------------------
MASS::fractions(
  contrastable:::.convert_matrix(
    contrasts(mdl_data14$carb)
  )
)

## ----drop-trends-set-contrasts-incompatible-----------------------------------
mdl_data15 <- set_contrasts(mdl_data, carb ~ polynomial_code - 3:5)

## -----------------------------------------------------------------------------
mdl_data16 <- 
  mdl_data |> 
  set_contrasts(cyl ~ helmert_code,
                gear ~ helmert_code) |> 
  decompose_contrasts(~ cyl * gear) 

# Look at the decomposed contrast columns
mdl_data16 |> 
  dplyr::select(matches("^cyl|gear")) |> 
  head()

## -----------------------------------------------------------------------------
coef(lm(mpg ~ cyl, data = mdl_data16))
coef(lm(mpg ~ `cyl<6` + `cyl<8`, data = mdl_data16))
coef(lm(mpg ~ cyl * gear, data = mdl_data16))
coef(lm(mpg ~ 
          `cyl<6` + `cyl<8` + 
          `gear<4` + `gear<5` + 
          `cyl<6:gear<4` + 
          `cyl<8:gear<4` + 
          `cyl<6:gear<5` +
          `cyl<8:gear<5`, 
        data = mdl_data16))

## -----------------------------------------------------------------------------
coef(lm(mpg ~ `cyl<6` + `cyl<8`, data = mdl_data16))
coef(lm(mpg ~ `cyl<6`, data = mdl_data16)) # not the same estimate as the above

## ----usage-pattern-1----------------------------------------------------------
raw_data <- mtcars # load raw data from a csv or something here

# wrangle data for your final model
final_data <- 
  mtcars |> 
  # mutate(# ... some data wrangling transformations ..) |> 
  set_contrasts(carb ~ sum_code) # set contrasts at the very end

mdl <- lm(mpg ~ carb, data = final_data) # run model with contrasts set

## ----usage-pattern-2----------------------------------------------------------
raw_data <- mtcars # load raw data from a csv or something here

# specify contrasts up front
my_contrasts <- list(carb ~ sum_code,
                     cyl  ~ scaled_sum_code,
                     gear ~ sum_code)

# wrangle data for your final model
final_data <- 
  mtcars |> 
  # mutate(# ... some data wrangling transformations ..) |> 
  set_contrasts(my_contrasts) # set contrasts at the very end

# Show the matrices we're using
enlist_contrasts(final_data, my_contrasts)

# Show a summary
glimpse_contrasts(final_data, my_contrasts)

# Fit the model with contrasts set
mdl <- lm(mpg ~ carb, data = final_data)

## ----fractions-usage----------------------------------------------------------
enlist_contrasts(final_data, my_contrasts) |> 
  lapply(MASS::fractions)

