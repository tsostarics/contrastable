## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(contrastable)
library(dplyr)
library(MASS)
mdl_data <- 
  mtcars %>% 
  as_tibble() %>% 
  mutate(cyl = factor(cyl), 
         twolevel = round(runif(n()),0),
         twolevel = ifelse(twolevel == 1, "a", "b"),
         twolevel = factor(twolevel),
         gear = factor(gear),
         carb = factor(carb))

## -----------------------------------------------------------------------------
contrasts(mdl_data$twolevel)
contrasts(mdl_data$carb)

## -----------------------------------------------------------------------------
contrasts(mdl_data$carb) <- contr.sum(6)
contrasts(mdl_data$carb)

## ----reset, echo = FALSE------------------------------------------------------
mdl_data <- 
  mtcars %>% 
  as_tibble() %>% 
  mutate(cyl = factor(cyl), 
         twolevel = round(runif(n()),0),
         twolevel = ifelse(twolevel == 1, "a", "b"),
         twolevel = factor(twolevel),
         gear = factor(gear),
         carb = factor(carb))

## -----------------------------------------------------------------------------
contrast_code(mdl_data$carb, contr.sum)

## -----------------------------------------------------------------------------
contrast_code(mdl_data$carb, contr.sum, reference_level = 4)

## -----------------------------------------------------------------------------
options('contrasts')

## -----------------------------------------------------------------------------
contrast_code(mdl_data$carb, contr.poly)

## -----------------------------------------------------------------------------
my_contrasts <- 
  enlist_contrasts(mdl_data,
                   cyl ~ contr.sum + 6, # Set the reference level with + ___
                   twolevel ~ scaled_sum_code + "a",
                   gear ~ forward_difference_code,
                   carb ~ helmert_code)

my_model <- lm(mpg ~ cyl + twolevel + gear + carb, 
               data = mdl_data,  
               contrasts = my_contrasts)

summary(my_model)

## -----------------------------------------------------------------------------
contrast_list <- enlist_contrasts(mdl_data, carb ~ helmert_code)

## -----------------------------------------------------------------------------
mdl_data2 <- 
  set_contrasts(mdl_data,
                cyl ~ contr.sum + 6,
                twolevel ~ scaled_sum_code + "a",
                gear ~ forward_difference_code,
                carb ~ helmert_code)

# Compare treatment coding to helmert coding (note these are different dfs)
contrasts(mdl_data$carb)
contrasts(mdl_data2$carb) %>% fractions()

## -----------------------------------------------------------------------------
helmert_code(2) # Compare k to >k
scaled_sum_code(2) # Compare k to reference level
forward_difference_code(2) # Compare k to k+1

## -----------------------------------------------------------------------------
-contr.sum(4)/2

## -----------------------------------------------------------------------------
scaled_sum_code(4) %>% MASS::fractions()

## -----------------------------------------------------------------------------
# Not what we want
matrix(c(1,1,1,1,-contr.sum(4)/2), nrow = 4) %>%
  t() %>% solve() %>% fractions()

# Actually what we want
matrix(c(1,1,1,1,scaled_sum_code(4)), nrow = 4) %>%
  t() %>% solve() %>% fractions()

## -----------------------------------------------------------------------------
fractions(scaled_sum_code(4))

## -----------------------------------------------------------------------------
matrix(c(rep(1,4),
         scaled_sum_code(4)),
       nrow = 4) %>% fractions()

## -----------------------------------------------------------------------------
contrast_matrix <- 
  matrix(c(rep(1,4),
           scaled_sum_code(4)),
         nrow = 4)

hypothesis_matrix <- solve(t(contrast_matrix))

fractions(hypothesis_matrix)

## -----------------------------------------------------------------------------
hypothesis_matrix2 <- hypothesis_matrix
hypothesis_matrix2[,1] <- c(0,0,0,1)
hypothesis_matrix2

## -----------------------------------------------------------------------------
# Inverse of the transpose, remove the first column to get contrasts
maybe_treatment_contrasts <- solve(t(hypothesis_matrix2))[,2:4]

# I'm manually reordering to make the reference level the 4th level here
definitely_treatment_contrasts <- unname(contr.treatment(4)[c(2,3,4,1),])

maybe_treatment_contrasts
definitely_treatment_contrasts

# Moment of truth: Are they the same?!
all(maybe_treatment_contrasts == definitely_treatment_contrasts)

## -----------------------------------------------------------------------------
forward_contrasts <- forward_difference_code(6)
forward_hypotheses <- 
  matrix(c(rep(1,6), forward_contrasts), nrow = 6) %>% t() %>% solve()

fractions(forward_contrasts)
fractions(forward_hypotheses)

## -----------------------------------------------------------------------------
new_forward_hypotheses <- forward_hypotheses
new_forward_hypotheses[,1] <- c(0,0,0,0,0,1)
new_forward_hypotheses

new_forward_contrasts <- solve(t(new_forward_hypotheses))[,2:6]
new_forward_contrasts

## -----------------------------------------------------------------------------
enlist_contrasts(mdl_data, 
                 carb ~ forward_difference_code * 8,
                 gear ~ scaled_sum_code + 4 * 4)

## -----------------------------------------------------------------------------
contrast_code(mdl_data$gear, 
              scaled_sum_code, 
              reference_level = 4, 
              set_intercept = 4)

## -----------------------------------------------------------------------------
library(ggplot2,diamonds) # load just the diamonds dataset

# Clarity has 8 levels, so we'll remove anything higher than the cubic trend
my_contrasts <- enlist_contrasts(diamonds,
                                 clarity ~ contr.poly - 4:7)

# Higher trends removed
lm(price ~ clarity, data = diamonds, contrasts = my_contrasts) %>% 
  broom::tidy()

# Higher trends retained
lm(price ~ clarity, data = diamonds) %>% 
  broom::tidy()

# Trying to manually remove trends on the contrast in the data set doesn't work
diamonds1 <- diamonds
contrasts(diamonds1$clarity) <- contr.poly(8)[,1:3]
lm(price ~ clarity, data = diamonds1) %>% 
  broom::tidy()

# Decomposing the contrasts to create new columns for the first three trends
# also works well (decompose_contrasts is still experimental)
diamonds2 <- decompose_contrasts(diamonds, "clarity", 1:3)
lm(price ~ clarity.L + clarity.Q + clarity.C, data = diamonds2) %>% 
  broom::tidy()

## -----------------------------------------------------------------------------
# Just show the messages, not the output
invisible(set_contrasts(diamonds, clarity ~ contr.poly - 4:7))

## -----------------------------------------------------------------------------
tstdf <- mtcars
tstdf$cyl <- factor(tstdf$cyl)
contrast_info <- glimpse_contrasts(tstdf, 
                                   vs ~ contr.treatment,
                                   am ~ scaled_sum_code + 1,
                                   gear ~ helmert_code,
                                   carb ~ contr.poly,
                                   all.factors = TRUE)

contrast_info

## -----------------------------------------------------------------------------
contrast_info <- glimpse_contrasts(tstdf, 
                                   vs ~ contr.treatment,
                                   am ~ scaled_sum_code + 1,
                                   gear ~ helmert_code,
                                   carb ~ contr.poly,
                                   all.factors = TRUE,
                                   return.list = TRUE)

my_contrasts <- contrast_info$contrasts
contrast_glimpse <- contrast_info$glimpse

## -----------------------------------------------------------------------------
schemes <- list(vs ~ contr.treatment,
                am ~ scaled_sum_code + 1,
                gear ~ helmert_code,
                carb ~ contr.poly)

contrast_glimpse <- glimpse_contrasts(tstdf, schemes)
contrast_glimpse # Review if needed
tstdf <- set_contrasts(tstdf, schemes, verbose = FALSE)

## -----------------------------------------------------------------------------
contrast_info <- glimpse_contrasts(tstdf, schemes, clean.schemes = TRUE)
contrast_info

## -----------------------------------------------------------------------------
phone_df <- data.frame(phone = factor(c('S', 'SH','N', 'T'),
                                      levels = c('S', 'SH','N', 'T')))

enlist_contrasts(phone_df, 
                 phone ~ scaled_sum_code + "T")

## -----------------------------------------------------------------------------
enlist_contrasts(phone_df, 
                 phone ~ reverse_helmert_code)

## -----------------------------------------------------------------------------
# Note you can add a line break after (or before, really) the | for readability
enlist_contrasts(phone_df, 
                 phone ~ reverse_helmert_code | 
                   c("CompactvsDiffuse","SibvsNas", "StopvsCont"))

## ---- eval=FALSE--------------------------------------------------------------
#  # Not run to avoid loading lme4
#  library(lme4)
#  library(contrastable)
#  mdl_data <- set_contrasts(mtcars, cyl ~ sum_code |
#                              c("longlonglonglonglongname",'shortname'))
#  lmer(mpg ~ cyl + (1|gear), data = mdl_data) %>% summary()

