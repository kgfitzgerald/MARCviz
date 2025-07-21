# test-viz_MARC.R

# Load required libraries
library(devtools)
devtools::install_github("kgfitzgerald/MARCviz")
library(testthat)
library(MARCviz)
library(metafor)
library(dplyr)
library(tibble)

# ------------------------------------------------------------------------------
# Test 1: viz_MARC returns ggplot object for static type
test_that("viz_MARC returns a ggplot object for static type", {
  data(viz_MA_data)
  d_j <- viz_MA_data$d_j
  se_j <- viz_MA_data$se_j
  p <- viz_MARC(d_j, se_j, type = "static")
  expect_s3_class(p, "ggplot")
})

# ------------------------------------------------------------------------------
# Test 2: viz_MARC works with metafor rma.uni objects
test_that("viz_MARC works with metafor rma.uni objects", {
  dat <- data.frame(
    yi = c(0.2, -0.1, 0.3),
    sei = c(0.05, 0.06, 0.07)
  )
  # fit metafor model using data columns directly
  res <- metafor::rma.uni(yi = dat$yi, sei = dat$sei)
  # run function with model object
  output <- viz_MARC(res, type = "static")
  # check output class
  expect_s3_class(output, "ggplot")
})

# ------------------------------------------------------------------------------
# Test 3: viz_MARC works within tidyverse pipelines
test_that("viz_MARC works within tidyverse pipelines", {
  df <- tibble(
    d_j = c(0.2, -0.1, 0.3),
    se_j = c(0.05, 0.06, 0.07)
  )
  d_j <- df %>% pull(d_j)
  se_j <- df %>% pull(se_j)
  output <- viz_MARC(d_j, se_j, type = "static")
  expect_s3_class(output, "ggplot")
  # check additional ggplot2 layers can be added without error
  output2 <- output + ggplot2::theme_minimal()
  expect_s3_class(output2, "ggplot")
})

# ------------------------------------------------------------------------------
# Test 4: Error checks

test_that("viz_MARC throws errors for invalid inputs", {
  #Missing Effect sizes 
  expect_error(
    viz_MARC(se_j = c(0.1, 0.05)),
    "Missing Effect sizes or metafor object. 
         Effect sizes or metafor object must be provided."
  )
  #Missing standard errors
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3)),
    "No provided standard errors. Standard errors must be provided."
  )
  # negative standard errors
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1, -0.05)),
    "Negative values found in se_j"
  )
  # lengths not matching
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1)),
    "d_j and se_j must be the same length"
  )
  # d_j not numeric
  expect_error(
    viz_MARC(d_j = c("a", "b"), se_j = c(0.1, 0.2)),
    "Both d_j and se_j must be numeric vectors"
  )
  # missing values
  expect_error(
    viz_MARC(d_j = c(0.2, NA), se_j = c(0.1, 0.2)),
    "Missing values detected in d_j or se_j"
  )
  #Invalid summary effect sizes 
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3),summary_es="c",summary_se=.05,w_j=c(.02,.03)),
    "Both the summary effect size and summary standard error must be numeric."
  )
  #Invalid weight matrix
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3),summary_es=.2,summary_se=.05,w_j=c("a","b")),
    "Weights must be a numeric vector or matrix."
  )
  #lengths of d_j and study_labels not matching
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3),summary_es=.2,summary_se=.05,w_j=c(.02,.03),study_labels=c("a","b","c")),
    "d_j and study_labels must be the same length."
  )
  dat <- data.frame(
    yi = c(0.2, -0.1, 0.3),
    sei = c(0.05, 0.06, 0.07)
  )
  # fit metafor model using data columns directly
  res <- metafor::rma.uni(yi = dat$yi, sei = dat$sei)
  expect_error(
    viz_MARC(res,study_labels=c("a","b")),
    "method_object effect sizes and study_labels must be the same length."
  )
  
  # invalid summary_only arguemnt
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1, 0.2), summary_only = "wrong"),
    "The 'summary_only' argument must be either 'TRUE' or 'FALSE'."
  )
  
  #invalid show_study_labels argument
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1, 0.2), show_study_labels = "wrong"),
    "The 'show_study_labels' argument must be either 'TRUE' or 'FALSE'."
  )
  
  #invalid x_limits argument
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1, 0.2), x_limits=c(5,3,4)),
    "The x_limits arguments must be a vector with length 2."
  )
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1, 0.2), x_limits=c("a","b")),
    "The x_limits arguments must be numeric."
  )
  #invalid max_dot_size argument
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1, 0.2), font_sizes = c(14, 10, 8, 9, 8, 7, 10,12)),
    "Font sizes must be a numeric vector with 9 sizes."
  )
  
  #invalid font size argument
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1, 0.2), max_dot_size=c("a","b")),
    "The max_dot_size must be a single numeric value."
  )
  
  # invalid 'type' argument
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1, 0.2), type = "wrong"),
    "The 'type' argument must be either 'static' or 'interactive'"
  )
  # confidence level out of range
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1, 0.2), confidence_level = 1.1),
    "confidence_level must be a numeric value between 0 and 1"
  )
  # negative max_dot_size
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1, 0.2), max_dot_size = -5),
    "max_dot_size must be a positive number"
  )
  # zero textbox_width
  expect_error(
    viz_MARC(d_j = c(0.2, 0.3), se_j = c(0.1, 0.2), textbox_width = 0),
    "textbox_width must be a positive number"
  )
})

# ------------------------------------------------------------------------------
# Manual testing snippets (as comments for interactive testing)

# # testing the plot generation by pasting directly in the console
# data(viz_MA_data)
# d_j <- viz_MA_data$d_j
# se_j <- viz_MA_data$se_j
# viz_MARC(d_j, se_j, type = "static")
# 
# res <- rma.uni(yi = c(0.2, -0.1, 0.3), sei = c(0.05, 0.06, 0.07))
# viz_MARC(res)

# ------------------------------------------------------------------------------
