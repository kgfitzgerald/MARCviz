# test-viz_MARC.R

# Load required libraries
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
