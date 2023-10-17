#' @importFrom base "data.frame"
#' @importFrom base "dim"
#' @importFrom base "is.null"
#' @importFrom base "set.seed"
#' #' @importFrom base "sum"
#' @importFrom base "max"
#' @importFrom base "abs"
#' @importFrom base "as.numeric"
#' #' @importFrom base "seq"
#' @importFrom base "factor"
#' #' @importFrom base "ceiling"
#' #' @importFrom base "paste"
#' @importFrom base "round"
#' @importFrom base "list"
#'
#' #' @description
#' Includes data for 4 meta-analyses of size k = 10, 20, 50, 100.
#' Should filter/subset by the first column (k) to extract data for one meta-analysis
#'
#' @docType data
#'
#' @usage data(viz_MA_data)
#'
#' @format ## viz_MA_data
#' "data.frame with 180 rows and 15 columns"
#'
#' \describe{"Allows you to give the user a specific description of your
#' variables included in the data set"
#' }
#'
#' @examples "Finally gives you some room to showcase your data"
"viz_MA_data"

# add row for summary data to MA_data
# set w_j_perc = 1 for summary since V(summary) = 1/sum(weights)
MA_data <- MA_data %>%
  add_row(w_j_perc = 1,
          d_j = summary_es,
          w_j = sum(MA_data$w_j))

if(is.null(study_labels)){
  #NOTE TO SELF: another place to make flexible in package for when # of es > k
  study_labels <- c(seq(1:k), "SUMMARY")
} else{
  study_labels <- c(study_labels, "SUMMARY")
}
