#' Meta-analytic data to visualize using viz_MARC()
#'
#' Includes data for 4 meta-analyses of size k = 10, 20, 50, 100.
#' Should filter/subset by the first column (k) to extract data for one meta-analysis
#'
#' @docType data
#'
#' @usage data(viz_MA_data)
#'
#' @format An object of class \code{"data.frame"} with 180 rows and 5 columns
#' \describe{
#'  \item{k}{Number of studies in the meta-analysis. Here there are 4 meta-analytic datasets here, with k = 10, 20, 50, 100}
#'  \item{d_j}{Treatment effect estimate in study j}
#'  \item{se_j}{Standard error for study j}
#'  \item{w_j}{Meta-analytic weight for study j}
#'  \item{w_j_perc}{Percent weight allocated to study j}
#' }
#' @references This data set was generated for the statistical cognition experiment in Fitzgerald, Khella, Charles, & Tipton (2024).
#' @keywords datasets
#' @examples
#' # for k = 10
#' k10_data <- viz_MA_data %>% filter(k == 10)
#' viz_adj_MARC(d_j = k10_data$d_j, se_j = k10_data$se_j,
#' study_labels = c(1:10), seed = 102,
#' x_breaks = seq(-1, 1, 0.2), x_limit = c(-1, 1))
#'
#' # for k = 20
#' k20_data <- viz_MA_data %>% filter(k == 20)
#' viz_adj_MARC(d_j = k20_data$d_j, se_j = k20_data$se_j,
#' study_labels = c(1:20), seed = 102,
#' x_breaks = seq(-1, 1, 0.2), x_limit = c(-1, 1))
#'
#' # for k = 50
#' k50_data <- viz_MA_data %>% filter(k == 50)
#' viz_adj_MARC(d_j = k50_data$d_j, se_j = k50_data$se_j,
#' study_labels = c(1:50), seed = 102,
#' x_breaks = seq(-1, 1, 0.2), x_limit = c(-1, 1))
#'
#' # for k = 100
#' k100_data <- viz_MA_data %>% filter(k == 100)
#' viz_adj_MARC(d_j = k100_data$d_j, se_j = k100_data$se_j,
#' study_labels = c(1:100), seed = 102,
#' x_breaks = seq(-1, 1, 0.2), x_limit = c(-1, 1))
#'
#' data(viz_MA_data)
#' head(viz_MA_data)
#'
"viz_MA_data"
