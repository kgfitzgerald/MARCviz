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
#'  \item{j}{Study # j = 1, ..., k_}
#'  \item{delta}{True treatment effect}
#'  \item{N_j}{total # of students in study j}
#'  \item{M_j}{# of clusters in study j}
#'  \item{n_j}{# of students per school in study j}
#'  \item{N_t_j}{Total # of treatment students in study j}
#'  \item{N_c_j}{Total # of control students in study j}
#'  \item{rho}{ICC}
#'  \item{SE}{Standard error used to generate initial d_j estimates}
#'  \item{d_j}{Treatment effect estimate in study j}
#'  \item{se_j}{Standard error for study j (before rescaling)}
#'  \item{w_j}{Meta-analytic weight for study j (before rescaling)}
#'  \item{w_j_perc}{Percent weight allocated to study j}
#' }
#' @references This data set was generated for the statistical cognition experiment in Fitzgerald, Khella, Charles, & Tipton (2024).
#' @keywords datasets
#' @examples
#'
#' data(viz_MA_data)
#' head(viz_MA_data)
#'
"viz_MA_data"
