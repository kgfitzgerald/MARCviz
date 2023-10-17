## code to prepare `viz_MA_data` dataset goes here

# read raw data, generated in Fitzgerald, Khella, Charles, & Tipton (2024)
viz_MA_data_raw <- readRDS("./data-raw/viz_MA_data_raw.RDS")

#select only columns users of viz_MARC() will need
viz_MA_data <- viz_MA_data_raw %>%
  select(k, d_j, se_j, w_j, w_j_perc)

usethis::use_data(viz_MA_data, overwrite = TRUE)
