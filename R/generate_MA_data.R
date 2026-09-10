###############################################################################
##      THIS SCRIPT PRODUCES viz_MA_data_expX.RDS OBJECTS                    ##
###############################################################################

#use devtools to download MethylCapSig package since it's been taken off CRAN
#install.packages("devtools")
devtools::install_version("MethylCapSig", "1.0.1")
library(tidyverse)
library(MethylCapSig)
library(here)

################ DEFINE PARAMETER VALUES ###########################
## k   = number of studies per meta-analytic dataset
## delta = true treatment effect
## N   = total # of students per study
## M   = # of clusters (schools) per study
## N_t = total # of treatment students per study
## N_c = total # of control students per study
## n   = # of students per cluster (school) (n = N/M)
## rho = intra-class correlation (ICC)
## I   = number of meta-analytic datasets to generate
####################################################################
generate_data <- function(experiment, k, delta, seed, I = 100000) {
  N <- 492 #median from WWC sample size data (see Fitzgerald & Tipton 2022)
  M <- 20 #median from WWC sample size data (see Fitzgerald & Tipton 2022)
  N_t <- N / 2 #assume equal size treatment/control groups
  N_c <- N / 2
  n <- N / M
  rho <- 0.2 #from Hedges & Hedberg (2016)
  I <- I
  ####################################################################

  ################################ CODEBOOK OF VARIABLE NAMES ####################
  #        j = study # j = 1, ..., k
  #      N_j = total # of students in study j
  #      M_j = # of clusters in study j
  #      n_j = # of students per school in study j
  #    N_t_j = total # of treatment students in study j
  #    N_c_j = total # of control students in study j
  #       SE = standard error used to generate initial d_j estimates
  #      d_j = treatment effect estimate in study j
  #     se_j = standard error for study j (before rescaling)
  #      w_j = meta-analytic weight for study j (before rescaling)
  # w_j_perc = percent weight allocated to study j
  ################################################################################

  ################## SIMULATE SAMPLE SIZES #######################################
  #use mvlognormal() to generate N_j and M_j values for K studies
  K <- I * k #total number of studies to generate
  set.seed(seed)
  sample_sizes <- as.data.frame(mvlognormal(
    n = K,
    Mu = c(N, M),
    #variance matrix based on standard deviation of N and M in WWC data
    Sigma = c(849^2, 19.7^2),
    #correlation between N and M, based on WWC data
    R = toeplitz(.7^(0:1))
  )) |>
    rename(N_j = V1, M_j = V2) |>
    mutate_all(ceiling) |> #make integers
    #get rid of extreme values
    #ensure at least 2 clusters per treatment group (M_j >= 4)
    #ensure at least 20 students per school
    mutate(
      M_j = if_else(M_j < 4, 4, M_j),
      n_j = if_else(N_j / M_j < 20, 20, N_j / M_j),
      N_j = n_j * M_j,
      N_t_j = N_j / 2,
      N_c_j = N_j / 2,
      rho = .2
    )

  k_vector <- rep(k, nrow(sample_sizes)) #vector to indicate k
  j_vector <- rep(1:k, I) #vector to indicate study number
  ma_vector <- rep(1:I, each = k) #vector to indicate iteration/Meta-analysis ID

  data <- data.frame(
    ma_id = ma_vector,
    k = k_vector,
    j = j_vector,
    delta = delta
  ) |>
    cbind(sample_sizes) |>
    # SE formula from equation (16) in Hedges (2007)
    mutate(
      SE = sqrt(
        (N_t + N_c) /
          (N_t * N_c) *
          ((1 + (n - 1) * rho)) +
          delta^2 *
            (((N - 2) * (1 - rho)^2 + n * (N - 2 * n) * rho * (1 - rho)) /
              (2 * (N - 2) * ((N - 2) - 2 * (n - 1) * rho)))
      ),
      #generate K treatment effect estimates
      d_j = rnorm(K, mean = delta, sd = SE),
      #calculate study-specific standard errors using study-specific sample sizes
      se_j = sqrt(
        (N_t_j + N_c_j) /
          (N_t_j * N_c_j) *
          ((1 + (n_j - 1) * rho)) +
          delta^2 *
            (((N_j - 2) *
              (1 - rho)^2 +
              n_j * (N_j - 2 * n_j) * rho * (1 - rho)) /
              (2 * (N_j - 2) * ((N_j - 2) - 2 * (n_j - 1) * rho)))
      ),
      w_j = 1 / (se_j^2)
    ) |>
    group_by(ma_id) |>
    mutate(w_j_perc = w_j / sum(w_j)) |>
    ungroup()

  #compute fixed effects MA summary data for each of the 100,000 meta-analytic datasets
  summary_data <- data |>
    group_by(ma_id) |>
    summarize(
      es = weighted.mean(d_j, w_j),
      W = sum(w_j),
      se = sqrt(1 / sum(w_j))
    )

  #keep only meta-analytic datasets that meet criteria
  #determined by experimental conditions.
  #For all experiments: MA summary should round to the intended delta value

  #Experiment 1 & 4: no negative effects
  if (experiment %in% c(1, 4)) {
    UNusable_ma_ids <- data |>
      filter(d_j < 0) |>
      distinct(ma_id) |>
      pull()
    usable_summary_data <- summary_data |>
      filter(!(ma_id %in% UNusable_ma_ids)) |>
      filter(round(es, 2) == delta) |>
      #compute the probability the summary effect is positive
      #and the corresponding significance level
      #also use Z-score/critical value to compute sanity check on sig level
      mutate(
        prob_pos = pnorm(0, es, se, lower.tail = FALSE),
        sig_level = 1 - 2 * (1 - prob_pos),
        Z = (0 - es) / se,
        sig_level_check = pnorm(abs(Z)) - pnorm(-abs(Z))
      )

    #extract 1st, 33rd, 67th, and 99th percentiles to use as the 4 experimental conditions
    ec_sig_levels <- quantile(
      usable_summary_data$sig_level,
      c(0.01, 0.33, 0.67, 0.99)
    )
    ec_prob_levels <- quantile(
      usable_summary_data$prob_pos,
      c(0.01, 0.33, 0.67, 0.99)
    )

    usable_summary_data <- usable_summary_data |>
      #compute the probability the summary effect is positive
      #and the corresponding significance level
      #also use Z-score/critical value to compute sanity check on sig level
      mutate(
        prob_pos = pnorm(0, es, se, lower.tail = FALSE),
        sig_level = 1 - 2 * (1 - prob_pos),
        Z = (0 - es) / se,
        sig_level_check = pnorm(abs(Z)) - pnorm(-abs(Z))
      )
  }

  #Experiment 3: 0, 1, 3, 6 negative effects
  if (experiment == 3) {
    #count the number of negative effects per ma_id
    neg_counts <- data |>
      group_by(ma_id) |>
      summarize(neg_count = sum(d_j < 0))
    #join neg_counts column to data and summary_data
    data <- data |>
      left_join(neg_counts)
    summary_data <- summary_data |>
      left_join(neg_counts)
    #extract UNusable ma_ids
    UNusable_ma_ids <- summary_data |>
      filter(!(neg_count %in% c(0, 1, 3, 6))) |>
      distinct(ma_id) |>
      pull()
    #keep only ma datasets where summary effect rounds to intended delta
    usable_summary_data <- summary_data |>
      filter(round(es, 2) == delta) |>
      filter(!(ma_id %in% UNusable_ma_ids))

    #levels in proposal match fairly well with range that's possible
    #with experimental conditions (e.g. k = 10, delta = 0.15, neg_count = 0,1,3,6)
    ec_sig_levels <- c(0.8, 0.925, 0.97, 0.999)
    ec_prob_levels <- (ec_sig_levels + 1) / 2

    usable_summary_data <- usable_summary_data |>
      #compute the probability the summary effect is positive
      #and the corresponding significance level
      #also use Z-score/critical value to compute sanity check on sig level
      mutate(
        prob_pos = pnorm(0, es, se, lower.tail = FALSE),
        sig_level = 1 - 2 * (1 - prob_pos),
        Z = (0 - es) / se,
        sig_level_check = pnorm(abs(Z)) - pnorm(-abs(Z))
      )
  }

  #Experiment 3 practice: 2 negative effects
  if (experiment == "3practice") {
    #count the number of negative effects per ma_id
    neg_counts <- data |>
      group_by(ma_id) |>
      summarize(neg_count = sum(d_j < 0))
    #join neg_counts column to data and summary_data
    data <- data |>
      left_join(neg_counts)
    summary_data <- summary_data |>
      left_join(neg_counts)
    #extract UNusable ma_ids
    UNusable_ma_ids <- summary_data |>
      filter(!(neg_count %in% c(2))) |>
      distinct(ma_id) |>
      pull()
    #keep only ma datasets where summary effect rounds to intended delta
    usable_summary_data <- summary_data |>
      filter(round(es, 2) == delta) |>
      filter(!(ma_id %in% UNusable_ma_ids))

    #low and high
    ec_sig_levels <- c(0.5, 0.6, 0.9, 0.95)
    ec_prob_levels <- (ec_sig_levels + 1) / 2

    usable_summary_data <- usable_summary_data |>
      #compute the probability the summary effect is positive
      #and the corresponding significance level
      #also use Z-score/critical value to compute sanity check on sig level
      mutate(
        prob_pos = pnorm(0, es, se, lower.tail = FALSE),
        sig_level = 1 - 2 * (1 - prob_pos),
        Z = (0 - es) / se,
        sig_level_check = pnorm(abs(Z)) - pnorm(-abs(Z))
      )
  }

  usable_summary_data <- usable_summary_data |>
    #compute the probability the summary effect is positive
    #and the corresponding significance level
    #also use Z-score/critical value to compute sanity check on sig level
    mutate(
      prob_pos = pnorm(0, es, se, lower.tail = FALSE),
      sig_level = 1 - 2 * (1 - prob_pos),
      Z = (0 - es) / se,
      sig_level_check = pnorm(abs(Z)) - pnorm(-abs(Z))
    )

  #keep only the meta-analytic datasets that round to true delta value
  #intended by the experimental conditions
  usable_data <- data |>
    filter(ma_id %in% usable_summary_data$ma_id)

  #create 4 versions of the usuable data (1 for each experimental condition)
  exp_ma_data <- map_dfr(1:4, ~ usable_data |> mutate(B = .x))

  #rescale weights & standard errors to match the 4 experimental conditions
  #determined above. Note the "significance level" is defined as the confidence
  #level % that would give a lower bound of 0 (i.e. the highest level of
  #confidence that would yield a "significant" result).
  #Example, for a 95% CI, sig_level = 0.95, and z_star = qnorm(.975) = 1.96
  #since lb = es - se*z_star, and se = sqrt(1/W),
  #where W is the sum of the meta-analytic weights, this implies W = (z_star/es)^2
  #we can re-scale the individual study se_j and w_j values so that
  #when meta-analyzed, it gives the desired W for any sig_level (or equivalently, z_star)
  exp_ma_data <- exp_ma_data |>
    group_by(ma_id, B) |>
    mutate(
      sig_level = case_when(
        B == 1 ~ ec_sig_levels[1],
        B == 2 ~ ec_sig_levels[2],
        B == 3 ~ ec_sig_levels[3],
        B == 4 ~ ec_sig_levels[4]
      ),
      prob_pos = case_when(
        B == 1 ~ ec_prob_levels[1],
        B == 2 ~ ec_prob_levels[2],
        B == 3 ~ ec_prob_levels[3],
        B == 4 ~ ec_prob_levels[4]
      ),
      z_star = qnorm((1 - sig_level) / 2 + sig_level),
      needed_sum_weights = (z_star / delta)^2,
      initial_sum_weights = sum(w_j),
      w_j_rescaled = w_j_perc * needed_sum_weights,
      se_j_rescaled = sqrt(1 / (w_j_rescaled))
    ) |>
    ungroup()
  to_return <- list(
    exp_ma_data,
    usable_summary_data,
    ec_sig_levels,
    ec_prob_levels
  )
  return(to_return)
}

exp1_results <- generate_data(experiment = 1, k = 3, delta = 0.15, seed = 2427)
exp1_ma_data <- exp1_results[[1]]
exp3_results <- generate_data(experiment = 3, k = 10, delta = 0.15, seed = 2427)
exp3_ma_data <- exp3_results[[1]]

exp3_practice <- generate_data(
  experiment = "3practice",
  k = 10,
  delta = 0.15,
  seed = 437,
  I = 100
)
exp3_practice_ma_data <- exp3_practice[[1]]
saveRDS(exp3_practice_ma_data, here("data", "exp3_practice_ma_data.RDS"))

exp3_ma_data |>
  count(neg_count) |>
  mutate(n = n / (10 * 4)) #k = 10 rows per ma_id, each ma_id repeated 4 times


plot_sig_levels <- function(data) {
  ggplot(data[[2]], aes(x = sig_level)) +
    geom_histogram() +
    geom_vline(xintercept = data[[3]][1], color = "blue") +
    geom_vline(xintercept = data[[3]][2], color = "blue") +
    geom_vline(xintercept = data[[3]][3], color = "blue") +
    geom_vline(xintercept = data[[3]][4], color = "blue")
}
plot_sig_levels(exp1_results)
plot_sig_levels(exp3_results)


plot_prob_levels <- function(data) {
  ggplot(data[[2]], aes(x = prob_pos)) +
    geom_histogram() +
    geom_vline(xintercept = data[[4]][1], color = "blue") +
    geom_vline(xintercept = data[[4]][2], color = "blue") +
    geom_vline(xintercept = data[[4]][3], color = "blue") +
    geom_vline(xintercept = data[[4]][4], color = "blue")
}
plot_prob_levels(exp1_results)
plot_prob_levels(exp3_results)


#compute meta-analytic summaries for each meta-analytic dataset/B combination
summary_data <- exp3_ma_data |>
  group_by(B, ma_id, sig_level, z_star, prob_pos) |>
  summarize(
    es = weighted.mean(d_j, w_j_rescaled),
    W = sum(w_j_rescaled),
    se = sqrt(1 / sum(w_j_rescaled))
  )
#check generated data to make sure it meets desired conditions
summary_data |>
  #if generated correctly, lb should always be 0 and ub should always be 0.3
  #since we determined weights such that for that sig_level, the lower bound would be 0
  #since the effect size is 0.15, these intervals will always go from 0 to 0.3
  #Note this will NOT be the case for 95% intervals, which is what is displayed
  #in the visualizations
  mutate(
    lb = round(es - se * z_star, 2),
    ub = round(es + se * z_star, 2),
    width = ub - lb,
    lb95 = es - se * qnorm(.975),
    ub95 = es + se * qnorm(.975),
    width95 = ub95 - lb95
  ) |>
  View()

ggplot(summary_data, aes(x = es)) +
  geom_histogram() +
  facet_wrap(~B)

ggplot(summary_data, aes(x = prob_pos)) +
  geom_histogram() +
  facet_wrap(~B)

exp1_ma_data <- exp1_ma_data |> ungroup()
saveRDS(exp1_ma_data, here("data", "exp1_ma_data.RDS"))

exp3_ma_data <- exp3_ma_data |> ungroup()
saveRDS(exp3_ma_data, here("exp3_vote_counting", "data", "exp3_ma_data.RDS"))
