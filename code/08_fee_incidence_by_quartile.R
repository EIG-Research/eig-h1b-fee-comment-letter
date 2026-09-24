# 08_fee_incidence_by_quartile -- who the fee actually removes
# Author - Jiaxin He
# research title - H-1B Fee Analysis and Comment Letter
# research question - Even if we take Borjas's methods, what is wrong with how the
#   DHS interpreted it?
#
# The reservation fee in Borjas's hiring rule,
#     F*_h = w_h exp(pi_h) R (exp(dw_h - pi_h) - 1),
# is increasing in the payroll saving dw_h. An employer paying close to, or above,
# what a comparable native earns has little or no saving to protect and will not
# pay the fee. Because the payroll saving is negatively correlated with the wage
# itself, the fee therefore removes the best-paid H-1B workers first. This script
# measures that directly, splitting the in-US cohort into payroll-savings quartiles
# and reporting, for each quartile, the fee response, the median wage, the share
# who came from an F-1 student visa and the average age.
#
# Four scenarios:
#   1. Borjas headline model (Table 8), current unweighted lottery
#   2. Tenure-adjusted model (Table 9), current unweighted lottery
#   3. Tenure-adjusted model, weighted lottery with the old OFLC wage levels
#   4. Tenure-adjusted model, weighted lottery with the new OFLC wage levels
#
# Scenarios 1 and 2 differ in the labour demand model only, not in the measured
# payroll saving: both use the seniority-adjusted saving, as 05_borjas_labor_demand.R does for
# Tables 8 and 9 alike. Table 9 adds 1.5 percent annual wage growth and a 9.4
# percent annual separation rate, which shortens the horizon over which the
# employer expects to keep the saving and therefore lowers what it will pay.
#
# Quartile boundaries are fixed once on the status-quo cohort and reused across all
# four scenarios, so that a quartile always means the same range of payroll savings
# and the reallocation of workers across quartiles is visible.
#
# The cohort is restricted to beneficiaries adjusting status inside the United
# States, because that is the population the FY2027 fee falls on: beneficiaries
# abroad already paid the $100,000 Proclamation payment and are grandfathered.
#
# DEPENDENCY: run 04, 05 and 06 first.

rm(list = ls())
options(scipen = 999)
set.seed(42)

################################################################################
###                           1. Load packages                               ###
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(Hmisc)
})

################################################################################
###                        2. Paths and model constants                      ###
################################################################################

if (requireNamespace("here", quietly = TRUE)) {
  path_project <- here::here()
} else {
  path_project <- getwd()
}

path_processed <- file.path(path_project, "data", "processed")
path_output <- file.path(path_project, "output", "tables")

if (!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)

discount_rate_num <- 0.03
visa_term_int <- 6L
wage_growth_num <- 0.015
separation_rate_num <- 0.094
sigma_pi_num <- 0.5 / 2.576
mu_pi_values <- c(-0.1, 0.0, 0.1)
mu_pi_headline_num <- 0.0            # the central case reported in the table
n_replications_int <- 100L
proposed_fee_num <- 103265
n_quartiles_int <- 4L
savings_bottom_code_num <- -0.7      # footnote 33 trim, applied in script 05
savings_top_code_num <- 0.4

# FY2027 in-US registrations, derived as in 07_fee_under_reformed_rules.R: the in-US channel is
# held at its FY2026 level because the Proclamation payment did not apply to it.
fy2026_registrations_int <- 343981L
fy2027_registrations_int <- 211600L

discount_factor_num <- sum(1 / (1 + discount_rate_num)^(0:(visa_term_int - 1L)))

tenure_probability_num <- c(
  separation_rate_num * (1 - separation_rate_num)^(0:(visa_term_int - 2L)),
  (1 - separation_rate_num)^(visa_term_int - 1L)
)

tenure_discount_num <- vapply(
  seq_len(visa_term_int),
  function(tenure_int) sum(1 / (1 + discount_rate_num)^(0:(tenure_int - 1L))),
  numeric(1L)
)

tenure_growth_discount_num <- vapply(
  seq_len(visa_term_int),
  function(tenure_int) {
    sum(((1 + wage_growth_num) / (1 + discount_rate_num))^(0:(tenure_int - 1L)))
  },
  numeric(1L)
)

################################################################################
###                    3. Assemble the petition-level panel                  ###
################################################################################

payroll_savings <- readRDS(
  file.path(path_processed, "borjas_august_payroll_savings.rds")
)

selection_counts <- readRDS(
  file.path(path_processed, "borjas_allocation_selection_counts.rds")
)

regression_data <- readRDS(
  file.path(path_processed, "borjas_august_regression_data.rds")
)

# 1) Age is not carried in the payroll-savings checkpoint, so it is taken from the
#    pooled regression sample. Only keys that identify exactly one petition are
#    used, so the lookup cannot attach the wrong beneficiary's age.
age_lookup <- regression_data |>
  dplyr::filter(is_h1b_int == 1L) |>
  dplyr::count(petition_key_chr, age_int, name = "n_rows_int") |>
  dplyr::add_count(petition_key_chr, name = "n_keys_int") |>
  dplyr::filter(n_keys_int == 1L) |>
  dplyr::select(petition_key_chr, age_int)

channel_shares <- payroll_savings |>
  dplyr::count(beneficiary_location, name = "n_obs_int") |>
  dplyr::mutate(share_num = n_obs_int / sum(n_obs_int))

in_us_fy2027_num <- fy2026_registrations_int *
  channel_shares$share_num[channel_shares$beneficiary_location == "USA"]

message(sprintf(
  "FY2027 in-US registrations, derived as in script 07: %s",
  formatC(round(in_us_fy2027_num), big.mark = ",", format = "d")
))

# 2) The fee in the FY2027 scenario falls on the in-US channel only. The cohort is
#    defined by beneficiary_location == "USA" so that it matches the volume
#    definition used in 07_fee_under_reformed_rules.R, which groups the 0.2 percent coded
#    "Other" with the abroad channel.
petition_panel <- payroll_savings |>
  dplyr::filter(year_int >= 2022L, beneficiary_location == "USA") |>
  dplyr::left_join(age_lookup, by = "petition_key_chr") |>
  dplyr::mutate(
    f1_flag = prior_status_group == "In US, prior F-1"
  )

message(
  "In-US petitions, FY2022-24: ", format(nrow(petition_panel), big.mark = ","),
  "; with an age match: ", format(sum(!is.na(petition_panel$age_int)), big.mark = ",")
)

# 3) Two demand models crossed with three allocation rules. The grouped chart uses
#    the three Borjas-headline rows, so that the only thing varying across the bars
#    is the allocation rule; the tenure-adjusted rows are kept in the saved table
#    for the separate tenure point.
scenario_table <- tidyr::expand_grid(
  tibble::tibble(
    demand_model_chr = c("baseline", "growth_and_separations"),
    demand_label_chr = c("Borjas headline", "Tenure-adjusted")
  ),
  tibble::tibble(
    alloc_method_chr = c(
      "1. old unweighted lottery",
      "2a. weighted lottery, old wage levels",
      "2b. weighted lottery, new wage levels"
    ),
    alloc_label_chr = c(
      "current lottery", "weighted lottery + old wage levels",
      "weighted lottery + new wage levels"
    )
  )
) |>
  dplyr::mutate(
    scenario_int = dplyr::row_number(),
    scenario_chr = paste0(demand_label_chr, ", ", alloc_label_chr)
  )

################################################################################
###          4. Fixed quartile boundaries from the status-quo cohort         ###
################################################################################

status_quo_weights <- selection_counts |>
  dplyr::filter(alloc_method == "1. old unweighted lottery") |>
  dplyr::select(petition_key_chr, selection_count_int)

status_quo_panel <- petition_panel |>
  dplyr::inner_join(status_quo_weights, by = "petition_key_chr")

quartile_breaks_num <- Hmisc::wtd.quantile(
  status_quo_panel$payroll_savings_num,
  weights = status_quo_panel$selection_count_int,
  probs = c(0.25, 0.5, 0.75)
)

message("")
message(sprintf(
  "Payroll-savings quartile cut points on the status-quo cohort: %+.4f, %+.4f, %+.4f",
  quartile_breaks_num[1L], quartile_breaks_num[2L], quartile_breaks_num[3L]
))

wage_breaks_num <- Hmisc::wtd.quantile(
  status_quo_panel$wage_real_num,
  weights = status_quo_panel$selection_count_int,
  probs = c(0.25, 0.5, 0.75)
)

message(sprintf(
  "Wage quartile cut points on the status-quo cohort: $%s, $%s, $%s",
  formatC(round(wage_breaks_num[1L]), big.mark = ",", format = "d"),
  formatC(round(wage_breaks_num[2L]), big.mark = ",", format = "d"),
  formatC(round(wage_breaks_num[3L]), big.mark = ",", format = "d")
))

petition_panel <- petition_panel |>
  dplyr::mutate(
    savings_quartile_int = 1L +
      findInterval(payroll_savings_num, unname(quartile_breaks_num)),
    wage_quartile_int = 1L +
      findInterval(wage_real_num, unname(wage_breaks_num))
  )

################################################################################
###        5. Fee response by quartile, scenario by scenario                 ###
################################################################################

quartile_results <- NULL

for (cut_variable_chr in c("savings_quartile_int", "wage_quartile_int")) {
for (scenario_index in seq_len(nrow(scenario_table))) {

  scenario_weights <- selection_counts |>
    dplyr::filter(
      alloc_method == scenario_table$alloc_method_chr[scenario_index]
    ) |>
    dplyr::select(petition_key_chr, selection_count_int)

  scenario_panel <- petition_panel |>
    dplyr::inner_join(scenario_weights, by = "petition_key_chr")

  wage_vector_num <- scenario_panel$wage_real_num
  savings_vector_num <- scenario_panel$payroll_savings_trimmed_num
  weight_vector_num <- as.numeric(scenario_panel$selection_count_int)
  quartile_vector_int <- scenario_panel[[cut_variable_chr]]
  age_vector_num <- as.numeric(scenario_panel$age_int)
  f1_vector_num <- as.numeric(scenario_panel$f1_flag)
  n_workers_int <- nrow(scenario_panel)

  for (mu_pi_num in mu_pi_values) {

    rate_matrix_num <- matrix(
      NA_real_, nrow = n_replications_int, ncol = n_quartiles_int
    )
    age_post_matrix_num <- matrix(
      NA_real_, nrow = n_replications_int, ncol = n_quartiles_int
    )
    f1_post_matrix_num <- matrix(
      NA_real_, nrow = n_replications_int, ncol = n_quartiles_int
    )
    cohort_age_post_num <- numeric(n_replications_int)
    cohort_f1_post_num <- numeric(n_replications_int)

    for (replication_index in seq_len(n_replications_int)) {

      if (scenario_table$demand_model_chr[scenario_index] == "baseline") {
        truncation_point_num <- savings_vector_num
        effective_discount_num <- rep(discount_factor_num, n_workers_int)
      } else {
        tenure_draw_int <- sample(
          seq_len(visa_term_int), size = n_workers_int, replace = TRUE,
          prob = tenure_probability_num
        )
        truncation_point_num <- savings_vector_num +
          log(tenure_discount_num[tenure_draw_int]) -
          log(tenure_growth_discount_num[tenure_draw_int])
        effective_discount_num <- tenure_growth_discount_num[tenure_draw_int]
      }

      upper_probability_num <- stats::pnorm(
        truncation_point_num, mean = mu_pi_num, sd = sigma_pi_num
      )
      pi_draw_num <- stats::qnorm(
        stats::runif(n_workers_int) * upper_probability_num,
        mean = mu_pi_num, sd = sigma_pi_num
      )
      pi_draw_num <- pmin(pi_draw_num, truncation_point_num)

      reservation_fee_num <- wage_vector_num * exp(pi_draw_num) *
        effective_discount_num *
        (exp(truncation_point_num - pi_draw_num) - 1)

      hired_flag <- reservation_fee_num >= proposed_fee_num

      # 4) Weighting a petition by its selection count times an indicator for
      #    being hired gives the post-fee cohort directly, so the surviving
      #    average age and F-1 share are measured rather than approximated from
      #    the pre-fee quartile means.
      survivor_weight_num <- weight_vector_num * as.numeric(hired_flag)

      cohort_age_post_num[replication_index] <- stats::weighted.mean(
        age_vector_num, survivor_weight_num, na.rm = TRUE
      )
      cohort_f1_post_num[replication_index] <- stats::weighted.mean(
        f1_vector_num, survivor_weight_num
      )

      for (quartile_current in seq_len(n_quartiles_int)) {
        quartile_flag <- quartile_vector_int == quartile_current
        rate_matrix_num[replication_index, quartile_current] <-
          stats::weighted.mean(
            hired_flag[quartile_flag], weight_vector_num[quartile_flag]
          )
        if (sum(survivor_weight_num[quartile_flag]) > 0) {
          age_post_matrix_num[replication_index, quartile_current] <-
            stats::weighted.mean(
              age_vector_num[quartile_flag], survivor_weight_num[quartile_flag],
              na.rm = TRUE
            )
          f1_post_matrix_num[replication_index, quartile_current] <-
            stats::weighted.mean(
              f1_vector_num[quartile_flag], survivor_weight_num[quartile_flag]
            )
        }
      }
    }

    # 3) Composition of each quartile under this scenario's allocation rule.
    quartile_composition <- scenario_panel |>
      dplyr::group_by(quartile_int = .data[[cut_variable_chr]]) |>
      dplyr::summarise(
        cohort_share_pct = 100 * sum(selection_count_int),
        savings_lower_num = min(payroll_savings_num),
        savings_upper_num = max(payroll_savings_num),
        median_savings_num = Hmisc::wtd.quantile(
          payroll_savings_num, weights = selection_count_int, probs = 0.5
        ),
        wage_lower_num = min(wage_real_num),
        wage_upper_num = max(wage_real_num),
        median_wage_num = Hmisc::wtd.quantile(
          wage_real_num, weights = selection_count_int, probs = 0.5
        ),
        f1_share_pre_pct = 100 * stats::weighted.mean(f1_flag, selection_count_int),
        mean_age_pre_num = stats::weighted.mean(
          age_int, selection_count_int, na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(cohort_share_pct = cohort_share_pct / sum(cohort_share_pct) * 100)

    quartile_results <- dplyr::bind_rows(
      quartile_results,
      quartile_composition |>
        dplyr::mutate(
          scenario_int = scenario_table$scenario_int[scenario_index],
          scenario_chr = scenario_table$scenario_chr[scenario_index],
          mu_pi_num = mu_pi_num,
          cut_variable = dplyr::if_else(
            cut_variable_chr == "savings_quartile_int", "payroll savings", "wage"
          ),
          hiring_rate_pct = 100 * colMeans(rate_matrix_num)[quartile_int],
          volume_reduction_pct = 100 - hiring_rate_pct,
          mean_age_post_num = colMeans(age_post_matrix_num, na.rm = TRUE)[quartile_int],
          age_shift_num = mean_age_post_num - mean_age_pre_num,
          f1_share_post_pct =
            100 * colMeans(f1_post_matrix_num, na.rm = TRUE)[quartile_int],
          cohort_mean_age_pre_num = stats::weighted.mean(
            scenario_panel$age_int, scenario_panel$selection_count_int, na.rm = TRUE
          ),
          cohort_mean_age_post_num = mean(cohort_age_post_num, na.rm = TRUE),
          cohort_f1_share_pre_pct = 100 * stats::weighted.mean(
            scenario_panel$f1_flag, scenario_panel$selection_count_int
          ),
          cohort_f1_share_post_pct = 100 * mean(cohort_f1_post_num, na.rm = TRUE),
          fy2027_applications_num = in_us_fy2027_num * cohort_share_pct / 100,
          fy2027_lost_num = fy2027_applications_num * volume_reduction_pct / 100
        )
    )
  }

  message("  simulated ", scenario_table$scenario_chr[scenario_index],
          " (", cut_variable_chr, ")")
}
}

quartile_results <- quartile_results |>
  dplyr::select(
    cut_variable, scenario_int, scenario_chr, mu_pi_num, quartile_int,
    cohort_share_pct,
    savings_lower_num, median_savings_num, savings_upper_num,
    wage_lower_num, median_wage_num, wage_upper_num,
    f1_share_pre_pct, f1_share_post_pct,
    mean_age_pre_num, mean_age_post_num, age_shift_num,
    hiring_rate_pct, volume_reduction_pct,
    fy2027_applications_num, fy2027_lost_num,
    cohort_mean_age_pre_num, cohort_mean_age_post_num,
    cohort_f1_share_pre_pct, cohort_f1_share_post_pct
  ) |>
  dplyr::arrange(cut_variable, scenario_int, mu_pi_num, quartile_int)

readr::write_csv(
  quartile_results,
  file.path(path_output, "fee_incidence_by_savings_quartile.csv")
)
################################################################################
###          6. Datawrapper export: the grouped bar chart                    ###
################################################################################

path_datawrapper <- file.path(path_output, "datawrapper")

if (!dir.exists(path_datawrapper)) {
  dir.create(path_datawrapper, recursive = TRUE)
}

# 5) The chart groups are the three Borjas-headline rows, so the bars differ only
#    by allocation rule. The tenure-adjusted rows stay in the full table for the
#    separate tenure point.
chart_scenarios <- scenario_table |>
  dplyr::filter(demand_model_chr == "baseline") |>
  dplyr::mutate(
    series_chr = dplyr::case_when(
      alloc_method_chr == "1. old unweighted lottery" ~ "Borjas headline",
      alloc_method_chr == "2a. weighted lottery, old wage levels" ~
        "Weighted lottery, old wage levels",
      TRUE ~ "Weighted lottery, new wage levels"
    )
  )

chart_rows <- quartile_results |>
  dplyr::filter(
    cut_variable == "payroll savings", mu_pi_num == mu_pi_headline_num,
    scenario_chr %in% chart_scenarios$scenario_chr
  ) |>
  dplyr::left_join(
    chart_scenarios |> dplyr::select(scenario_chr, series_chr),
    by = "scenario_chr"
  ) |>
  dplyr::mutate(
    series_chr = factor(series_chr, levels = chart_scenarios$series_chr)
  )

# 6) Quartile bin edges. The cut points are fixed on the status-quo cohort, so the
#    interior edges are the same in every series; the open ends are closed at the
#    observed extremes of that cohort.
status_quo_savings_range_num <- range(status_quo_panel$payroll_savings_num)

quartile_labels <- tibble::tibble(
  quartile_int = 1L:4L,
  bin_lower_num = c(
    status_quo_savings_range_num[1L], unname(quartile_breaks_num)
  ),
  bin_upper_num = c(
    unname(quartile_breaks_num), status_quo_savings_range_num[2L]
  )
) |>
  dplyr::mutate(
    # 6a) The open-ended first and last bins are labelled as such rather than by
    #     their observed extremes. The extreme values are outliers -- the lowest
    #     observed saving is -2.31 -- and the hiring rule never sees them, because
    #     footnote 33 trims the saving to [-0.7, +0.4] before the fee is applied.
    #     The exact bounds are in the annotations file.
    quartile_label_chr = dplyr::case_when(
      quartile_int == 1L ~ sprintf("Q1: below %+.2f", bin_upper_num),
      quartile_int == 4L ~ sprintf("Q4: above %+.2f", bin_lower_num),
      TRUE ~ sprintf("Q%d: %+.2f to %+.2f", quartile_int, bin_lower_num,
                     bin_upper_num)
    )
  )

datawrapper_reduction <- chart_rows |>
  dplyr::left_join(quartile_labels, by = "quartile_int") |>
  dplyr::select(quartile_label_chr, series_chr, volume_reduction_pct) |>
  tidyr::pivot_wider(
    names_from = series_chr, values_from = volume_reduction_pct
  ) |>
  dplyr::rename(`Employer payroll saving, log points` = quartile_label_chr) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(.x, 1)))

readr::write_csv(
  datawrapper_reduction,
  file.path(path_datawrapper,
            "fee_application_reduction_by_savings_quartile_datawrapper.csv")
)

# 7) Everything the chart's notes, tooltips and text might need, one row per bar.
datawrapper_annotations <- chart_rows |>
  dplyr::left_join(quartile_labels, by = "quartile_int") |>
  dplyr::transmute(
    quartile = quartile_label_chr,
    series = as.character(series_chr),
    `Savings, lower bound` = round(bin_lower_num, 4),
    `Savings, median` = round(median_savings_num, 4),
    `Savings, upper bound` = round(bin_upper_num, 4),
    `Savings observed in this series, min` = round(savings_lower_num, 4),
    `Savings observed in this series, max` = round(savings_upper_num, 4),
    `Savings as the hiring rule sees it, min` = round(
      pmax(savings_lower_num, savings_bottom_code_num), 4
    ),
    `Savings as the hiring rule sees it, max` = round(
      pmin(savings_upper_num, savings_top_code_num), 4
    ),
    `Median wage` = round(median_wage_num),
    `Share of in-US cohort, %` = round(cohort_share_pct, 1),
    `Application reduction, %` = round(volume_reduction_pct, 1),
    `FY2027 applications` = round(fy2027_applications_num),
    `FY2027 applications lost` = round(fy2027_lost_num),
    `Mean age before the fee` = round(mean_age_pre_num, 1),
    `Mean age after the fee` = round(mean_age_post_num, 1),
    `Age shift` = round(age_shift_num, 2),
    `F-1 share before the fee, %` = round(f1_share_pre_pct, 1),
    `F-1 share after the fee, %` = round(f1_share_post_pct, 1)
  )

readr::write_csv(
  datawrapper_annotations,
  file.path(path_datawrapper,
            "fee_application_reduction_by_savings_quartile_annotations.csv")
)

################################################################################
###                              7. Report                                   ###
################################################################################

headline_rows <- quartile_results |>
  dplyr::filter(mu_pi_num == mu_pi_headline_num)

savings_rows <- headline_rows |> dplyr::filter(cut_variable == "payroll savings")
wage_rows <- headline_rows |> dplyr::filter(cut_variable == "wage")

message("")
message("===== Fee incidence by payroll-savings quartile, mu = 0 =====")
message("  Q1 holds the smallest payroll savings, which is where the best-paid")
message("  H-1B workers sit: the payroll saving falls as the wage rises.")

for (scenario_current in scenario_table$scenario_chr) {

  scenario_rows <- savings_rows |>
    dplyr::filter(scenario_chr == scenario_current) |>
    dplyr::arrange(quartile_int)

  message("")
  message("  ", scenario_current)
  message(sprintf(
    "    %-3s %7s %9s %9s %9s %10s %9s %7s %7s %9s",
    "Q", "share", "sav.low", "sav.med", "sav.high", "med. wage", "vol. cut",
    "age pre", "age post", "FY27 lost"
  ))
  for (row_index in seq_len(nrow(scenario_rows))) {
    row_current <- scenario_rows[row_index, ]
    message(sprintf(
      "    %-3d %6.1f%% %+9.3f %+9.3f %+9.3f %10s %8.1f%% %7.1f %8.1f %9s",
      row_current$quartile_int, row_current$cohort_share_pct,
      row_current$savings_lower_num, row_current$median_savings_num,
      row_current$savings_upper_num,
      paste0("$", formatC(round(row_current$median_wage_num), big.mark = ",",
                          format = "d")),
      row_current$volume_reduction_pct,
      row_current$mean_age_pre_num, row_current$mean_age_post_num,
      formatC(round(row_current$fy2027_lost_num), big.mark = ",", format = "d")
    ))
  }
  message(sprintf(
    "    cohort: %s of %s in-US applications lost (%.1f%%); mean age %.1f -> %.1f; F-1 share %.1f%% -> %.1f%%",
    formatC(round(sum(scenario_rows$fy2027_lost_num)), big.mark = ",", format = "d"),
    formatC(round(sum(scenario_rows$fy2027_applications_num)), big.mark = ",",
            format = "d"),
    100 * sum(scenario_rows$fy2027_lost_num) /
      sum(scenario_rows$fy2027_applications_num),
    scenario_rows$cohort_mean_age_pre_num[1L],
    scenario_rows$cohort_mean_age_post_num[1L],
    scenario_rows$cohort_f1_share_pre_pct[1L],
    scenario_rows$cohort_f1_share_post_pct[1L]
  ))
}

message("")
message("=== The fee removes the best-paid first, in every scenario ===")
for (scenario_current in scenario_table$scenario_chr) {
  scenario_rows <- savings_rows |>
    dplyr::filter(scenario_chr == scenario_current) |>
    dplyr::arrange(quartile_int)
  message(sprintf(
    "  %-54s Q1 (median wage %s) loses %.1f%%; Q4 (median wage %s) loses %.1f%%",
    scenario_current,
    paste0("$", formatC(round(scenario_rows$median_wage_num[1L]),
                        big.mark = ",", format = "d")),
    scenario_rows$volume_reduction_pct[1L],
    paste0("$", formatC(round(scenario_rows$median_wage_num[4L]),
                        big.mark = ",", format = "d")),
    scenario_rows$volume_reduction_pct[4L]
  ))
}

message("")
message("=== The fee selects for older workers ===")
age_summary <- savings_rows |>
  dplyr::distinct(
    scenario_int, scenario_chr, cohort_mean_age_pre_num,
    cohort_mean_age_post_num, cohort_f1_share_pre_pct, cohort_f1_share_post_pct
  ) |>
  dplyr::mutate(
    cohort_age_shift_num = cohort_mean_age_post_num - cohort_mean_age_pre_num
  ) |>
  dplyr::arrange(scenario_int)

for (row_index in seq_len(nrow(age_summary))) {
  row_current <- age_summary[row_index, ]
  message(sprintf(
    "  %-54s mean age %.2f -> %.2f (%+.2f years)   F-1 share %.1f%% -> %.1f%%",
    row_current$scenario_chr, row_current$cohort_mean_age_pre_num,
    row_current$cohort_mean_age_post_num, row_current$cohort_age_shift_num,
    row_current$cohort_f1_share_pre_pct, row_current$cohort_f1_share_post_pct
  ))
}

message("")
message("=== Cut by wage instead of by payroll savings, mu = 0 ===")
for (scenario_current in scenario_table$scenario_chr) {
  scenario_rows <- wage_rows |>
    dplyr::filter(scenario_chr == scenario_current) |>
    dplyr::arrange(quartile_int)
  message("")
  message("  ", scenario_current)
  for (row_index in seq_len(nrow(scenario_rows))) {
    row_current <- scenario_rows[row_index, ]
    message(sprintf(
      "    Q%d  %5.1f%% of cohort   median wage %11s   age %4.1f -> %4.1f   volume cut %5.1f%%",
      row_current$quartile_int, row_current$cohort_share_pct,
      paste0("$", formatC(round(row_current$median_wage_num), big.mark = ",",
                          format = "d")),
      row_current$mean_age_pre_num, row_current$mean_age_post_num,
      row_current$volume_reduction_pct
    ))
  }
}

f1_summary <- savings_rows |>
  dplyr::group_by(scenario_int, scenario_chr) |>
  dplyr::summarise(
    f1_share_before_pct = dplyr::first(cohort_f1_share_pre_pct),
    f1_share_after_pct = dplyr::first(cohort_f1_share_post_pct),
    mean_age_before_num = dplyr::first(cohort_mean_age_pre_num),
    mean_age_after_num = dplyr::first(cohort_mean_age_post_num),
    f1_applications_before_num = sum(fy2027_applications_num * f1_share_pre_pct / 100),
    f1_applications_lost_num = sum(
      fy2027_applications_num * f1_share_pre_pct / 100 -
        (fy2027_applications_num - fy2027_lost_num) * f1_share_post_pct / 100
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    f1_loss_pct = 100 * f1_applications_lost_num / f1_applications_before_num
  )

message("")
message("=== What it does to the F-1 pipeline ===")
for (row_index in seq_len(nrow(f1_summary))) {
  row_current <- f1_summary[row_index, ]
  message(sprintf(
    "  %-54s F-1 applications lost %s of %s (%.1f%%)",
    row_current$scenario_chr,
    formatC(round(row_current$f1_applications_lost_num), big.mark = ",", format = "d"),
    formatC(round(row_current$f1_applications_before_num), big.mark = ",", format = "d"),
    row_current$f1_loss_pct
  ))
}

readr::write_csv(
  f1_summary, file.path(path_output, "fee_incidence_f1_and_age_shift.csv")
)

saveRDS(
  quartile_results,
  file.path(path_processed, "fee_incidence_by_savings_quartile.rds")
)
arrow::write_parquet(
  quartile_results,
  file.path(path_processed, "fee_incidence_by_savings_quartile.parquet"),
  compression = "snappy"
)

message("")
message("Saved the incidence tables to output/tables/fee_incidence_*.csv")
message("Saved the Datawrapper files to output/tables/datawrapper/")
