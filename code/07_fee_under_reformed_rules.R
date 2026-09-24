# 07_fee_under_reformed_rules -- the $103,265 fee after the wage rules
# Author - Jiaxin He
# research title - H-1B Fee Analysis and Comment Letter
# research question - Even if we take Borjas's methods, what is wrong with how the
#   DHS interpreted it?
#
# The fee in the NPRM is justified as a way to capture the payroll savings an
# employer makes by hiring an H-1B worker instead of a comparable native. Borjas's
# labour demand model (NBER WP 34793, Aug. 2026, sec. V-VI) makes that explicit:
# an employer pays a fee F for worker h only when
#     F <= F*_h = w_h exp(pi_h) R (exp(dw_h - pi_h) - 1),
# so an employer facing dw_h <= 0 -- a worker who already costs at least what a
# comparable native costs -- will not pay any positive fee at all.
#
# Script 06 shows that the weighted lottery combined with the updated OFLC wage
# levels turns the measured wage gap positive. This script prices the $103,265 fee
# against the payroll-savings distribution each allocation regime produces, on the
# tenure-adjusted model of Table 9, and asks two questions:
#
#   1. What happens to FY2027 volume when the fee falls only on the in-US channel?
#   2. What does Borjas's own revenue-maximising fee become under each regime?
#
# Regimes, all taken from 06_allocation_simulation.R:
#   1. old unweighted lottery (status quo)
#   2a. weighted lottery, old OFLC wage levels
#   2b. weighted lottery, new OFLC wage levels
#   3. EIG wage ranking, RPP and NPV adjusted
#
# DEPENDENCY: run 04, 05 and 06 first. This script reads the payroll
# savings checkpoint from 08 and the selection counts from 06.

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

# 1) Tenure-adjusted model constants, identical to 05_borjas_labor_demand.R.
discount_rate_num <- 0.03
visa_term_int <- 6L
wage_growth_num <- 0.015
separation_rate_num <- 0.094
sigma_pi_num <- 0.5 / 2.576
mu_pi_values <- c(-0.1, 0.0, 0.1)
n_replications_int <- 100L
visa_cap_int <- 85000L
proposed_fee_num <- 103265
fee_grid_num <- seq(0, 300000, by = 1000)
borjas_excess_demand_int <- 170000L   # the N used in Borjas's Tables 8 and 9

fy2026_registrations_int <- 343981L
fy2027_registrations_int <- 211600L

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
###             3. Payroll savings and the allocation selection counts       ###
################################################################################

payroll_savings <- readRDS(
  file.path(path_processed, "borjas_august_payroll_savings.rds")
)

selection_counts <- readRDS(
  file.path(path_processed, "borjas_allocation_selection_counts.rds")
)

message(
  "Loaded ", format(nrow(payroll_savings), big.mark = ","),
  " petitions with payroll savings, FY", min(payroll_savings$year_int), "-FY",
  max(payroll_savings$year_int)
)

# 2) The channel shares used to split FY2027 volume are taken from the full
#    FY2021-FY2024 sample, exactly as the archived channel-scenario script did, so
#    the same counterfactual. The regime compositions below use FY2022-FY2024
#    only, because that is where the recomputed OFLC wage levels exist.
channel_shares <- payroll_savings |>
  dplyr::count(beneficiary_location, name = "n_obs_int") |>
  dplyr::mutate(share_num = n_obs_int / sum(n_obs_int))

share_usa_num <- channel_shares$share_num[
  channel_shares$beneficiary_location == "USA"
]

savings_recent <- payroll_savings |>
  dplyr::filter(year_int >= 2022L) |>
  dplyr::mutate(
    channel_chr = dplyr::if_else(
      prior_status_group == "Abroad", "Abroad", "In US"
    )
  )

# 2a) Eligibility has to be read off the whole base sample. The selection counts
#     only contain petitions that won at least once, and an ineligible petition
#     can never win, so eligibility measured on that file is 100 percent by
#     construction under every regime.
allocation_base_sample <- readRDS(
  file.path(path_processed, "borjas_allocation_base_sample.rds")
)

eligibility_panel <- allocation_base_sample |>
  dplyr::select(petition_key_chr, Old_OFLC_wage_level_wt,
                New_OFLC_wage_level_strict_wt) |>
  dplyr::inner_join(savings_recent, by = "petition_key_chr")

regime_panel <- selection_counts |>
  dplyr::select(petition_key_chr, alloc_method, selection_count_int,
                Old_OFLC_wage_level_wt, New_OFLC_wage_level_strict_wt) |>
  dplyr::inner_join(savings_recent, by = "petition_key_chr")

message(
  "Matched selection counts to payroll savings for ",
  format(dplyr::n_distinct(regime_panel$petition_key_chr), big.mark = ","),
  " of ", format(nrow(savings_recent), big.mark = ","), " FY2022-24 petitions"
)

# 3) Two weight schemes per regime. "applicant" gives every eligible petition
#    equal weight, which is the pool the fee screens if an employer decides
#    before the lottery, as Borjas's equation (9) assumes. "selected" uses the
#    lottery selection counts, which is the pool the fee screens if the employer
#    decides after selection -- the ordering the statute actually creates, since
#    the payment accompanies the petition and only selected registrants file one.
eligibility_rules <- tibble::tibble(
  alloc_method = c(
    "1. old unweighted lottery",
    "2a. weighted lottery, old wage levels",
    "2b. weighted lottery, new wage levels",
    "3. EIG wage ranking, RPP and NPV adjusted"
  ),
  eligibility_column = c(
    NA_character_, "Old_OFLC_wage_level_wt", "New_OFLC_wage_level_strict_wt",
    NA_character_
  )
)

regime_panel <- regime_panel |>
  dplyr::left_join(eligibility_rules, by = "alloc_method") |>
  dplyr::mutate(
    eligible_flag = dplyr::case_when(
      is.na(eligibility_column) ~ TRUE,
      eligibility_column == "Old_OFLC_wage_level_wt" ~
        !is.na(Old_OFLC_wage_level_wt) & Old_OFLC_wage_level_wt != 0,
      TRUE ~ !is.na(New_OFLC_wage_level_strict_wt) &
        New_OFLC_wage_level_strict_wt != 0
    ),
    weight_applicant_num = as.numeric(eligible_flag),
    weight_selected_num = as.numeric(selection_count_int)
  )

eligibility_summary <- tidyr::expand_grid(
  eligibility_rules, channel_chr = c("Abroad", "In US")
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    eligible_share_num = {
      channel_rows <- eligibility_panel[
        eligibility_panel$channel_chr == channel_chr,
      ]
      if (is.na(eligibility_column)) {
        1
      } else {
        mean(
          !is.na(channel_rows[[eligibility_column]]) &
            channel_rows[[eligibility_column]] != 0
        )
      }
    },
    n_obs_int = sum(eligibility_panel$channel_chr == channel_chr)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(alloc_method, channel_chr, eligible_share_num, n_obs_int)

message("")
message("=== 3. Share of FY2022-24 petitions eligible under each regime ===")
message("  (measured on all ", format(nrow(eligibility_panel), big.mark = ","),
        " petitions, not only those that won a lottery)")
for (row_index in seq_len(nrow(eligibility_summary))) {
  message(sprintf(
    "  %-42s %-7s %6.1f%%",
    eligibility_summary$alloc_method[row_index],
    eligibility_summary$channel_chr[row_index],
    100 * eligibility_summary$eligible_share_num[row_index]
  ))
}

################################################################################
###          4. Payroll savings under each regime, the fee's own currency    ###
################################################################################

savings_by_regime <- NULL

for (weight_scheme_chr in c("weight_applicant_num", "weight_selected_num")) {
  savings_by_regime <- dplyr::bind_rows(
    savings_by_regime,
    regime_panel |>
      dplyr::group_by(alloc_method) |>
      dplyr::summarise(
        weight_scheme = dplyr::if_else(
          weight_scheme_chr == "weight_applicant_num", "eligible applicants",
          "lottery winners"
        ),
        mean_savings_num = stats::weighted.mean(
          payroll_savings_num, .data[[weight_scheme_chr]]
        ),
        share_positive_pct = 100 * stats::weighted.mean(
          payroll_savings_num > 0, .data[[weight_scheme_chr]]
        ),
        median_wage_num = Hmisc::wtd.quantile(
          wage_real_num, weights = .data[[weight_scheme_chr]], probs = 0.5
        ),
        .groups = "drop"
      )
  )
}

# 3a) The dollar quantity the fee is calibrated against. Borjas: "the average
#     payroll savings accruing to a firm that wins an H-1B visa in the lottery
#     are large: nearing $100,000 over the six-year employment term." That is
#     the discounted sum of the annual saving w_h (exp(dw_h) - 1) over the term,
#     computed here on the raw Oaxaca gap, as his headline does, and again after
#     his own 3.6-point seniority correction.
discount_factor_num <- sum(1 / (1 + discount_rate_num)^(0:(visa_term_int - 1L)))

dollar_savings_by_regime <- NULL

for (weight_scheme_chr in c("weight_applicant_num", "weight_selected_num")) {
  dollar_savings_by_regime <- dplyr::bind_rows(
    dollar_savings_by_regime,
    regime_panel |>
      dplyr::group_by(alloc_method) |>
      dplyr::summarise(
        weight_scheme = dplyr::if_else(
          weight_scheme_chr == "weight_applicant_num", "eligible applicants",
          "lottery winners"
        ),
        term_savings_headline_num = discount_factor_num * stats::weighted.mean(
          wage_real_num * (exp(payroll_savings_raw_num) - 1),
          .data[[weight_scheme_chr]]
        ),
        term_savings_tenure_num = discount_factor_num * stats::weighted.mean(
          wage_real_num * (exp(payroll_savings_num) - 1),
          .data[[weight_scheme_chr]]
        ),
        .groups = "drop"
      )
  )
}

savings_by_regime <- savings_by_regime |>
  dplyr::mutate(implied_wage_gap_pct = 100 * (mean_savings_num + 0.036)) |>
  dplyr::left_join(
    dollar_savings_by_regime, by = c("alloc_method", "weight_scheme")
  )

message("")
message("=== 4. Payroll savings by regime (tenure-adjusted, Figure 1 basis) ===")
message("  a negative mean means the average H-1B worker costs MORE than a comparable native")
for (scheme_current in unique(savings_by_regime$weight_scheme)) {
  message("")
  message("  weighting: ", scheme_current)
  scheme_rows <- savings_by_regime |>
    dplyr::filter(weight_scheme == scheme_current)
  for (row_index in seq_len(nrow(scheme_rows))) {
    message(sprintf(
      "    %-42s mean savings %+.4f   %5.1f%% positive   median wage $%s   six-year saving $%s (headline) / $%s (tenure-adjusted)",
      scheme_rows$alloc_method[row_index],
      scheme_rows$mean_savings_num[row_index],
      scheme_rows$share_positive_pct[row_index],
      formatC(round(scheme_rows$median_wage_num[row_index]),
              big.mark = ",", format = "d"),
      formatC(round(scheme_rows$term_savings_headline_num[row_index]),
              big.mark = ",", format = "d"),
      formatC(round(scheme_rows$term_savings_tenure_num[row_index]),
              big.mark = ",", format = "d")
    ))
  }
}

readr::write_csv(
  savings_by_regime, file.path(path_output, "fee_regime_payroll_savings.csv")
)

################################################################################
###      5. Fee response and the whole revenue curve, regime by regime       ###
################################################################################

# 4) One pass per regime, weight scheme, channel and mu. Each replication draws a
#    job tenure and a productivity shock, solves for the reservation fee in closed
#    form, and reads the willing share off the weighted distribution of those
#    reservation fees at every point on the fee grid at once.
fee_response <- NULL
revenue_curves <- NULL

for (regime_current in eligibility_rules$alloc_method) {
  for (weight_scheme_chr in c("weight_applicant_num", "weight_selected_num")) {
    for (channel_current in c("Abroad", "In US", "All")) {

      regime_rows <- regime_panel |>
        dplyr::filter(
          alloc_method == regime_current,
          channel_current == "All" | channel_chr == channel_current,
          .data[[weight_scheme_chr]] > 0
        )

      if (nrow(regime_rows) == 0L) next

      wage_vector_num <- regime_rows$wage_real_num
      savings_vector_num <- regime_rows$payroll_savings_trimmed_num
      weight_vector_num <- regime_rows[[weight_scheme_chr]]
      n_workers_int <- nrow(regime_rows)

      for (mu_pi_num in mu_pi_values) {

        replication_rate_num <- numeric(n_replications_int)
        replication_curve_num <- matrix(
          0, nrow = n_replications_int, ncol = length(fee_grid_num)
        )

        for (replication_index in seq_len(n_replications_int)) {

          tenure_draw_int <- sample(
            seq_len(visa_term_int), size = n_workers_int, replace = TRUE,
            prob = tenure_probability_num
          )
          truncation_point_num <- savings_vector_num +
            log(tenure_discount_num[tenure_draw_int]) -
            log(tenure_growth_discount_num[tenure_draw_int])
          effective_discount_num <- tenure_growth_discount_num[tenure_draw_int]

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

          replication_rate_num[replication_index] <- stats::weighted.mean(
            reservation_fee_num >= proposed_fee_num, weight_vector_num
          )

          # Willing share at every fee on the grid, from the weighted
          # distribution of reservation fees. Only needed for the pooled channel,
          # which is what the revenue curve is built on.
          if (channel_current == "All") {
            order_int <- order(reservation_fee_num)
            cumulative_weight_num <- cumsum(weight_vector_num[order_int]) /
              sum(weight_vector_num)
            below_index_int <- findInterval(
              fee_grid_num, reservation_fee_num[order_int]
            )
            replication_curve_num[replication_index, ] <- 1 - dplyr::if_else(
              below_index_int == 0L, 0,
              cumulative_weight_num[pmax(below_index_int, 1L)]
            )
          }
        }

        fee_response <- dplyr::bind_rows(
          fee_response,
          tibble::tibble(
            alloc_method = regime_current,
            weight_scheme = dplyr::if_else(
              weight_scheme_chr == "weight_applicant_num",
              "eligible applicants", "lottery winners"
            ),
            channel_chr = channel_current,
            mu_pi_num = mu_pi_num,
            hiring_rate_num = mean(replication_rate_num),
            n_petitions_int = n_workers_int
          )
        )

        if (channel_current == "All") {
          revenue_curves <- dplyr::bind_rows(
            revenue_curves,
            tibble::tibble(
              alloc_method = regime_current,
              weight_scheme = dplyr::if_else(
                weight_scheme_chr == "weight_applicant_num",
                "eligible applicants", "lottery winners"
              ),
              mu_pi_num = mu_pi_num,
              fee_num = fee_grid_num,
              willing_share_num = colMeans(replication_curve_num)
            )
          )
        }
      }
    }
  }
  message("  simulated regime: ", regime_current)
}

readr::write_csv(
  fee_response, file.path(path_output, "fee_regime_hiring_rates.csv")
)

message("")
message("=== 5. Share of employers willing to pay $103,265, tenure-adjusted ===")
for (scheme_current in c("eligible applicants", "lottery winners")) {
  message("")
  message("  weighting: ", scheme_current)
  for (regime_current in eligibility_rules$alloc_method) {
    regime_rows <- fee_response |>
      dplyr::filter(
        alloc_method == regime_current, weight_scheme == scheme_current,
        channel_chr == "In US"
      ) |>
      dplyr::arrange(mu_pi_num)
    message(sprintf(
      "    %-42s in-US  mu -0.1: %5.1f%%   mu 0.0: %5.1f%%   mu +0.1: %5.1f%%",
      regime_current, 100 * regime_rows$hiring_rate_num[1L],
      100 * regime_rows$hiring_rate_num[2L], 100 * regime_rows$hiring_rate_num[3L]
    ))
  }
}

################################################################################
###          6. FY2027 with the fee falling only on the in-US channel        ###
################################################################################

# 5) FY2027 composition: the Proclamation
#    applied only to beneficiaries abroad, so the in-US channel is held at its
#    FY2026 level and the abroad channel is the residual.
in_us_fy2027_num <- fy2026_registrations_int * share_usa_num
abroad_fy2027_num <- fy2027_registrations_int - in_us_fy2027_num

message("")
message("=== 6. FY2027, ", formatC(fy2027_registrations_int, big.mark = ",", format = "d"),
        " registrations ===")
message(sprintf(
  "  in-US %s, abroad %s (already paid the $100,000 Proclamation payment)",
  formatC(round(in_us_fy2027_num), big.mark = ",", format = "d"),
  formatC(round(abroad_fy2027_num), big.mark = ",", format = "d")
))

# 6) Two readings of what the wage-level rules do to volume.
#
#    "compliance" assumes employers meet the new Level I by raising the offer, so
#    the registration count is unchanged and only the wage distribution moves.
#    That is what the wage-level reform is designed to achieve.
#
#    "exclusion" assumes registrations that pay below the new Level I simply do
#    not happen, which is the assumption 06_allocation_simulation.R makes when it drops them from
#    the lottery. It contracts the pool as well as shifting its composition.
#
#    The truth is between the two; reporting both bounds the answer.
eligible_wide <- eligibility_summary |>
  tidyr::pivot_wider(
    id_cols = alloc_method, names_from = channel_chr,
    values_from = eligible_share_num, names_prefix = "eligible_"
  )

scenario_grid <- tidyr::expand_grid(
  alloc_method = eligibility_rules$alloc_method,
  scenario_variant = c("compliance", "exclusion"),
  mu_pi_num = mu_pi_values
)

fy2027_scenario <- scenario_grid |>
  dplyr::left_join(eligible_wide, by = "alloc_method") |>
  dplyr::mutate(
    eligible_abroad_share_num = dplyr::if_else(
      scenario_variant == "compliance", 1, `eligible_Abroad`
    ),
    eligible_in_us_share_num = dplyr::if_else(
      scenario_variant == "compliance", 1, `eligible_In US`
    ),
    pool_abroad_num = abroad_fy2027_num * eligible_abroad_share_num,
    pool_in_us_num = in_us_fy2027_num * eligible_in_us_share_num,
    eligible_total_num = pool_abroad_num + pool_in_us_num
  ) |>
  # 7) When the eligible pool exceeds the cap the lottery weights bind and the
  #    fee is priced against the winners. When it does not, every eligible
  #    registration is selected and the fee is priced against the applicants.
  dplyr::mutate(
    weight_scheme = dplyr::if_else(
      eligible_total_num >= visa_cap_int, "lottery winners", "eligible applicants"
    )
  ) |>
  dplyr::left_join(
    fee_response |>
      dplyr::filter(channel_chr == "In US") |>
      dplyr::select(alloc_method, weight_scheme, mu_pi_num,
                    rate_in_us_num = hiring_rate_num),
    by = c("alloc_method", "weight_scheme", "mu_pi_num")
  ) |>
  dplyr::mutate(
    survive_abroad_num = pool_abroad_num,
    survive_in_us_num = pool_in_us_num * rate_in_us_num,
    survive_total_num = survive_abroad_num + survive_in_us_num,
    in_us_lost_num = pool_in_us_num * (1 - rate_in_us_num),
    visas_filled_num = pmin(survive_total_num, visa_cap_int),
    shortfall_num = pmax(visa_cap_int - survive_total_num, 0),
    fee_revenue_billions = proposed_fee_num * visas_filled_num / 1e9
  )

for (variant_current in c("compliance", "exclusion")) {
  message("")
  message("  --- ", variant_current,
          if (variant_current == "compliance") {
            ": employers raise pay to the new floor, volume unchanged ---"
          } else {
            ": registrations below the new floor disappear ---"
          })
  for (regime_current in eligibility_rules$alloc_method) {
    regime_rows <- fy2027_scenario |>
      dplyr::filter(
        alloc_method == regime_current, scenario_variant == variant_current
      ) |>
      dplyr::arrange(mu_pi_num)
    message("")
    message("  ", regime_current)
    message(sprintf(
      "    eligible: abroad %s + in-US %s = %s   (fee priced against %s)",
      formatC(round(regime_rows$pool_abroad_num[1L]), big.mark = ",", format = "d"),
      formatC(round(regime_rows$pool_in_us_num[1L]), big.mark = ",", format = "d"),
      formatC(round(regime_rows$eligible_total_num[1L]), big.mark = ",", format = "d"),
      regime_rows$weight_scheme[1L]
    ))
    for (row_index in seq_len(nrow(regime_rows))) {
      message(sprintf(
        "    mu = %+.1f  in-US willing %5.1f%%  ->  %8s petitions   visas %8s   shortfall %8s   revenue $%.1fbn",
        regime_rows$mu_pi_num[row_index],
        100 * regime_rows$rate_in_us_num[row_index],
        formatC(round(regime_rows$survive_total_num[row_index]), big.mark = ",", format = "d"),
        formatC(round(regime_rows$visas_filled_num[row_index]), big.mark = ",", format = "d"),
        formatC(round(regime_rows$shortfall_num[row_index]), big.mark = ",", format = "d"),
        regime_rows$fee_revenue_billions[row_index]
      ))
    }
  }
}

readr::write_csv(
  fy2027_scenario, file.path(path_output, "fee_fy2027_by_allocation_regime.csv")
)

################################################################################
###      7. Borjas's own revenue-maximising fee under each regime            ###
################################################################################

# 7) Equation (9): V_F = min(p_F * N, 85,000) and revenue = F * V_F. N is held at
#    the 170,000 excess-demand benchmark Borjas uses in Tables 8 and 9, so the
#    status quo row is directly comparable with his reported $95k / $115k / $136k
#    and the only thing changing across regimes is the composition of applicants.
revenue_maximising <- revenue_curves |>
  dplyr::mutate(
    visas_demanded_num = pmin(
      willing_share_num * borjas_excess_demand_int, visa_cap_int
    ),
    revenue_num = fee_num * visas_demanded_num
  ) |>
  dplyr::group_by(alloc_method, weight_scheme, mu_pi_num) |>
  dplyr::slice_max(revenue_num, n = 1L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    revenue_maximising_fee_1000s = fee_num / 1000,
    total_revenue_billions = revenue_num / 1e9
  ) |>
  dplyr::arrange(weight_scheme, alloc_method, mu_pi_num)

message("")
message("=== 7. Revenue-maximising fee, N = 170,000 (Borjas Table 9: $95k / $115k / $136k) ===")
for (scheme_current in c("eligible applicants", "lottery winners")) {
  message("")
  message("  weighting: ", scheme_current)
  for (regime_current in eligibility_rules$alloc_method) {
    regime_rows <- revenue_maximising |>
      dplyr::filter(
        alloc_method == regime_current, weight_scheme == scheme_current
      ) |>
      dplyr::arrange(mu_pi_num)
    message(sprintf(
      "    %-42s $%5.0fk  $%5.0fk  $%5.0fk   (revenue $%.1f / %.1f / %.1f bn)",
      regime_current,
      regime_rows$revenue_maximising_fee_1000s[1L],
      regime_rows$revenue_maximising_fee_1000s[2L],
      regime_rows$revenue_maximising_fee_1000s[3L],
      regime_rows$total_revenue_billions[1L],
      regime_rows$total_revenue_billions[2L],
      regime_rows$total_revenue_billions[3L]
    ))
  }
}

# 8) The same calculation with N set to each regime's own FY2027 eligible pool
#    under the exclusion reading, which is the policy-relevant applicant count
#    rather than Borjas's fixed benchmark.
regime_pool_int <- fy2027_scenario |>
  dplyr::filter(scenario_variant == "exclusion") |>
  dplyr::distinct(alloc_method, weight_scheme, eligible_total_num)

revenue_maximising_own_pool <- revenue_curves |>
  dplyr::inner_join(regime_pool_int, by = c("alloc_method", "weight_scheme")) |>
  dplyr::mutate(
    visas_demanded_num = pmin(
      willing_share_num * eligible_total_num, visa_cap_int
    ),
    revenue_num = fee_num * visas_demanded_num
  ) |>
  dplyr::group_by(alloc_method, mu_pi_num) |>
  dplyr::slice_max(revenue_num, n = 1L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    revenue_maximising_fee_1000s = fee_num / 1000,
    total_revenue_billions = revenue_num / 1e9
  ) |>
  dplyr::arrange(alloc_method, mu_pi_num)

message("")
message("  with N set to each regime's own FY2027 eligible pool (exclusion reading):")
for (regime_current in eligibility_rules$alloc_method) {
  regime_rows <- revenue_maximising_own_pool |>
    dplyr::filter(alloc_method == regime_current) |>
    dplyr::arrange(mu_pi_num)
  if (nrow(regime_rows) < 3L) next
  message(sprintf(
    "    %-42s N = %8s   $%5.0fk  $%5.0fk  $%5.0fk",
    regime_current,
    formatC(round(regime_rows$eligible_total_num[1L]), big.mark = ",", format = "d"),
    regime_rows$revenue_maximising_fee_1000s[1L],
    regime_rows$revenue_maximising_fee_1000s[2L],
    regime_rows$revenue_maximising_fee_1000s[3L]
  ))
}

readr::write_csv(
  revenue_maximising_own_pool,
  file.path(path_output, "fee_regime_revenue_maximising_own_pool.csv")
)

readr::write_csv(
  revenue_maximising, file.path(path_output, "fee_regime_revenue_maximising.csv")
)
readr::write_csv(
  revenue_curves |> dplyr::filter(fee_num %% 5000 == 0),
  file.path(path_output, "fee_regime_revenue_curves.csv")
)

saveRDS(
  fy2027_scenario,
  file.path(path_processed, "fee_fy2027_by_allocation_regime.rds")
)
arrow::write_parquet(
  fy2027_scenario,
  file.path(path_processed, "fee_fy2027_by_allocation_regime.parquet"),
  compression = "snappy"
)

message("")
message("Saved the regime fee tables to output/tables/fee_regime_*.csv")
