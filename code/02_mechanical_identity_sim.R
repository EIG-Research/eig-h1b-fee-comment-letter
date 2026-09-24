# 02_mechanical_identity_sim -- show DHS's specification recovers ~0.95 regardless of the truth
# Author - Jiaxin He
# research title - H-1B Fee Analysis and Comment Letter
# research question - What are the problems with how the DHS's technical appendix estimated elasticity?

rm(list = ls())
options(scipen = 999)
set.seed(42)

################################################################################
###                           1. Load packages                               ###
################################################################################

library(tidyverse)
library(fixest)
library(arrow)

################################################################################
###                             2. Set paths                                 ###
################################################################################

# 1) Resolve the project root without a user-specific path
if (requireNamespace("here", quietly = TRUE)) {
  path_project <- here::here()
} else {
  path_project <- getwd()
  message("Package 'here' unavailable; using working directory: ", path_project)
}

path_raw <- file.path(path_project, "data", "raw", "DHS Technical Appendix")
path_processed <- file.path(path_project, "data", "processed")
path_tables <- file.path(path_project, "output", "tables")

for (path_name in c(path_processed, path_tables)) {
  if (!dir.exists(path_name)) {
    dir.create(path_name, recursive = TRUE)
  }
}

################################################################################
###          3. Variance decomposition using DHS's own published tables      ###
################################################################################

# 2) DHS regresses log(count of receipts) on log(total fees). Total fees are the
#    per-petition fee schedule multiplied by the count of receipts, so the
#    regressor contains the dependent variable by construction. Quantify how much
#    of the regressor's variance is the dependent variable, using only the
#    distribution published in Table A.2 and the fee schedule in Table A.1.
table_a2 <- readr::read_csv(
  file.path(path_raw, "table_a2_petitions_per_entity.csv"),
  show_col_types = FALSE
)

# 3) Represent each Table A.2 bin by its geometric midpoint, weighted by the
#    five-year entity counts actually published
table_a2_bins <- table_a2 |>
  dplyr::mutate(
    bin_lower_int = c(1L, 2L, 11L, 31L, 51L, 76L, 101L, 151L, 201L, 251L,
                      301L, 401L, 501L, 1000L, 5001L, 10001L),
    bin_upper_int = c(1L, 10L, 30L, 50L, 75L, 100L, 150L, 200L, 250L, 300L,
                      400L, 500L, 1000L, 5000L, 10000L, 20000L),
    bin_representative_num = sqrt(bin_lower_int * bin_upper_int),
    entity_weight_num = five_year_total
  )

# 4) The published fee schedule takes three values after April 1, 2024:
#    $1,380 (more than 25 employees), $760 (25 or fewer), $460 (nonprofit).
#    Weight them by the FY2025 entity-type shares implied by Table A.1.
fee_schedule <- tibble::tibble(
  entity_type = c("more than 25 employees", "25 or fewer employees", "nonprofit"),
  unit_fee_num = c(1380, 760, 460),
  petition_share_num = c(401767, 50763, 55148) / (401767 + 50763 + 55148)
)

log_count_mean_num <- stats::weighted.mean(
  log(table_a2_bins$bin_representative_num),
  w = table_a2_bins$entity_weight_num
)

log_count_variance_num <- stats::weighted.mean(
  (log(table_a2_bins$bin_representative_num) - log_count_mean_num)^2,
  w = table_a2_bins$entity_weight_num
)

log_fee_mean_num <- stats::weighted.mean(
  log(fee_schedule$unit_fee_num),
  w = fee_schedule$petition_share_num
)

log_fee_variance_num <- stats::weighted.mean(
  (log(fee_schedule$unit_fee_num) - log_fee_mean_num)^2,
  w = fee_schedule$petition_share_num
)

share_of_regressor_variance_from_count_num <- log_count_variance_num /
  (log_count_variance_num + log_fee_variance_num)

message(
  "Variance decomposition of DHS's regressor, from DHS's own tables: ",
  sprintf("%.1f", 100 * share_of_regressor_variance_from_count_num),
  "% of the variance in log(total fees) is variance in the dependent variable."
)

################################################################################
###                   4. Build a simulated petitioner panel                  ###
################################################################################

# 5) Draw entities so the simulated petition-count distribution reproduces the
#    published Table A.2 distribution, including its 54.69 percent single-filer mass
n_entities_int <- 60000L
fiscal_years_int <- 2021L:2025L

entity_bin_index_int <- sample(
  seq_len(nrow(table_a2_bins)),
  size = n_entities_int,
  replace = TRUE,
  prob = table_a2_bins$entity_weight_num
)

# 6) Assign each entity a persistent filing propensity, a fee category, and inert
#    control characteristics. None of these depend on fees.
entity_frame <- tibble::tibble(
  entity_id_int = seq_len(n_entities_int),
  filing_propensity_num = table_a2_bins$bin_representative_num[entity_bin_index_int],
  entity_type = sample(
    fee_schedule$entity_type,
    size = n_entities_int,
    replace = TRUE,
    prob = fee_schedule$petition_share_num
  ),
  naics_code = sample(sprintf("N%02d", 1L:20L), n_entities_int, replace = TRUE),
  state_code = sample(state.abb, n_entities_int, replace = TRUE)
)

# 7) Expand to an entity-by-year panel and attach the published fee schedule.
#    The April 1, 2024 fee rule raises the schedule from FY2024 onward.
simulation_panel <- tidyr::expand_grid(
  entity_frame,
  fiscal_year_int = fiscal_years_int
) |>
  dplyr::mutate(
    post_fee_rule_flag = fiscal_year_int >= 2024L,
    unit_fee_num = dplyr::case_when(
      !post_fee_rule_flag ~ 460,
      entity_type == "more than 25 employees" ~ 1380,
      entity_type == "25 or fewer employees" ~ 760,
      TRUE ~ 460
    ),
    premium_processing_share_num = stats::runif(dplyr::n()),
    cap_status_flag = stats::runif(dplyr::n()) < 0.25
  )

################################################################################
###        5. Generate outcomes under three known true elasticities          ###
################################################################################

# 8) Under each scenario the analyst KNOWS the true fee elasticity because the
#    data were generated with it. Scenario 1 is the null of no fee response at all.
elasticity_scenarios <- tibble::tibble(
  scenario_id = c("A", "B", "C"),
  true_elasticity_num = c(0, -0.5, -2.0),
  scenario_label = c(
    "Fees have no effect on filing volume (true elasticity = 0)",
    "Firms respond substantially to fees (true elasticity = -0.5)",
    "Firms respond very strongly to fees (true elasticity = -2.0)"
  )
)

estimation_results <- vector("list", length = nrow(elasticity_scenarios))

for (scenario_index in seq_len(nrow(elasticity_scenarios))) {

  true_elasticity_num <- elasticity_scenarios$true_elasticity_num[scenario_index]
  scenario_id_chr <- elasticity_scenarios$scenario_id[scenario_index]

  # 9) Scale each entity's expected count by the true fee response, then draw
  #    counts. Total fees are then formed exactly as USCIS forms them: the
  #    per-petition fee schedule multiplied by the realized count.
  scenario_panel <- simulation_panel |>
    dplyr::mutate(
      expected_count_num = filing_propensity_num *
        (unit_fee_num / 460)^true_elasticity_num,
      receipt_count_int = stats::rpois(dplyr::n(), lambda = expected_count_num),
      total_fees_num = receipt_count_int * unit_fee_num
    )

  # 10) USCIS's cross-sectional analysis is estimated on participating entities,
  #     so drop entity-years with no petitions (total fees of zero are undefined
  #     in logs)
  participating_flag <- scenario_panel$receipt_count_int > 0L
  scenario_participants <- scenario_panel[participating_flag, ]

  # 11) Cross-sectional negative binomial by fiscal year, matching Equation (1):
  #     controls for cap status, premium processing, NAICS, and state
  for (fiscal_year_current in fiscal_years_int) {

    year_flag <- scenario_participants$fiscal_year_int == fiscal_year_current
    year_data <- scenario_participants[year_flag, ]

    cross_section_model <- fixest::fenegbin(
      receipt_count_int ~ log(total_fees_num) + cap_status_flag +
        premium_processing_share_num | naics_code + state_code,
      data = year_data,
      notes = FALSE
    )

    estimation_results[[scenario_index]] <- dplyr::bind_rows(
      estimation_results[[scenario_index]],
      tibble::tibble(
        scenario_id = scenario_id_chr,
        true_elasticity_num = true_elasticity_num,
        specification = paste0("Cross-section FY", fiscal_year_current),
        estimated_coefficient_num = unname(
          stats::coef(cross_section_model)["log(total_fees_num)"]
        ),
        standard_error_num = unname(
          fixest::se(cross_section_model)["log(total_fees_num)"]
        ),
        pseudo_r_squared_num = fixest::r2(cross_section_model, type = "apr2"),
        n_observations_int = as.integer(stats::nobs(cross_section_model))
      )
    )
  }

  # 12) Two-way fixed effects negative binomial on the panel, matching Equation (2)
  panel_model <- fixest::fenegbin(
    receipt_count_int ~ log(total_fees_num) + cap_status_flag +
      premium_processing_share_num | entity_id_int + fiscal_year_int,
    data = scenario_participants,
    notes = FALSE
  )

  estimation_results[[scenario_index]] <- dplyr::bind_rows(
    estimation_results[[scenario_index]],
    tibble::tibble(
      scenario_id = scenario_id_chr,
      true_elasticity_num = true_elasticity_num,
      specification = "Panel, two-way fixed effects",
      estimated_coefficient_num = unname(
        stats::coef(panel_model)["log(total_fees_num)"]
      ),
      standard_error_num = unname(
        fixest::se(panel_model)["log(total_fees_num)"]
      ),
      pseudo_r_squared_num = fixest::r2(panel_model, type = "apr2"),
      n_observations_int = as.integer(stats::nobs(panel_model))
    )
  )

  message(
    "Scenario ", scenario_id_chr, " estimated (true elasticity = ",
    true_elasticity_num, ")"
  )
}

################################################################################
###                        6. Assemble and compare                           ###
################################################################################

simulation_results <- dplyr::bind_rows(estimation_results) |>
  dplyr::left_join(
    elasticity_scenarios |> dplyr::select(scenario_id, scenario_label),
    by = "scenario_id"
  ) |>
  dplyr::mutate(
    estimate_minus_truth_num = estimated_coefficient_num - true_elasticity_num,
    # DHS reports 0.94 to 0.97 in cross-section and 0.62 in the panel
    within_dhs_reported_range_flag = estimated_coefficient_num >= 0.60 &
      estimated_coefficient_num <= 1.00
  )

results_summary <- simulation_results |>
  dplyr::group_by(scenario_id, scenario_label, true_elasticity_num) |>
  dplyr::summarise(
    min_estimate_num = min(estimated_coefficient_num),
    max_estimate_num = max(estimated_coefficient_num),
    mean_estimate_num = mean(estimated_coefficient_num),
    share_in_dhs_range_num = mean(within_dhs_reported_range_flag),
    .groups = "drop"
  )

message("")
message("=== DHS's specification applied to data with KNOWN true elasticities ===")
for (row_index in seq_len(nrow(results_summary))) {
  message(
    "  True elasticity ",
    sprintf("%+5.2f", results_summary$true_elasticity_num[row_index]),
    "  ->  estimated ",
    sprintf("%+.3f to %+.3f", results_summary$min_estimate_num[row_index],
            results_summary$max_estimate_num[row_index]),
    "   (", sprintf("%.0f", 100 * results_summary$share_in_dhs_range_num[row_index]),
    "% of estimates fall inside DHS's reported 0.62-0.97 range)"
  )
}
message("")
message("DHS reports 0.94 to 0.97 (cross-section) and 0.62 (panel).")

################################################################################
###                        7. Save checkpoints and outputs                   ###
################################################################################

variance_decomposition <- tibble::tibble(
  quantity = c(
    "Variance of log(petition count) across entities",
    "Variance of log(per-petition fee) across entities",
    "Share of variance in log(total fees) that is variance in the outcome"
  ),
  value_num = c(
    log_count_variance_num,
    log_fee_variance_num,
    share_of_regressor_variance_from_count_num
  ),
  source_note = "Computed from published Technical Appendix Tables A.1 and A.2"
)

saveRDS(
  simulation_results,
  file.path(path_processed, "mechanical_identity_results.rds")
)

arrow::write_parquet(
  simulation_results,
  file.path(path_processed, "mechanical_identity_results.parquet"),
  compression = "snappy"
)

readr::write_csv(
  simulation_results,
  file.path(path_tables, "mechanical_identity_results.csv")
)

readr::write_csv(
  results_summary,
  file.path(path_tables, "mechanical_identity_summary.csv")
)

readr::write_csv(
  variance_decomposition,
  file.path(path_tables, "regressor_variance_decomposition.csv")
)

message("Saved simulation results to output/tables/ and data/processed/")
