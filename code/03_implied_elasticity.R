# 03_implied_elasticity -- elasticity implied by DHS's own $100,000 natural experiment
# Author - Jiaxin He
# research title - H-1B Fee Analysis and Comment Letter
# research question - What are the factual and methodological errors made by the DHS?

rm(list = ls())
options(scipen = 999)
set.seed(42)

################################################################################
###                           1. Load packages                               ###
################################################################################

library(tidyverse)
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
###                     3. Load the appendix evidence                        ###
################################################################################

table_a7 <- readr::read_csv(
  file.path(path_raw, "table_a7_consular_receipts.csv"),
  show_col_types = FALSE
)

table_a6 <- readr::read_csv(
  file.path(path_raw, "table_a6_firm_registrations.csv"),
  show_col_types = FALSE
)

table_a7_totals <- table_a7 |>
  dplyr::filter(month_of_receipt == "Total")

message("Loaded Tables A.6 and A.7")

################################################################################
###          4. Elasticity implied by the Proclamation payment (A.7)         ###
################################################################################

# 2) The Proclamation added a $100,000 payment effective September 21, 2025.
#    Table A.7 reports initial consular I-129 H-1B receipts in matched 245-day
#    windows. The pre-period is the immediately preceding year.
proclamation_payment_num <- 100000

receipts_pre_int <- table_a7_totals$window_2024_25
receipts_post_int <- table_a7_totals$window_2025_26

# 3) The baseline all-in cost of filing is not stated in the appendix, so report
#    the implied elasticity across a defensible range. $460 and $1,380 are the
#    I-129 H-1B fees in Table A.1; higher values allow for the other statutory
#    fees, premium processing, and legal costs a petitioner actually bears.
baseline_cost_grid <- tibble::tibble(
  baseline_cost_num = c(460, 780, 1380, 2000, 3000, 5000, 10000, 20000),
  baseline_cost_note = c(
    "I-129 H-1B fee, pre-2024 rule (Table A.1)",
    "I-129 H-1B fee alone, post-2024 rule",
    "I-129 H-1B fee, post-2024 rule, largest entities (Table A.1)",
    "fee plus modest filing overhead",
    "fee plus fraud prevention and ACWIA fees",
    "fee plus statutory fees and premium processing",
    "fee plus statutory fees, premium processing, and legal costs",
    "upper bound on all-in petition cost"
  )
)

proclamation_elasticity <- baseline_cost_grid |>
  dplyr::mutate(
    receipts_pre_int = receipts_pre_int,
    receipts_post_int = receipts_post_int,
    quantity_change_pct = 100 * (receipts_post_int - receipts_pre_int) /
      receipts_pre_int,
    log_quantity_change_num = log(receipts_post_int / receipts_pre_int),
    cost_post_num = baseline_cost_num + proclamation_payment_num,
    log_price_change_num = log(cost_post_num / baseline_cost_num),
    implied_elasticity_num = log_quantity_change_num / log_price_change_num
  )

message(
  "Implied elasticity from Table A.7 ranges from ",
  sprintf("%.2f", max(proclamation_elasticity$implied_elasticity_num)),
  " to ",
  sprintf("%.2f", min(proclamation_elasticity$implied_elasticity_num)),
  " across baseline cost assumptions -- all NEGATIVE."
)

################################################################################
###        5. Registration response among the firms DHS itself lists (A.6)   ###
################################################################################

# 4) Table A.6 shows cap registrations before and after the $100,000 payment for
#    the 30 firms with the largest declines. This is selection on the outcome, so
#    it cannot identify a population elasticity -- but it does bound the response
#    of the program's heaviest users, which is the population the fee targets.
table_a6_aggregate <- table_a6 |>
  dplyr::summarise(
    firms_int = dplyr::n(),
    registrations_fy2026_int = sum(cap_fy2026_registrations),
    registrations_fy2027_int = sum(cap_fy2027_registrations),
    firms_increasing_int = sum(cap_fy2027_registrations > cap_fy2026_registrations)
  ) |>
  dplyr::mutate(
    change_pct = 100 * (registrations_fy2027_int - registrations_fy2026_int) /
      registrations_fy2026_int,
    log_quantity_change_num = log(registrations_fy2027_int / registrations_fy2026_int)
  )

# 5) Report the same elasticity calculation for the listed firms, flagged clearly
#    as descriptive of a sample selected on the dependent variable
listed_firm_elasticity <- baseline_cost_grid |>
  dplyr::mutate(
    log_quantity_change_num = table_a6_aggregate$log_quantity_change_num,
    log_price_change_num = log(
      (baseline_cost_num + proclamation_payment_num) / baseline_cost_num
    ),
    implied_elasticity_num = log_quantity_change_num / log_price_change_num,
    caveat = "Descriptive only: Table A.6 is selected on the dependent variable"
  )

message(
  "Among the 30 firms DHS lists, registrations fell from ",
  format(table_a6_aggregate$registrations_fy2026_int, big.mark = ","), " to ",
  format(table_a6_aggregate$registrations_fy2027_int, big.mark = ","), " (",
  sprintf("%.1f", table_a6_aggregate$change_pct), "%); firms increasing: ",
  table_a6_aggregate$firms_increasing_int
)

################################################################################
###       6. What DHS's own elasticity would have predicted, and the gap     ###
################################################################################

# 6) Apply DHS's reported elasticities to the Proclamation price change to show
#    what their own parameters imply for quantity, against what was observed
dhs_reported_elasticity <- tibble::tibble(
  dhs_estimate_source = c(
    "Table A.4 cross-section, FY2025",
    "Table A.4 cross-section, FY2021",
    "Table A.5 panel (long run)",
    "Table A.5 panel, read with the sign a demand elasticity must have"
  ),
  dhs_elasticity_num = c(0.94, 0.97, 0.62, -0.62)
)

# 7) Use a $1,380 baseline, the highest I-129 H-1B fee DHS itself reports
baseline_for_comparison_num <- 1380
log_price_change_comparison_num <- log(
  (baseline_for_comparison_num + proclamation_payment_num) /
    baseline_for_comparison_num
)

prediction_comparison <- dhs_reported_elasticity |>
  dplyr::mutate(
    predicted_log_quantity_change_num = dhs_elasticity_num *
      log_price_change_comparison_num,
    predicted_quantity_change_pct = 100 *
      (exp(predicted_log_quantity_change_num) - 1),
    predicted_receipts_int = round(
      receipts_pre_int * exp(predicted_log_quantity_change_num)
    ),
    observed_receipts_int = receipts_post_int,
    observed_quantity_change_pct = 100 *
      (receipts_post_int - receipts_pre_int) / receipts_pre_int
  )

message("")
message("=== DHS's own parameters vs. DHS's own observed outcome ===")
message(
  "  Observed: ", format(receipts_pre_int, big.mark = ","), " -> ",
  format(receipts_post_int, big.mark = ","), " receipts (",
  sprintf("%.1f", prediction_comparison$observed_quantity_change_pct[1L]), "%)"
)
for (row_index in seq_len(nrow(prediction_comparison))) {
  message(
    "  ", prediction_comparison$dhs_estimate_source[row_index],
    " (elasticity ",
    sprintf("%+.2f", prediction_comparison$dhs_elasticity_num[row_index]),
    ") predicts ",
    format(prediction_comparison$predicted_receipts_int[row_index], big.mark = ","),
    " receipts (",
    sprintf("%+.1f", prediction_comparison$predicted_quantity_change_pct[row_index]),
    "%)"
  )
}

################################################################################
###            7. Out-of-sample extrapolation, stated in log points          ###
################################################################################

# 8) The elasticity is estimated over the April 1, 2024 fee change and then used
#    to reason about a fee two orders of magnitude larger
extrapolation_range <- tibble::tibble(
  comparison = c(
    "Fee change in the estimation window (Table A.1)",
    "Fee change contemplated by the proposed rule"
  ),
  fee_from_num = c(460, 1380),
  fee_to_num = c(1380, 1380 + 103265)
) |>
  dplyr::mutate(
    multiple_num = fee_to_num / fee_from_num,
    log_points_num = log(fee_to_num / fee_from_num)
  )

extrapolation_ratio_num <- extrapolation_range$log_points_num[2L] /
  extrapolation_range$log_points_num[1L]

message("")
message(
  "Extrapolation: the proposed fee change is ",
  sprintf("%.1f", extrapolation_ratio_num),
  " times larger in log points than the change used to estimate the elasticity."
)

################################################################################
###                        8. Save checkpoints and outputs                   ###
################################################################################

implied_elasticity_all <- dplyr::bind_rows(
  proclamation_elasticity |>
    dplyr::mutate(
      population = "Initial consular I-129 H-1B receipts (Table A.7)",
      caveat = "Matched 245-day windows; population-level, not selected on outcome"
    ) |>
    dplyr::select(
      population, baseline_cost_num, baseline_cost_note,
      log_quantity_change_num, log_price_change_num, implied_elasticity_num, caveat
    ),
  listed_firm_elasticity |>
    dplyr::mutate(
      population = "Cap registrations, 30 firms listed in Table A.6"
    ) |>
    dplyr::select(
      population, baseline_cost_num, baseline_cost_note,
      log_quantity_change_num, log_price_change_num, implied_elasticity_num, caveat
    )
)

saveRDS(
  implied_elasticity_all,
  file.path(path_processed, "implied_elasticity.rds")
)

arrow::write_parquet(
  implied_elasticity_all,
  file.path(path_processed, "implied_elasticity.parquet"),
  compression = "snappy"
)

readr::write_csv(
  implied_elasticity_all,
  file.path(path_tables, "implied_elasticity.csv")
)

readr::write_csv(
  prediction_comparison,
  file.path(path_tables, "dhs_parameter_prediction_vs_observed.csv")
)

readr::write_csv(
  extrapolation_range,
  file.path(path_tables, "extrapolation_range.csv")
)

message("Saved implied elasticity outputs to output/tables/ and data/processed/")
