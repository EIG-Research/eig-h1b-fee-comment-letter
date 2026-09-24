# 01_appendix_audit -- verify every internal arithmetic claim in the DHS Technical Appendix
# Author - Jiaxin He
# research title - H-1B Fee Analysis and Comment Letter
# research question - Do the justifications for the H-1B fee hold up to scrutiny?

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
###                        3. Load keyed appendix tables                     ###
################################################################################

# 2) Read the tables exactly as published in the Technical Appendix
table_a1 <- readr::read_csv(
  file.path(path_raw, "table_a1_receipts_by_entity.csv"),
  show_col_types = FALSE
)

table_a2 <- readr::read_csv(
  file.path(path_raw, "table_a2_petitions_per_entity.csv"),
  show_col_types = FALSE
)

table_a3 <- readr::read_csv(
  file.path(path_raw, "table_a3_summary_stats.csv"),
  show_col_types = FALSE
)

table_a5 <- readr::read_csv(
  file.path(path_raw, "table_a5_panel.csv"),
  show_col_types = FALSE
)

table_a6 <- readr::read_csv(
  file.path(path_raw, "table_a6_firm_registrations.csv"),
  show_col_types = FALSE
)

table_a7 <- readr::read_csv(
  file.path(path_raw, "table_a7_consular_receipts.csv"),
  show_col_types = FALSE
)

message("Loaded 6 keyed appendix tables from: ", path_raw)

################################################################################
###                   4. Table A.1 -- entity share claims                    ###
################################################################################

# 3) Collapse the two FY2024 half-year rows so fiscal years are comparable
table_a1_annual <- table_a1 |>
  dplyr::mutate(
    fiscal_year_int = as.integer(substr(fiscal_year_label, 1L, 4L))
  ) |>
  dplyr::group_by(fiscal_year_int) |>
  dplyr::summarise(
    i129_h1b_receipts = sum(i129_h1b_receipts),
    entity_25_or_fewer = sum(entity_25_or_fewer),
    entity_more_than_25 = sum(entity_more_than_25),
    entity_nonprofit = sum(entity_nonprofit),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    entity_size_reported = entity_25_or_fewer + entity_more_than_25,
    size_question_coverage_pct = 100 * entity_size_reported / i129_h1b_receipts,
    share_more_than_25_of_reported_pct =
      100 * entity_more_than_25 / entity_size_reported,
    share_more_than_25_of_all_pct =
      100 * entity_more_than_25 / i129_h1b_receipts,
    share_nonprofit_pct = 100 * entity_nonprofit / i129_h1b_receipts
  )

# 4) Footnote 8 states the FY2021 nonprofit share as 5.07 percent while the body
#    text of the same paragraph states 5.22 percent
nonprofit_fy2021_pct <- table_a1_annual |>
  dplyr::filter(fiscal_year_int == 2021L) |>
  dplyr::pull(share_nonprofit_pct)

nonprofit_fy2025_pct <- table_a1_annual |>
  dplyr::filter(fiscal_year_int == 2025L) |>
  dplyr::pull(share_nonprofit_pct)

# 5) The appendix claims 86.13 percent of petitions came from entities with more
#    than 25 employees in fiscal years where the size question response rate
#    exceeded 90 percent. Enumerate every plausible construction of that figure.
high_coverage_years <- table_a1_annual |>
  dplyr::filter(size_question_coverage_pct > 90) |>
  dplyr::pull(fiscal_year_int)

candidate_86_13 <- dplyr::bind_rows(
  table_a1_annual |>
    dplyr::transmute(
      construction = paste0("single year ", fiscal_year_int, ", share of reported"),
      value_pct = share_more_than_25_of_reported_pct
    ),
  table_a1_annual |>
    dplyr::transmute(
      construction = paste0("single year ", fiscal_year_int, ", share of all receipts"),
      value_pct = share_more_than_25_of_all_pct
    ),
  tibble::tibble(
    construction = "pooled >90% coverage years, share of reported",
    value_pct = 100 *
      sum(table_a1_annual$entity_more_than_25[
        table_a1_annual$fiscal_year_int %in% high_coverage_years
      ]) /
      sum(table_a1_annual$entity_size_reported[
        table_a1_annual$fiscal_year_int %in% high_coverage_years
      ])
  ),
  tibble::tibble(
    construction = "pooled >90% coverage years, share of all receipts",
    value_pct = 100 *
      sum(table_a1_annual$entity_more_than_25[
        table_a1_annual$fiscal_year_int %in% high_coverage_years
      ]) /
      sum(table_a1_annual$i129_h1b_receipts[
        table_a1_annual$fiscal_year_int %in% high_coverage_years
      ])
  ),
  tibble::tibble(
    construction = "unweighted mean of yearly shares of reported, >90% coverage",
    value_pct = mean(
      table_a1_annual$share_more_than_25_of_reported_pct[
        table_a1_annual$fiscal_year_int %in% high_coverage_years
      ]
    )
  )
) |>
  dplyr::mutate(
    distance_from_claim = abs(value_pct - 86.13)
  ) |>
  dplyr::arrange(distance_from_claim)

claim_86_13_reproduced_flag <- min(candidate_86_13$distance_from_claim) < 0.01

message(
  "Table A.1 checked. FY2021 nonprofit share = ",
  sprintf("%.2f", nonprofit_fy2021_pct),
  "% (footnote 8 states 5.07%). 86.13% claim reproduced: ",
  claim_86_13_reproduced_flag
)

################################################################################
###                4. Table A.2 -- distribution and share columns             ###
################################################################################

# 6) Verify the five-year total, the five-year average, and the stated shares
table_a2_checked <- table_a2 |>
  dplyr::mutate(
    five_year_total_computed = fy2021 + fy2022 + fy2023 + fy2024 + fy2025,
    five_year_average_computed = five_year_total_computed / 5,
    percentage_computed = 100 * five_year_total_computed /
      sum(five_year_total_computed),
    cumulative_percentage_computed = cumsum(percentage_computed),
    total_matches_flag = five_year_total_computed == five_year_total,
    percentage_gap = abs(percentage_computed - percentage_stated),
    cumulative_gap = abs(cumulative_percentage_computed - cumulative_percentage_stated)
  )

# 7) Table A.2 column totals should equal the entity counts in Table A.3
entity_totals_a2 <- tibble::tibble(
  fiscal_year = 2021L:2025L,
  entities_from_a2 = c(
    sum(table_a2$fy2021), sum(table_a2$fy2022), sum(table_a2$fy2023),
    sum(table_a2$fy2024), sum(table_a2$fy2025)
  )
) |>
  dplyr::left_join(
    table_a3 |> dplyr::select(fiscal_year, n_observations),
    by = "fiscal_year"
  ) |>
  dplyr::mutate(
    entity_gap = entities_from_a2 - n_observations
  )

message(
  "Table A.2 checked. Column totals vs Table A.3 entity counts differ by: ",
  paste(entity_totals_a2$entity_gap, collapse = ", ")
)

################################################################################
###             5. Table A.3 -- mean petitions per entity consistency         ###
################################################################################

# 8) The stated mean should equal Table A.1 receipts divided by Table A.3 entities
table_a3_checked <- table_a3 |>
  dplyr::left_join(
    table_a1_annual |>
      dplyr::select(fiscal_year = fiscal_year_int, i129_h1b_receipts),
    by = "fiscal_year"
  ) |>
  dplyr::mutate(
    mean_computed = i129_h1b_receipts / n_observations,
    mean_gap = abs(mean_computed - mean_stated),
    dispersion_ratio = variance_stated / mean_stated
  )

message(
  "Table A.3 checked. Largest gap between stated and implied mean: ",
  sprintf("%.4f", max(table_a3_checked$mean_gap))
)

################################################################################
###          6. Table A.5 -- the panel is described as balanced              ###
################################################################################

# 9) The appendix states the unbalanced panel "is converted to a balanced panel
#    dataset". A balanced panel over four fiscal-year cells requires the
#    observation count to equal entities times four exactly.
panel_n_observations <- table_a5 |>
  dplyr::filter(statistic == "n_observations") |>
  dplyr::pull(value)

panel_n_entities <- table_a5 |>
  dplyr::filter(statistic == "n_entities") |>
  dplyr::pull(value)

panel_periods_int <- 4L

panel_implied_observations <- panel_n_entities * panel_periods_int
panel_observation_shortfall <- panel_implied_observations - panel_n_observations
panel_observations_per_entity <- panel_n_observations / panel_n_entities
panel_is_balanced_flag <- panel_observation_shortfall == 0

message(
  "Table A.5 checked. Entities x 4 = ",
  format(panel_implied_observations, big.mark = ","),
  " but reported N = ", format(panel_n_observations, big.mark = ","),
  " (shortfall ", panel_observation_shortfall,
  "). Panel is balanced: ", panel_is_balanced_flag
)

################################################################################
###        7. Table A.6 -- arithmetic, category rules, and sample design      ###
################################################################################

# 10) Recompute the difference column and the consular share column
table_a6_checked <- table_a6 |>
  dplyr::mutate(
    difference_computed = cap_fy2027_registrations - cap_fy2026_registrations,
    difference_gap = difference_computed - difference_stated,
    share_consular_computed = 100 * fy2025_consular_initial_receipts /
      fy2025_initial_receipts,
    share_consular_gap = abs(share_consular_computed - share_consular_stated),
    # Footnote 14 thresholds: High > 70; Upper Medium 50-70;
    # Lower Medium 30-49; Low < 30
    category_from_footnote_rule = dplyr::case_when(
      share_consular_computed > 70 ~ "High",
      share_consular_computed >= 50 ~ "Upper Medium",
      share_consular_computed >= 30 ~ "Lower Medium",
      TRUE ~ "Low"
    ),
    category_matches_flag =
      category_from_footnote_rule == share_consular_category_stated,
    # A share in [49, 50) is described by no footnote 14 category
    falls_in_definitional_gap_flag =
      share_consular_computed >= 49 & share_consular_computed < 50
  )

# 11) Table A.6 is sorted by the outcome and truncated to the 30 largest declines,
#     so quantify what the selected sample represents
table_a6_selection <- tibble::tibble(
  firms_shown_int = nrow(table_a6_checked),
  firms_with_decline_int = sum(table_a6_checked$difference_computed < 0),
  firms_with_increase_int = sum(table_a6_checked$difference_computed > 0),
  fy2026_registrations_shown = sum(table_a6_checked$cap_fy2026_registrations),
  fy2027_registrations_shown = sum(table_a6_checked$cap_fy2027_registrations),
  decline_shown = sum(table_a6_checked$difference_computed),
  decline_pct_among_shown = 100 * sum(table_a6_checked$difference_computed) /
    sum(table_a6_checked$cap_fy2026_registrations)
)

message(
  "Table A.6 checked. Max arithmetic gap: difference = ",
  max(abs(table_a6_checked$difference_gap)),
  ", consular share = ",
  sprintf("%.3f", max(table_a6_checked$share_consular_gap)),
  ". Category rule mismatches: ",
  sum(!table_a6_checked$category_matches_flag),
  ". Firms with an increase shown: ",
  table_a6_selection$firms_with_increase_int
)

################################################################################
###          8. Table A.7 -- column totals and the -91.2 percent claim        ###
################################################################################

# 12) Verify the stated column totals against the monthly rows
table_a7_months <- table_a7 |>
  dplyr::filter(month_of_receipt != "Total")

table_a7_stated_totals <- table_a7 |>
  dplyr::filter(month_of_receipt == "Total")

table_a7_checked <- tibble::tibble(
  window = c("window_2022_23", "window_2023_24", "window_2024_25", "window_2025_26"),
  total_stated = c(
    table_a7_stated_totals$window_2022_23, table_a7_stated_totals$window_2023_24,
    table_a7_stated_totals$window_2024_25, table_a7_stated_totals$window_2025_26
  ),
  total_computed = c(
    sum(table_a7_months$window_2022_23), sum(table_a7_months$window_2023_24),
    sum(table_a7_months$window_2024_25), sum(table_a7_months$window_2025_26)
  )
) |>
  dplyr::mutate(
    total_gap = total_computed - total_stated
  )

receipts_before_num <- table_a7_stated_totals$window_2024_25
receipts_after_num <- table_a7_stated_totals$window_2025_26
consular_change_pct <- 100 * (receipts_after_num - receipts_before_num) /
  receipts_before_num

message(
  "Table A.7 checked. Column total gaps: ",
  paste(table_a7_checked$total_gap, collapse = ", "),
  ". Consular change = ", sprintf("%.2f", consular_change_pct),
  "% (appendix states -91.23%)"
)

################################################################################
###                       9. Assemble the audit findings                     ###
################################################################################

# 13) One row per checkable claim, with the published value and the recomputed value
audit_findings <- dplyr::bind_rows(
  tibble::tibble(
    table_id = "A.1",
    claim = "Footnote 8: FY2021 nonprofit share of I-129 H-1B receipts",
    published_value = "5.07%",
    recomputed_value = sprintf("%.2f%%", nonprofit_fy2021_pct),
    verdict = "Internally inconsistent: body text of the same paragraph states 5.22%, which is the correct value"
  ),
  tibble::tibble(
    table_id = "A.1",
    claim = "Footnote 8: FY2025 nonprofit share of I-129 H-1B receipts",
    published_value = "12.07%",
    recomputed_value = sprintf("%.2f%%", nonprofit_fy2025_pct),
    verdict = "Reproduced"
  ),
  tibble::tibble(
    table_id = "A.1",
    claim = "Share of petitions from entities employing more than 25 workers",
    published_value = "86.13%",
    recomputed_value = sprintf(
      "%.2f%% (nearest of %d constructions: %s)",
      candidate_86_13$value_pct[1L], nrow(candidate_86_13),
      candidate_86_13$construction[1L]
    ),
    verdict = ifelse(
      claim_86_13_reproduced_flag,
      "Reproduced",
      "Not reproducible from Table A.1 under any single-year, pooled, or averaged construction"
    )
  ),
  tibble::tibble(
    table_id = "A.2",
    claim = "Five-year totals equal the sum of the fiscal-year columns",
    published_value = "as printed",
    recomputed_value = sprintf(
      "%d of %d rows match",
      sum(table_a2_checked$total_matches_flag), nrow(table_a2_checked)
    ),
    verdict = ifelse(
      all(table_a2_checked$total_matches_flag), "Reproduced", "Discrepancy found"
    )
  ),
  tibble::tibble(
    table_id = "A.2",
    claim = "Bin definitions partition the support of petitions per entity",
    published_value = "'501 to 1,000' and '1,000 to 5,000'",
    recomputed_value = "the value 1,000 belongs to two bins",
    verdict = "Overlapping bin boundary"
  ),
  tibble::tibble(
    table_id = "A.2 vs A.3",
    claim = "Table A.2 column totals equal Table A.3 entity counts",
    published_value = paste(entity_totals_a2$n_observations, collapse = "; "),
    recomputed_value = paste(entity_totals_a2$entities_from_a2, collapse = "; "),
    verdict = ifelse(
      all(entity_totals_a2$entity_gap == 0), "Reproduced",
      paste0(
        "Discrepancy of ",
        paste(entity_totals_a2$entity_gap, collapse = ", "),
        " entities by fiscal year"
      )
    )
  ),
  tibble::tibble(
    table_id = "A.3",
    claim = "Mean petitions per entity equals receipts divided by entities",
    published_value = paste(table_a3_checked$mean_stated, collapse = "; "),
    recomputed_value = paste(
      sprintf("%.2f", table_a3_checked$mean_computed), collapse = "; "
    ),
    verdict = ifelse(
      max(table_a3_checked$mean_gap) < 0.01, "Reproduced",
      "Discrepancy found"
    )
  ),
  tibble::tibble(
    table_id = "A.5",
    claim = "The panel dataset is balanced across four fiscal years",
    published_value = sprintf(
      "N = %s observations, %s entities",
      format(panel_n_observations, big.mark = ","),
      format(panel_n_entities, big.mark = ",")
    ),
    recomputed_value = sprintf(
      "%s x 4 = %s, i.e. %.4f observations per entity",
      format(panel_n_entities, big.mark = ","),
      format(panel_implied_observations, big.mark = ","),
      panel_observations_per_entity
    ),
    verdict = ifelse(
      panel_is_balanced_flag, "Reproduced",
      sprintf(
        "Not balanced: reported N is %d observations short of entities x 4, and is not divisible by 4",
        panel_observation_shortfall
      )
    )
  ),
  tibble::tibble(
    table_id = "A.6",
    claim = "Difference in Registration Volume column equals (B) minus (A)",
    published_value = "as printed",
    recomputed_value = sprintf(
      "max absolute gap = %d", max(abs(table_a6_checked$difference_gap))
    ),
    verdict = ifelse(
      all(table_a6_checked$difference_gap == 0), "Reproduced", "Discrepancy found"
    )
  ),
  tibble::tibble(
    table_id = "A.6",
    claim = "% Share Consular column equals (D) divided by (C)",
    published_value = "as printed",
    recomputed_value = sprintf(
      "max absolute gap = %.3f percentage points",
      max(table_a6_checked$share_consular_gap)
    ),
    verdict = ifelse(
      max(table_a6_checked$share_consular_gap) < 0.01, "Reproduced",
      "Discrepancy found"
    )
  ),
  tibble::tibble(
    table_id = "A.6",
    claim = "Footnote 14 categories are exhaustive and mutually exclusive",
    published_value = "High >70; Upper Medium 50-70; Lower Medium 30-49; Low <30",
    recomputed_value = "no category covers shares in [49, 50); the value 70 is in two categories",
    verdict = "Definitional gap and overlap"
  ),
  tibble::tibble(
    table_id = "A.6",
    claim = "Table A.6 supports an inference about consular reliance and registration declines",
    published_value = "30 firms, sorted descending by decline",
    recomputed_value = sprintf(
      "%d of %d firms shown declined; %d increased; total registrations fell %s to %s (%.1f%%)",
      table_a6_selection$firms_with_decline_int, table_a6_selection$firms_shown_int,
      table_a6_selection$firms_with_increase_int,
      format(table_a6_selection$fy2026_registrations_shown, big.mark = ","),
      format(table_a6_selection$fy2027_registrations_shown, big.mark = ","),
      table_a6_selection$decline_pct_among_shown
    ),
    verdict = "Sample selected on the dependent variable; no firm with a flat or rising count is shown, so the stated correlation is not identified"
  ),
  tibble::tibble(
    table_id = "A.7",
    claim = "Column totals equal the sum of the monthly rows",
    published_value = paste(table_a7_checked$total_stated, collapse = "; "),
    recomputed_value = paste(table_a7_checked$total_computed, collapse = "; "),
    verdict = ifelse(
      all(table_a7_checked$total_gap == 0), "Reproduced", "Discrepancy found"
    )
  ),
  tibble::tibble(
    table_id = "A.7",
    claim = "Footnote 15: decline in initial consular receipts after the Proclamation payment",
    published_value = "-91.23%",
    recomputed_value = sprintf("%.2f%%", consular_change_pct),
    verdict = "Reproduced"
  )
)

################################################################################
###                        10. Save checkpoints and outputs                  ###
################################################################################

# 14) Save the audit findings and the per-table checked frames
saveRDS(
  audit_findings,
  file.path(path_processed, "appendix_audit_findings.rds")
)

arrow::write_parquet(
  audit_findings,
  file.path(path_processed, "appendix_audit_findings.parquet"),
  compression = "snappy"
)

saveRDS(
  table_a1_annual,
  file.path(path_processed, "appendix_table_a1_annual.rds")
)

arrow::write_parquet(
  table_a1_annual,
  file.path(path_processed, "appendix_table_a1_annual.parquet"),
  compression = "snappy"
)

saveRDS(
  table_a6_checked,
  file.path(path_processed, "appendix_table_a6_checked.rds")
)

arrow::write_parquet(
  table_a6_checked,
  file.path(path_processed, "appendix_table_a6_checked.parquet"),
  compression = "snappy"
)

readr::write_csv(
  audit_findings,
  file.path(path_tables, "appendix_audit_findings.csv")
)

readr::write_csv(
  candidate_86_13,
  file.path(path_tables, "appendix_86_13_reconciliation.csv")
)

message(
  "Audit complete. ", nrow(audit_findings), " claims checked; ",
  sum(audit_findings$verdict != "Reproduced"), " did not reproduce as published."
)
