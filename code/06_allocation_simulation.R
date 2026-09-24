# 06_allocation_simulation -- the H-1B wage gap under four allocation regimes
# Author - Jiaxin He
# research title - H-1B Fee Analysis and Comment Letter
# research question - Has the weighted lottery plus updated OFLC wage levels already
#   closed the H-1B wage gap, making a visa fee unnecessary?
#
# Simulates four ways of allocating the 85,000 cap-subject H-1B visas and estimates
# the resulting wage gap under two models:
#
#   Borjas model -- equation (1) of Borjas, NBER WP 34793, revised August 2026, as
#     replicated in 04_borjas_replication.R: log wages on an H-1B
#     indicator absorbing year, place-of-work metro (2023 CBSA with a state FIPS
#     pseudo-metro fallback), education, age, gender, occupation and industry fixed
#     effects; ACS person weights for natives and 1 for H-1B workers; robust SEs.
#     Wages are in 2025 dollars, deflated on monthly CPI-U over the 12-month
#     earnings window (H-1B) or the mean of the two overlapping annual CPIs (ACS).
#
#   Tenure model -- the same estimate corrected for the job-seniority bias. Borjas
#     (pp. 17-18) merges the CPS Job Tenure Supplement with the ASEC, finds a
#     seniority wage premium of 4.1 percent among the 88.6 percent of natives who
#     are not new hires, and reports the resulting bias as 0.886 x 0.041 = 3.6
#     points: "The ACS wage gap estimated in Table 2 is 16.1 percent. Adjusting for
#     the seniority bias would imply a 'true' wage gap of about 12.5 percent."
#     It is a constant additive shift, not a separate regression, and is applied
#     here exactly as 05_borjas_labor_demand.R applies it.
#
# Allocation regimes:
#   1. Old unweighted lottery                       (sanity check)
#   2. Weighted lottery, old OFLC wage levels       (Old_OFLC_wage_level_wt)
#   3. Weighted lottery, new OFLC wage levels       (New_OFLC_wage_level_strict_wt)
#   4. EIG wage-ranking proposal                    (RPP- and NPV-adjusted wage)
#
# DEPENDENCY: run 04_borjas_replication.R first. This script reads
# the pooled regression sample it checkpoints to data/processed, which is what
# carries the August draft's CPI deflation, place-of-work metro and industry codes.
# Rebuilding that sample here would duplicate roughly 400 lines and let the two
# scripts drift apart.

rm(list = ls())
options(scipen = 999)
set.seed(42)

################################################################################
###                           1. Load packages                               ###
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(fixest)
  library(arrow)
})

################################################################################
###                             2. Set paths                                 ###
################################################################################

if (requireNamespace("here", quietly = TRUE)) {
  path_project <- here::here()
} else {
  path_project <- getwd()
  message("Package 'here' unavailable; using working directory: ", path_project)
}

path_raw <- file.path(path_project, "data", "raw")
path_foia <- file.path(path_raw, "FOIA Data")
path_processed <- file.path(path_project, "data", "processed")
path_output <- file.path(path_project, "output", "tables")

for (path_name in c(path_processed, path_output)) {
  if (!dir.exists(path_name)) {
    dir.create(path_name, recursive = TRUE)
  }
}

regression_data_file <- file.path(
  path_processed, "borjas_august_regression_data.rds"
)
wage_level_file <- file.path(
  path_processed, "FY2022-2024 I-129 Data with New Wage Levels.csv"
)
entry_total_file <- file.path(path_processed, "FY2022-2024 I-129 Data Cleaned.csv")
rpp_file <- file.path(path_raw, "RPP 2021-23 Deflators.csv")
npv_file <- file.path(path_processed, "ACS 2021-23 NPV Projections.csv")

# 1) Model constants.
simulation_years_int <- 2022L:2024L   # the wage-level file covers FY2022-FY2024
n_sim <- 100L
# 1a) The synthetic pool holds about 1.4 million registrations per replication,
#     so all 100 at once would be roughly 141 million rows. Replications are
#     drawn in batches instead, which leaves the draws statistically identical
#     and keeps peak memory near 2 GB.
sim_batch_size_int <- 20L
cap_unres <- 65000L
cap_res <- 20000L
seniority_adjustment_num <- 0.036     # Borjas pp. 17-18, 0.886 x 0.041

borjas_fixed_effects_chr <- paste(
  "year_int + metro_area + education_chr + age_int + gender_chr + occ_code",
  "+ ind_code"
)

analysis_columns <- c(
  "is_h1b_int", "year_int", "log_wage_num", "wage_real_num", "metro_area",
  "age_int", "gender_chr", "education_chr", "occ_code", "ind_code", "weight_num"
)

###########################################
#### Declare H1-B Allocation Functions ####
###########################################

# 2) The pool passed to each function is an index table: one row per synthetic
#    registration, carrying only the fields selection depends on plus
#    base_row_int, the row of base_sample it was drawn from. The regression
#    payload is attached to the 85,000 winners inside summarize_model. At
#    n_sim = 20 the pool holds roughly 28 million rows, so materializing the full
#    payload for every registration is not affordable.

####################### Lottery-based Systems #######################
## -----------------------------
## 1) Current lottery
## -----------------------------
current_lottery <- function(DT, cap_res, cap_unres) {
  # Unreserved 65k from the whole pool
  unres_row_int <- DT[
    , .(row_id_int = .I[sample.int(.N, size = min(.N, cap_unres))]),
    by = .(registration_lottery_year, sim_id)
  ]$row_id_int

  # Remove unreserved winners; keep only reserved pool
  remaining_flag <- rep(TRUE, nrow(DT))
  remaining_flag[unres_row_int] <- FALSE
  remaining_current <- DT[remaining_flag & DT$sample_flag == "reserved"]

  # Reserved 20k from remaining reserved
  res_row_int <- remaining_current[
    , .(row_id_int = .I[sample.int(.N, size = min(.N, cap_res))]),
    by = .(registration_lottery_year, sim_id)
  ]$row_id_int

  # All winners under current lottery:
  winners_current <- rbindlist(
    list(DT[unres_row_int], remaining_current[res_row_int]),
    use.names = TRUE, fill = TRUE
  )
  remove(remaining_current, unres_row_int, res_row_int)

  # Return summary statistics, with the per-petition selection counts attached
  # so downstream scripts can reconstruct the composition of the winners without
  # repeating the draw.
  summary_current <- winners_current[
    , {
      summarize_model(.SD, "current lottery")
    },
    by = sim_id
  ]

  data.table::setattr(
    summary_current, "selection_counts",
    winners_current[, .(selection_count_int = .N), by = base_row_int]
  )

  summary_current
}

## -----------------------------------------
## 2) Proposed rule
## -----------------------------------------
proposed_rule <- function(DT, cap_res, cap_unres, weight_col = "wage_level_weight") {
  # 3) Weighted selection without replacement uses the Efraimidis-Spirakis key
  #    log(U_i) / w_i, taking the k largest keys. That is distributionally
  #    identical to sample(prob = w, replace = FALSE), which R implements by
  #    successive sampling in O(n*k) time -- at n = 500,000 and k = 65,000 that
  #    is roughly 3e10 operations per draw and does not finish. The key method
  #    is O(n log n) and completes in milliseconds.

  # Reserved 65k, wage level weighted
  unres_row_int <- DT[
    , .(row_id_int = .I[
      order(log(runif(.N)) / get(weight_col), decreasing = TRUE)[
        seq_len(min(.N, cap_unres))
      ]
    ]),
    by = .(registration_lottery_year, sim_id)
  ]$row_id_int

  # Remove unreserved winners; keep only reserved pool
  remaining_flag <- rep(TRUE, nrow(DT))
  remaining_flag[unres_row_int] <- FALSE
  remaining_proposed <- DT[remaining_flag & DT$sample_flag == "reserved"]

  # Reserved 20k, wage level weighted
  res_row_int <- remaining_proposed[
    , .(row_id_int = .I[
      order(log(runif(.N)) / get(weight_col), decreasing = TRUE)[
        seq_len(min(.N, cap_res))
      ]
    ]),
    by = .(registration_lottery_year, sim_id)
  ]$row_id_int

  # All winners under proposed rule:
  winners_proposed <- rbindlist(
    list(DT[unres_row_int], remaining_proposed[res_row_int]),
    use.names = TRUE, fill = TRUE
  )
  remove(remaining_proposed, unres_row_int, res_row_int)

  # Return summary statistics, with the per-petition selection counts attached
  summary_proposed <- winners_proposed[
    , {
      summarize_model(.SD, paste0("proposed rule (", weight_col, ")"))
    },
    by = sim_id
  ]

  data.table::setattr(
    summary_proposed, "selection_counts",
    winners_proposed[, .(selection_count_int = .N), by = base_row_int]
  )

  summary_proposed
}

####################### Wage-ranking #######################
rank_select <- function(DT, cap_res, cap_unres, metric = "wage_npv_mean_3pct") {

  # NA metrics naturally fall to the end; tie-break by synth_applicant_id
  setorderv(DT, c("registration_lottery_year", "sim_id", metric, "synth_applicant_id"),
            c(1L, 1L, -1L, 1L))

  # Pick top 65000 from the entire pool
  unres_row_int <- DT[
    , .(row_id_int = .I[seq_len(min(.N, cap_unres))]),
    by = .(registration_lottery_year, sim_id)
  ]$row_id_int

  # Remove winners; restrict to remaining reserved pool
  remaining_flag <- rep(TRUE, nrow(DT))
  remaining_flag[unres_row_int] <- FALSE
  remaining <- DT[remaining_flag & DT$sample_flag == "reserved"]

  # Reserved pass from remaining reserved pool
  res_row_int <- remaining[
    , .(row_id_int = .I[seq_len(min(.N, cap_res))]),
    by = .(registration_lottery_year, sim_id)
  ]$row_id_int

  # Combine winners
  winners_ranked <- rbindlist(
    list(DT[unres_row_int], remaining[res_row_int]),
    use.names = TRUE, fill = TRUE
  )
  remove(remaining, unres_row_int, res_row_int)

  # Return summary statistics, with the per-petition selection counts attached
  summary_ranked <- winners_ranked[
    , {
      if (metric == "wage_rpp_adj") {
        summarize_model(.SD, "RPP adjusted wage ranking")
      } else if (metric == "wage_npv_mean_3pct") {
        summarize_model(.SD, "NPV adjusted wage ranking, 3% discount")
      } else if (metric == "wage_npv_mean_7pct") {
        summarize_model(.SD, "NPV adjusted wage ranking, 7% discount")
      }
    },
    by = sim_id
  ]

  data.table::setattr(
    summary_ranked, "selection_counts",
    winners_ranked[, .(selection_count_int = .N), by = base_row_int]
  )

  summary_ranked
}

###################################
#### Declare summary functions ####
###################################
summarize_model <- function(df, allocation_method) {

  # 4) Attach the regression payload to this replication's winners. The pooled
  #    sample is already restricted to ages 21-50 and to earnings between
  #    $34,780 and $1,290,000 in 04_borjas_replication.R, so no further trimming is applied
  #    here -- the old percentile trim would have re-cut an already-cut sample.
  h1b_sample <- base_sample[df$base_row_int, ..analysis_columns]

  reg_data_borjas <- rbindlist(
    list(h1b_sample, native_sample), use.names = TRUE, fill = FALSE
  )

  # ── Borjas model (August 2026 draft, equation (1), Table 2 column 5) ──
  model_borjas <- feols(
    stats::as.formula(
      paste0("log_wage_num ~ is_h1b_int | ", borjas_fixed_effects_chr)
    ),
    data = reg_data_borjas,
    weights = ~weight_num,
    vcov = "hetero"
  )

  # ── Weighted mean residual by age, H-1B rows only ──
  # 5) predict() on newdata is used rather than fitted(), which returns one value
  #    per estimation observation and is shorter than the data whenever fixest
  #    drops a fixed-effect singleton.
  reg_data_borjas[
    , resid_num := log_wage_num -
      stats::predict(model_borjas, newdata = reg_data_borjas)
  ]

  resid_borjas <- reg_data_borjas[
    is_h1b_int == 1L,
    .(mean_resid_borjas = weighted.mean(resid_num, weight_num, na.rm = TRUE),
      sum_weight_borjas = sum(weight_num[!is.na(resid_num)])),
    by = .(age = age_int)
  ]

  # ── Tenure model: the Borjas estimate net of the 3.6-point seniority bias ──
  resid_borjas[
    , `:=`(
      alloc_method = allocation_method,
      n_winners_int = nrow(df),
      coef_borjas = unname(stats::coef(model_borjas)["is_h1b_int"]),
      se_borjas = unname(fixest::se(model_borjas)["is_h1b_int"]),
      mean_resid_tenure = mean_resid_borjas + seniority_adjustment_num,
      sum_weight_tenure = sum_weight_borjas,
      coef_tenure = unname(stats::coef(model_borjas)["is_h1b_int"]) +
        seniority_adjustment_num,
      se_tenure = unname(fixest::se(model_borjas)["is_h1b_int"])
    )
  ]

  resid_borjas[order(age)]
}

################################
#### Implement Borjas Model ####
################################

# 5) The pooled regression sample built by 04_borjas_replication.R. It already carries the
#    monthly-CPI real wage, the place-of-work metro, the ACS occupation and
#    industry codes, and the Table 1 sample restrictions.
if (!file.exists(regression_data_file)) {
  stop(
    "Missing ", regression_data_file,
    ". Run 04_borjas_replication.R first.",
    call. = FALSE
  )
}

regression_data <- as.data.table(readRDS(regression_data_file))

message(
  "Loaded the pooled Borjas sample: ",
  format(nrow(regression_data), big.mark = ","), " rows, FY",
  min(regression_data$year_int), "-FY", max(regression_data$year_int)
)

# 6) Both sides are restricted to FY2022-FY2024, the years for which the recomputed
#    OFLC wage levels exist. Keeping the 2021 ACS would leave a native-only year
#    that no simulated H-1B cohort can be compared against within the year effects.
h1b_columns <- c("petition_key_chr", analysis_columns)

native_sample <- regression_data[
  is_h1b_int == 0L & year_int %in% simulation_years_int, ..analysis_columns
]

h1b_observed <- regression_data[
  is_h1b_int == 1L & year_int %in% simulation_years_int, ..h1b_columns
]

message(
  "FY2022-24 natives: ", format(nrow(native_sample), big.mark = ","),
  "; FY2022-24 approved H-1B petitions: ",
  format(nrow(h1b_observed), big.mark = ",")
)

############################
#### Build base samples ####
############################

# 7) The recomputed OFLC wage levels live in a separately cleaned file whose
#    applicant_id is a fresh sequence, so it cannot be joined on the identifier.
#    The composite key written by 04_borjas_replication.R -- lottery year, LCA case number
#    stripped of punctuation, petition receive date, petition decision date,
#    annual pay and gender -- identifies the same petition in both files.
wage_levels_raw <- fread(
  wage_level_file,
  select = c(
    "registration_lottery_year", "DOL_ETA_CASE_NUMBER", "petition_recieve_date",
    "petition_decision_date", "petition_annual_pay_clean", "registration_gender",
    "petition_h1b_type", "petition_beneficiary_edu_code", "petition_worksite_state",
    "registration_age",
    "lca_wage_level", "Old_OFLC_wage_level", "New_OFLC_wage_level_strict"
  ),
  colClasses = list(character = c("DOL_ETA_CASE_NUMBER")),
  showProgress = FALSE
)

wage_levels_raw[
  , petition_key_chr := paste(
    registration_lottery_year,
    gsub("[^A-Za-z0-9]", "", DOL_ETA_CASE_NUMBER),
    as.character(petition_recieve_date),
    as.character(petition_decision_date),
    round(as.numeric(petition_annual_pay_clean), 2L),
    as.character(registration_gender),
    sep = "|"
  )
]

# 8) Only keys that identify exactly one petition on each side are joined, so the
#    merge cannot duplicate or misassign a wage level.
wage_level_unique_chr <- wage_levels_raw[, .N, by = petition_key_chr][N == 1L]$petition_key_chr
h1b_unique_chr <- h1b_observed[, .N, by = petition_key_chr][N == 1L]$petition_key_chr

wage_levels_clean <- wage_levels_raw[
  petition_key_chr %in% wage_level_unique_chr,
  .(petition_key_chr, petition_h1b_type, petition_beneficiary_edu_code,
    petition_worksite_state, registration_age,
    lca_wage_level_wt = fcase(
      lca_wage_level == "I", 1L, lca_wage_level == "II", 2L,
      lca_wage_level == "III", 3L, lca_wage_level == "IV", 4L,
      lca_wage_level == "Too Low", 0L
    ),
    Old_OFLC_wage_level_wt = fcase(
      Old_OFLC_wage_level == "I", 1L, Old_OFLC_wage_level == "II", 2L,
      Old_OFLC_wage_level == "III", 3L, Old_OFLC_wage_level == "IV", 4L,
      Old_OFLC_wage_level == "Too Low", 0L
    ),
    New_OFLC_wage_level_strict_wt = fcase(
      New_OFLC_wage_level_strict == "I", 1L, New_OFLC_wage_level_strict == "II", 2L,
      New_OFLC_wage_level_strict == "III", 3L, New_OFLC_wage_level_strict == "IV", 4L,
      New_OFLC_wage_level_strict == "Too Low", 0L
    ))
]

fy_base_sample <- merge(
  h1b_observed[petition_key_chr %in% h1b_unique_chr],
  wage_levels_clean,
  by = "petition_key_chr"
)

message(
  "Matched OFLC wage levels to ",
  format(nrow(fy_base_sample), big.mark = ","), " of ",
  format(nrow(h1b_observed), big.mark = ","), " petitions (",
  sprintf("%.1f%%", 100 * nrow(fy_base_sample) / nrow(h1b_observed)), ")"
)

# 8a) Merge check. registration_age is not part of the join key, so agreement
#     between it and the age computed from the birth year in the Borjas sample
#     is independent evidence that the key pairs the same petition on both sides.
age_agreement_num <- mean(fy_base_sample$registration_age == fy_base_sample$age_int)

message(sprintf(
  "  merge check: beneficiary age agrees on %.3f%% of matched petitions",
  100 * age_agreement_num
))

if (age_agreement_num < 0.99) {
  stop(
    "The petition key is pairing different beneficiaries: age agrees on only ",
    sprintf("%.1f%%", 100 * age_agreement_num), " of matched rows.",
    call. = FALSE
  )
}

# 9) Cap-subject types only, as in the original design: B is the regular cap and
#    M the advanced-degree exemption.
fy_base_sample <- fy_base_sample[petition_h1b_type %in% c("B", "M")]

message(
  "After restricting to cap-subject types B and M: ",
  format(nrow(fy_base_sample), big.mark = ",")
)

# 10) Regional price parities, lagged one year onto the lottery year as before.
rpps <- fread(rpp_file, showProgress = FALSE)
rpps[, Year := Year + 1L]

# 11) Lifetime earnings multipliers by age and year. The 2021 file year is reused
#     for lottery year 2021 and every file year is also shifted forward one year,
#     matching the original construction.
npv_adj_factors <- fread(npv_file, showProgress = FALSE)
npv_adj_factors <- rbindlist(
  list(
    npv_adj_factors[YEAR == 2021L],
    copy(npv_adj_factors)[, YEAR := YEAR + 1L]
  )
)

fy_base_sample <- merge(
  fy_base_sample,
  rpps[, .(petition_worksite_state = state_abbr, registration_lottery_year = Year,
           deflator)],
  by.x = c("petition_worksite_state", "year_int"),
  by.y = c("petition_worksite_state", "registration_lottery_year"),
  all.x = TRUE
)

# 12) The ranking metric is built from the real wage carried by the Borjas sample
#     rather than the nominal pay used in the February version, so that the
#     ranking is consistent with the wages the regression sees.
fy_base_sample[
  , `:=`(
    wage_rpp_adj = wage_real_num * deflator,
    join_age = fifelse(age_int < 22L, 22L, fifelse(age_int > 59L, 59L, age_int))
  )
]

fy_base_sample <- merge(
  fy_base_sample,
  npv_adj_factors[, .(join_age = AGE, year_int = YEAR, mean_wage_6yr_proj,
                      exp_lifetime_mean_3pct, exp_lifetime_mean_7pct)],
  by = c("join_age", "year_int"),
  all.x = TRUE
)

fy_base_sample[
  , `:=`(
    wage_npv_mean_3pct = wage_rpp_adj * exp_lifetime_mean_3pct,
    wage_npv_mean_7pct = wage_rpp_adj * exp_lifetime_mean_7pct
  )
]

npv_missing_int <- sum(is.na(fy_base_sample$wage_npv_mean_3pct))

message(
  "Dropping ", npv_missing_int,
  " petitions with no regional price parity or lifetime multiplier ",
  "(territories and unmatched worksite states)"
)

fy_base_sample <- fy_base_sample[!is.na(wage_npv_mean_3pct)]

# 13) Split sample into graduate degree holders and non-graduate degree holders
fy_base_sample[
  , sample_flag := factor(
    fifelse(
      petition_h1b_type == "M" |
        (petition_h1b_type == "B" &
           petition_beneficiary_edu_code %in% c("G", "H", "I")),
      "reserved", "unreserved"
    ),
    levels = c("reserved", "unreserved")
  )
]

base_sample <- copy(fy_base_sample)
base_sample[, base_row_int := seq_len(.N)]

message(
  "Base sample for the simulation: ",
  format(nrow(base_sample), big.mark = ","), " petitions (",
  format(sum(base_sample$sample_flag == "reserved"), big.mark = ","),
  " reserved, ",
  format(sum(base_sample$sample_flag == "unreserved"), big.mark = ","),
  " unreserved)"
)

grad_sample <- base_sample[sample_flag == "reserved"]
non_grad_sample <- base_sample[sample_flag == "unreserved"]

# 14) Eligible registrations per lottery year, from the FOIA registration files.
total_regs_list <- rep(0L, length(simulation_years_int))
i <- 1L
for (year in simulation_years_int) {
  if (year == 2024L) {
    total_regs_list[i] <- nrow(
      rbindlist(list(
        fread(file.path(path_foia, "TRK_13139_FY2024_single_reg.csv"),
              select = "status_type", showProgress = FALSE),
        fread(file.path(path_foia, "TRK_13139_FY2024_multi_reg.csv"),
              select = "status_type", showProgress = FALSE)
      ))[status_type %in% c("SELECTED", "CREATED", "ELIGIBLE")]
    )
  } else {
    total_regs_list[i] <- nrow(
      fread(file.path(path_foia, paste0("TRK_13139_FY", year, ".csv")),
            select = "status_type", showProgress = FALSE)[
              status_type %in% c("SELECTED", "CREATED", "ELIGIBLE")
            ]
    )
  }
  i <- i + 1L
}

total_lottery_winners <- fread(
  entry_total_file, select = "registration_lottery_year", showProgress = FALSE
)[, .(entry_total = .N), by = registration_lottery_year][
  registration_lottery_year %in% simulation_years_int
][order(registration_lottery_year)]

# 15) Scale the synthetic registrant pool so that the share of registrations that
#     survive into the analysis sample matches the observed share.
approval_rate <- base_sample[
  , .(win_total = .N), by = .(registration_lottery_year = year_int)
][order(registration_lottery_year)]

approval_rate <- merge(
  approval_rate, total_lottery_winners, by = "registration_lottery_year"
)
approval_rate[, approve_r := win_total / entry_total]

synth_reg_total <- copy(approval_rate)
synth_reg_total[, total_lottery_entries := total_regs_list]
synth_reg_total[, reg_total := round(total_lottery_entries * approve_r, 0)]

grad_unreserv <- base_sample[
  sample_flag == "reserved" & petition_h1b_type != "M",
  .(grad_unres_winner_total = .N), by = .(registration_lottery_year = year_int)
]
unreserve_total <- base_sample[
  petition_h1b_type != "M",
  .(unres_winner_total = .N), by = .(registration_lottery_year = year_int)
]

synth_grad_total <- merge(grad_unreserv, unreserve_total,
                          by = "registration_lottery_year")
synth_grad_total <- merge(
  synth_grad_total,
  synth_reg_total[, .(registration_lottery_year, reg_total)],
  by = "registration_lottery_year"
)
synth_grad_total[
  , grad_total := round(grad_unres_winner_total / unres_winner_total * reg_total, 0)
]

synth_non_grad_total <- merge(
  synth_reg_total[, .(registration_lottery_year, reg_total)],
  synth_grad_total[, .(registration_lottery_year, grad_total)],
  by = "registration_lottery_year"
)
synth_non_grad_total[, non_grad_total := reg_total - grad_total]

message("")
message("=== Synthetic registrant pool ===")
for (row_index in seq_len(nrow(synth_reg_total))) {
  message(sprintf(
    "  FY%d  eligible registrations %9s  approval rate %.3f  pool %9s",
    synth_reg_total$registration_lottery_year[row_index],
    format(synth_reg_total$total_lottery_entries[row_index], big.mark = ","),
    synth_reg_total$approve_r[row_index],
    format(synth_reg_total$reg_total[row_index], big.mark = ",")
  ))
}

remove(regression_data, h1b_observed, wage_levels_raw, wage_levels_clean,
       fy_base_sample)
invisible(gc())

################################################################################
###          Benchmark: the observed FY2022-24 allocation, unsimulated       ###
################################################################################

# 16) The wage gap among the petitions that were actually approved, estimated on
#     the same specification. Every simulated regime is read against this.
observed_benchmark <- summarize_model(
  data.table(base_row_int = base_sample$base_row_int), "observed FY2022-24"
)

message("")
message(sprintf(
  "Observed FY2022-24 allocation: Borjas gap %+.4f (%.4f), tenure-adjusted %+.4f",
  observed_benchmark$coef_borjas[1], observed_benchmark$se_borjas[1],
  observed_benchmark$coef_tenure[1]
))

########################
#### Run simulation ####
########################
set.seed(42)

g_cap <- synth_grad_total[
  , setNames(as.integer(grad_total), as.character(registration_lottery_year))
]
ng_cap <- synth_non_grad_total[
  , setNames(as.integer(non_grad_total), as.character(registration_lottery_year))
]

sim_batches <- split(
  seq_len(n_sim), ceiling(seq_len(n_sim) / sim_batch_size_int)
)

sim_sum_current <- NULL
sim_sum_old_wl <- NULL
sim_sum_blind_bench <- NULL
sim_sum_wage_rank <- NULL
selection_counts_raw <- NULL
eligibility_diagnostic <- NULL

message("")
message(
  "Running ", n_sim, " replications in ", length(sim_batches), " batches of up to ",
  sim_batch_size_int
)

for (batch_index in seq_along(sim_batches)) {

  sim_ids_int <- sim_batches[[batch_index]]
  n_batch_int <- length(sim_ids_int)

  grad_draws <- grad_sample[
    , {
      yr <- as.character(first(year_int))
      cap <- g_cap[yr]
      if (is.na(cap) || cap <= 0L) {
        data.table(sel = integer(0), sim_id = integer(0))
      } else {
        idx <- sample.int(.N, cap * n_batch_int, replace = TRUE)
        data.table(sel = .I[idx],                              # rows of grad_sample
                   sim_id = rep.int(sim_ids_int, cap))
      }
    },
    by = year_int
  ]

  non_grad_draws <- non_grad_sample[
    , {
      yr <- as.character(first(year_int))
      cap <- ng_cap[yr]
      if (is.na(cap) || cap <= 0L) {
        data.table(sel = integer(0), sim_id = integer(0))
      } else {
        idx <- sample.int(.N, cap * n_batch_int, replace = TRUE)
        data.table(sel = .I[idx],
                   sim_id = rep.int(sim_ids_int, cap))
      }
    },
    by = year_int
  ]

  # 17) The pool holds selection fields and a pointer back into base_sample only.
  synth_columns <- c("base_row_int", "sample_flag", "lca_wage_level_wt",
                     "Old_OFLC_wage_level_wt", "New_OFLC_wage_level_strict_wt",
                     "wage_npv_mean_3pct")

  synth <- rbindlist(
    list(
      grad_sample[grad_draws$sel, ..synth_columns][
        , `:=`(registration_lottery_year = grad_sample$year_int[grad_draws$sel],
               sim_id = grad_draws$sim_id)],
      non_grad_sample[non_grad_draws$sel, ..synth_columns][
        , `:=`(registration_lottery_year = non_grad_sample$year_int[non_grad_draws$sel],
               sim_id = non_grad_draws$sim_id)]
    ),
    use.names = TRUE, fill = TRUE
  )

  synth[, synth_applicant_id := seq_len(.N), by = .(registration_lottery_year, sim_id)]
  setkey(synth, registration_lottery_year, sim_id, synth_applicant_id)

  remove(grad_draws, non_grad_draws)
  invisible(gc())

  # 17a) Eligible pool by regime, measured once on the first batch. A petition
  #      whose wage falls below the regime's Level I is ineligible, so the
  #      weighted lotteries draw from a smaller pool than the unweighted one.
  #      Where that pool falls below the 85,000 cap the cap cannot be filled, and
  #      the regime awards fewer visas rather than reaching further down the wage
  #      distribution.
  if (batch_index == 1L) {
    eligibility_diagnostic <- rbindlist(lapply(
      c("lca_wage_level_wt", "Old_OFLC_wage_level_wt",
        "New_OFLC_wage_level_strict_wt"),
      function(weight_col_chr) {
        synth[
          get(weight_col_chr) != 0,
          .(weight_col = weight_col_chr,
            eligible_per_replication_num = .N / n_batch_int),
          by = .(registration_lottery_year)
        ]
      }
    ))

    eligibility_diagnostic[
      , cap_fillable_flag := eligible_per_replication_num >= (cap_unres + cap_res)
    ]
  }

  ################## Apply H1-B scenarios to simulated applications ##################
  batch_current <- current_lottery(
    synth[lca_wage_level_wt != 0], cap_res, cap_unres
  )
  batch_old_wl <- proposed_rule(
    synth[Old_OFLC_wage_level_wt != 0], cap_res, cap_unres,
    weight_col = "Old_OFLC_wage_level_wt"
  )
  batch_blind_bench <- proposed_rule(
    synth[New_OFLC_wage_level_strict_wt != 0], cap_res, cap_unres,
    weight_col = "New_OFLC_wage_level_strict_wt"
  )
  batch_wage_rank <- rank_select(
    copy(synth), cap_res, cap_unres, metric = "wage_npv_mean_3pct"
  )

  # 17b) Selection counts accumulate across batches and are summed per petition
  #      at the end, so the saved counts are out of the full n_sim.
  selection_counts_raw <- rbindlist(
    list(
      selection_counts_raw,
      as.data.table(attr(batch_current, "selection_counts"))[
        , alloc_method := "current lottery"],
      as.data.table(attr(batch_old_wl, "selection_counts"))[
        , alloc_method := "proposed rule (Old_OFLC_wage_level_wt)"],
      as.data.table(attr(batch_blind_bench, "selection_counts"))[
        , alloc_method := "proposed rule (New_OFLC_wage_level_strict_wt)"],
      as.data.table(attr(batch_wage_rank, "selection_counts"))[
        , alloc_method := "NPV adjusted wage ranking, 3% discount"]
    ),
    use.names = TRUE, fill = TRUE
  )

  sim_sum_current <- rbindlist(list(sim_sum_current, batch_current), fill = TRUE)
  sim_sum_old_wl <- rbindlist(list(sim_sum_old_wl, batch_old_wl), fill = TRUE)
  sim_sum_blind_bench <- rbindlist(
    list(sim_sum_blind_bench, batch_blind_bench), fill = TRUE
  )
  sim_sum_wage_rank <- rbindlist(
    list(sim_sum_wage_rank, batch_wage_rank), fill = TRUE
  )

  remove(synth, batch_current, batch_old_wl, batch_blind_bench, batch_wage_rank)
  invisible(gc())

  message(
    "  batch ", batch_index, " of ", length(sim_batches), " done (replications ",
    min(sim_ids_int), "-", max(sim_ids_int), ")"
  )
}

remove(grad_sample, non_grad_sample)
invisible(gc())

message("")
message("=== Eligible pool per replication, against the 85,000 cap ===")
for (row_index in seq_len(nrow(eligibility_diagnostic))) {
  row_current <- eligibility_diagnostic[row_index]
  message(sprintf(
    "  FY%d  %-30s %10s  %s",
    row_current$registration_lottery_year, row_current$weight_col,
    format(round(row_current$eligible_per_replication_num), big.mark = ","),
    if (row_current$cap_fillable_flag) "cap fillable" else "BELOW THE CAP"
  ))
}

readr::write_csv(
  eligibility_diagnostic[order(weight_col, registration_lottery_year)],
  file.path(path_output, "FY2022-24 Allocation Regime Eligible Pool.csv")
)

summarise_sim_results <- function(sim_sum, method) {
  tibble::as_tibble(sim_sum) |>
    dplyr::mutate(
      sum_weight_borjas = dplyr::if_else(is.na(sum_weight_borjas), 0, sum_weight_borjas),
      sum_weight_tenure = dplyr::if_else(is.na(sum_weight_tenure), 0, sum_weight_tenure)
    ) |>
    dplyr::group_by(age) |>
    dplyr::summarise(
      alloc_method = method,
      coef_borjas = weighted.mean(coef_borjas, sum_weight_borjas),
      se_borjas = weighted.mean(se_borjas, sum_weight_borjas),
      mean_resid_borjas = weighted.mean(mean_resid_borjas, sum_weight_borjas),
      sum_weight_borjas = round(mean(sum_weight_borjas), 0),
      coef_tenure = weighted.mean(coef_tenure, sum_weight_tenure),
      se_tenure = weighted.mean(se_tenure, sum_weight_tenure),
      mean_resid_tenure = weighted.mean(mean_resid_tenure, sum_weight_tenure),
      sum_weight_tenure = round(mean(sum_weight_tenure), 0),
      .groups = "drop"
    )
}

invisible(gc())

summaries_final <- dplyr::bind_rows(
  summarise_sim_results(observed_benchmark, "0. observed FY2022-24 allocation"),
  summarise_sim_results(sim_sum_current, "1. old unweighted lottery"),
  summarise_sim_results(sim_sum_old_wl, "2a. weighted lottery, old wage levels"),
  summarise_sim_results(sim_sum_blind_bench, "2b. weighted lottery, new wage levels"),
  summarise_sim_results(sim_sum_wage_rank, "3. EIG wage ranking, RPP and NPV adjusted")
)

readr::write_csv(
  summaries_final,
  file.path(path_output, "FY2022-24 Borjas FE Model Simulation Summaries.csv")
)

# 18) Replication-level coefficients, so the spread across the 20 draws is visible
#     rather than only the mean.
regime_labels_chr <- c(
  "observed FY2022-24" = "0. observed FY2022-24 allocation",
  "current lottery" = "1. old unweighted lottery",
  "proposed rule (Old_OFLC_wage_level_wt)" = "2a. weighted lottery, old wage levels",
  "proposed rule (New_OFLC_wage_level_strict_wt)" = "2b. weighted lottery, new wage levels",
  "NPV adjusted wage ranking, 3% discount" = "3. EIG wage ranking, RPP and NPV adjusted"
)

simulation_coefficients <- dplyr::bind_rows(
  observed_benchmark |> dplyr::mutate(sim_id = 0L),
  sim_sum_current, sim_sum_old_wl, sim_sum_blind_bench, sim_sum_wage_rank
) |>
  tibble::as_tibble() |>
  dplyr::mutate(alloc_method = unname(regime_labels_chr[alloc_method])) |>
  dplyr::distinct(
    alloc_method, sim_id, n_winners_int, coef_borjas, se_borjas, coef_tenure,
    se_tenure
  )

summaries_coefs <- simulation_coefficients |>
  dplyr::group_by(alloc_method) |>
  dplyr::summarise(
    n_replications = dplyr::n(),
    `Mean visas awarded` = mean(n_winners_int),
    `Estimated Wage Difference, Borjas Model` = mean(coef_borjas) * 100,
    `SD across replications, Borjas Model` = stats::sd(coef_borjas) * 100,
    `Estimated Wage Difference, Tenure Model` = mean(coef_tenure) * 100,
    `SD across replications, Tenure Model` = stats::sd(coef_tenure) * 100,
    .groups = "drop"
  ) |>
  dplyr::arrange(alloc_method)

readr::write_csv(
  summaries_coefs,
  file.path(path_output, "FY2022-24 Borjas FE Model Simulated Coefficients.csv")
)

summaries_age_resids <- summaries_final |>
  dplyr::select(age, alloc_method, mean_resid_borjas) |>
  tidyr::pivot_wider(names_from = alloc_method, values_from = mean_resid_borjas)

readr::write_csv(
  summaries_age_resids,
  file.path(path_output, "FY2022-24 Borjas FE Model Simulated Residuals by Age.csv")
)

# 18a) Selection counts: how many of the n_sim replications each petition won
#      under each regime. This is the sufficient statistic for the composition of
#      the winners, and 07_fee_under_reformed_rules.R uses it to price the fee against each regime's
#      payroll-savings distribution without repeating the draw.
selection_counts <- selection_counts_raw[
  , .(selection_count_int = sum(selection_count_int)),
  by = .(base_row_int, alloc_method)
] |>
  as.data.frame() |>
  dplyr::mutate(alloc_method = unname(regime_labels_chr[alloc_method])) |>
  dplyr::left_join(
    as.data.frame(base_sample[
      , .(base_row_int, petition_key_chr, year_int, lca_wage_level_wt,
          Old_OFLC_wage_level_wt, New_OFLC_wage_level_strict_wt)
    ]),
    by = "base_row_int"
  ) |>
  dplyr::mutate(n_replications_int = n_sim)

message("")
message(
  "Selection counts recorded for ",
  format(dplyr::n_distinct(selection_counts$base_row_int), big.mark = ","),
  " distinct petitions across ", dplyr::n_distinct(selection_counts$alloc_method),
  " regimes"
)

# 18b) The eligibility margin needs every petition, not only the ones that won at
#      least once, so the wage-level flags for the whole base sample are saved
#      separately. Without this the eligible share reads as 100 percent under
#      every regime, because a petition that is ineligible can never be selected.
allocation_base_sample <- as.data.frame(base_sample[
  , .(petition_key_chr, year_int, sample_flag, lca_wage_level_wt,
      Old_OFLC_wage_level_wt, New_OFLC_wage_level_strict_wt)
])

saveRDS(
  allocation_base_sample,
  file.path(path_processed, "borjas_allocation_base_sample.rds")
)
arrow::write_parquet(
  allocation_base_sample,
  file.path(path_processed, "borjas_allocation_base_sample.parquet"),
  compression = "snappy"
)

saveRDS(
  selection_counts,
  file.path(path_processed, "borjas_allocation_selection_counts.rds")
)
arrow::write_parquet(
  selection_counts,
  file.path(path_processed, "borjas_allocation_selection_counts.parquet"),
  compression = "snappy"
)

saveRDS(
  simulation_coefficients,
  file.path(path_processed, "borjas_allocation_simulation_coefficients.rds")
)
arrow::write_parquet(
  simulation_coefficients,
  file.path(path_processed, "borjas_allocation_simulation_coefficients.parquet"),
  compression = "snappy"
)

################################################################################
###                              19. Report                                  ###
################################################################################

message("")
message("========== Estimated H-1B wage gap by allocation regime ==========")
message("  (negative means H-1B workers earn less than comparable natives)")
message("")
message(sprintf(
  "  %-44s %10s %10s %8s %12s", "Allocation regime", "Borjas", "Tenure", "SD",
  "Visas"
))
for (row_index in seq_len(nrow(summaries_coefs))) {
  row_current <- summaries_coefs[row_index, ]
  message(sprintf(
    "  %-44s %9.2f%% %9.2f%% %7.2f %12s",
    row_current$alloc_method,
    row_current$`Estimated Wage Difference, Borjas Model`,
    row_current$`Estimated Wage Difference, Tenure Model`,
    row_current$`SD across replications, Borjas Model`,
    format(round(row_current$`Mean visas awarded`), big.mark = ",")
  ))
}

message("")
message("Saved simulation tables to output/tables/")
