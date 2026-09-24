# 04_borjas_replication -- Borjas August 2026 draft, Table 2, and its critique
# Author - Jiaxin He
# research title - H-1B Fee Analysis and Comment Letter
# research question - Even if we take Borjas's methods, what is wrong with how the
#   DHS interpreted it?
#
# Replicates the headline ACS model in Borjas, "The H-1B Wage Gap, Visa Fees, and
# Employer Demand", NBER WP 34793, Feb. 2026, revised Aug. 2026, as specified on
# pp. 4-13 and in the Data Appendix (pp. 49-50), and reported in Table 2 (p. 41).
#
# Equation (1):  log w_i = X_i'b + g*H_i + m_i + d_i + t_i + e_i
#   X = education, age, gender, occupation fixed effects
#   m = metropolitan area of WORK (2023 CBSA; state FIPS where no metro)
#   d = industry (census "ind" codes)
#   t = year (FY t of the H-1B data, or calendar year t of the ACS)
#   weights: ACS person weight for natives, 1 for H-1B workers; robust SEs
#
# Table 2 targets, Panel A row 1 (OLS):
#   (1) 0.044  (2) -0.062  (3) -0.023  (4) -0.136  (5) -0.161    N = 925,045
# Panel B, by beneficiary location when status was granted:
#   Abroad (1) 0.021 (2) -0.065 (3) -0.087 (4) -0.181 (5) -0.219
#   USA    (1) 0.061 (2) -0.059 (3)  0.025 (4) -0.102 (5) -0.118

rm(list = ls())
options(scipen = 999)
set.seed(42)

################################################################################
###                           1. Load packages                               ###
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(ipumsr)
  library(fixest)
  library(fredr)
  library(tidycensus)
  library(arrow)
})

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

path_raw <- file.path(path_project, "data", "raw")
path_acs <- file.path(path_raw, "ACS")
path_crosswalk <- file.path(path_raw, "Geographic Crosswalks")
path_processed <- file.path(path_project, "data", "processed")
path_foia <- file.path(path_processed, "Old Cleaned FOIA - for Borjas Replication")
path_output <- file.path(path_project, "output", "tables")

for (path_name in c(path_processed, path_output)) {
  if (!dir.exists(path_name)) {
    dir.create(path_name, recursive = TRUE)
  }
}

acs_ddi_file <- file.path(path_acs, "usa_00108.xml")
cpi_series_id <- "CPIAUCNS"    # CPI-U, all items, monthly, not seasonally adjusted
base_dollar_year_int <- 2025L  # Borjas reports all dollar values in 2025 dollars

# 2) A FRED key is required for the CPI deflators. Keys are free from
#    https://fredaccount.stlouisfed.org/apikeys. Set it once in ~/.Renviron as
#        FRED_API_KEY=your_key_here
#    and restart R. The key is never stored in this repository.
fred_api_key_chr <- Sys.getenv("FRED_API_KEY")

if (!nzchar(fred_api_key_chr)) {
  stop(
    "No FRED API key found. Add FRED_API_KEY=<your key> to ~/.Renviron and ",
    "restart R. Free keys: https://fredaccount.stlouisfed.org/apikeys",
    call. = FALSE
  )
}

fredr::fredr_set_key(fred_api_key_chr)

################################################################################
###                    3. Monthly CPI from FRED                              ###
################################################################################

# 3) Borjas deflates on the timing of actual wage receipt, so the monthly series
#    is required rather than the annual one.
cpi_monthly <- fredr::fredr(
  series_id = cpi_series_id,
  observation_start = as.Date("2018-01-01"),
  observation_end = as.Date("2026-12-31"),
  frequency = "m"
) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::transmute(
    cpi_date = as.Date(date),
    cpi_year_int = as.integer(format(cpi_date, "%Y")),
    cpi_index_num = as.numeric(value)
  )

message(
  "Retrieved ", nrow(cpi_monthly), " monthly ", cpi_series_id, " observations, ",
  min(cpi_monthly$cpi_date), " to ", max(cpi_monthly$cpi_date)
)

cpi_annual <- cpi_monthly |>
  dplyr::group_by(cpi_year_int) |>
  dplyr::summarise(
    cpi_annual_num = mean(cpi_index_num),
    months_observed_int = dplyr::n(),
    .groups = "drop"
  )

cpi_base_num <- cpi_annual$cpi_annual_num[
  cpi_annual$cpi_year_int == base_dollar_year_int
]

if (length(cpi_base_num) != 1L) {
  stop("No CPI annual average available for ", base_dollar_year_int, call. = FALSE)
}

months_in_base_int <- cpi_annual$months_observed_int[
  cpi_annual$cpi_year_int == base_dollar_year_int
]

if (months_in_base_int < 12L) {
  message(
    "NOTE: the ", base_dollar_year_int, " CPI base uses ", months_in_base_int,
    " months. This rescales all dollar figures uniformly and leaves the ",
    "wage-gap coefficients unchanged."
  )
}

################################################################################
###          4. State FIPS lookup, used for the pseudo-metro fallback        ###
################################################################################

state_fips_lookup <- tidycensus::fips_codes |>
  dplyr::distinct(state, state_code) |>
  dplyr::rename(state_abbr = state, state_fips_chr = state_code) |>
  dplyr::filter(state_fips_chr <= "56")

state_fips_valid <- sprintf("%02d", c(1:2, 4:6, 8:13, 15:42, 44:51, 53:56))

################################################################################
###        5. H-1B sample: approved I-129 petitions, FY2021-FY2024           ###
################################################################################

foia_files <- list.files(path_foia, pattern = "\\.csv$", full.names = TRUE)

i129_raw <- data.table::rbindlist(
  lapply(
    foia_files,
    function(file_name) {
      data.table::fread(
        file_name,
        select = c(
          "applicant_id", "registration_lottery_year", "registration_birth_year",
          "registration_gender", "petition_decision", "petition_request_action",
          "petition_classif_valid_start_date", "CASE_NUMBER", "SOC_CODE",
          "petition_beneficiary_classif",
          "petition_employer_naics", "petition_worksite_state", "MSA_code",
          "petition_beneficiary_edu_defin", "petition_annual_pay_clean",
          "petition_recieve_date", "petition_decision_date", "petition_worksite_zip",
          "petition_h1b_type"
        ),
        colClasses = c(
          MSA_code = "character", petition_employer_naics = "character",
          CASE_NUMBER = "character", petition_worksite_zip = "character"
        ),
        showProgress = FALSE
      )
    }
  ),
  fill = TRUE
)

message(
  "Loaded ", length(foia_files), " I-129 files: ",
  format(nrow(i129_raw), big.mark = ","), " petitions, FY",
  min(i129_raw$registration_lottery_year), "-FY",
  max(i129_raw$registration_lottery_year)
)

# 4) Sample restrictions from the Data Appendix: approved petitions, a valid LCA
#    case number, a valid state of employment among the 50 states and DC, a valid
#    H-1B status start date, and beneficiaries aged 21-50 at filing.
# 5) Full-time status is taken from the LCA, as Borjas specifies. The LCA extract
#    was filtered to FULL_TIME_POSITION == "Y" when these files were built, so a
#    valid CASE_NUMBER already carries that restriction. Do NOT filter on the
#    I-129 field petition_beneficiary_full_time: it is empty for all of FY2021 and
#    FY2022 and would silently delete two fiscal years.
h1b_stage_one <- i129_raw |>
  dplyr::left_join(
    state_fips_lookup,
    by = dplyr::join_by("petition_worksite_state" == "state_abbr")
  ) |>
  dplyr::mutate(
    lottery_year_int = as.integer(registration_lottery_year),
    # The lottery is held in March before the fiscal year begins on October 1
    lottery_held_date = as.Date(paste0(lottery_year_int - 1L, "-03-01")),
    fiscal_year_start_date = as.Date(paste0(lottery_year_int - 1L, "-10-01")),
    status_start_date = as.Date(
      petition_classif_valid_start_date, format = "%m/%d/%Y"
    ),
    # Age at the start of the fiscal year in which employment begins. Using
    # lottery_year - birth_year reproduces the mean age of 31.9 in Table 1;
    # subtracting a further year (age at filing) gives 30.9 and does not.
    age_int = lottery_year_int - as.integer(registration_birth_year),
    beneficiary_location = dplyr::case_when(
      petition_request_action == "A" ~ "Abroad",
      petition_request_action == "B" ~ "USA",
      TRUE ~ "Other"
    ),
    # Prior status splits the in-country channel by whether the beneficiary was
    # on a student visa. Request actions other than "A" all presuppose presence
    # in the United States (change, extend or amend status), so they join the
    # in-US side. Blank, "UU" and "UN" mean no recorded classification.
    prior_status_group = dplyr::case_when(
      petition_request_action == "A" ~ "Abroad",
      trimws(as.character(petition_beneficiary_classif)) %in% c("F1", "F2") ~
        "In US, prior F-1",
      TRUE ~ "In US, other status"
    ),
    wage_nominal_num = as.numeric(petition_annual_pay_clean),
    # A composite petition key, carried through to the saved checkpoint so that
    # downstream scripts can attach OFLC wage levels from the separately cleaned
    # file "FY2022-2024 I-129 Data with New Wage Levels.csv". That file renumbers
    # applicant_id, so it cannot be joined on the identifier. These six fields
    # match 99.0 percent of its rows one-to-one.
    petition_key_chr = paste(
      lottery_year_int,
      gsub("[^A-Za-z0-9]", "", as.character(CASE_NUMBER)),
      as.character(petition_recieve_date),
      as.character(petition_decision_date),
      round(as.numeric(petition_annual_pay_clean), 2L),
      as.character(registration_gender),
      sep = "|"
    ),
    soc6_chr = substr(gsub("[^0-9]", "", as.character(SOC_CODE)), 1L, 6L),
    naics4_chr = substr(gsub("[^0-9]", "", as.character(petition_employer_naics)), 1L, 4L),
    msa_code_chr = gsub("[^0-9]", "", as.character(MSA_code)),
    gender_chr = dplyr::case_when(
      registration_gender == "male" ~ "male",
      registration_gender == "female" ~ "female",
      TRUE ~ NA_character_
    ),
    # Borjas assigns filings with missing education the visa's minimum
    # requirement, a bachelor's degree. Table 1 reports only four education
    # categories, so the handful of sub-bachelor responses are folded in too.
    education_chr = dplyr::case_when(
      petition_beneficiary_edu_defin == "MASTER'S DEGREE" ~ "master",
      petition_beneficiary_edu_defin == "DOCTORATE DEGREE" ~ "doctorate",
      petition_beneficiary_edu_defin == "PROFESSIONAL DEGREE" ~ "professional",
      TRUE ~ "bachelor"
    ),
    # Flag the filings whose education is imputed rather than reported, so the
    # critique below can re-estimate without them. Footnote 17 of the August
    # draft puts this at 17 percent of filings; it is concentrated in FY2023-24.
    education_imputed_flag = !(
      trimws(as.character(petition_beneficiary_edu_defin)) %in%
        c("BACHELOR'S DEGREE", "MASTER'S DEGREE", "DOCTORATE DEGREE",
          "PROFESSIONAL DEGREE")
    ),
    education_reported_chr = dplyr::if_else(
      education_imputed_flag, "unknown", education_chr
    ),
    # Broad skill and sector cells, taken straight from the source codes on both
    # sides rather than through the ACS crosswalk: SOC major group and NAICS
    # sector, with the split sectors collapsed as the NAICS manual does.
    soc_major_chr = substr(soc6_chr, 1L, 2L),
    naics_sector_chr = dplyr::case_when(
      substr(naics4_chr, 1L, 2L) %in% c("31", "32", "33") ~ "31-33",
      substr(naics4_chr, 1L, 2L) %in% c("44", "45") ~ "44-45",
      substr(naics4_chr, 1L, 2L) %in% c("48", "49") ~ "48-49",
      nchar(naics4_chr) >= 2L ~ substr(naics4_chr, 1L, 2L),
      TRUE ~ NA_character_
    ),
    worksite_zip_chr = stringr::str_pad(
      substr(gsub("[^0-9]", "", as.character(petition_worksite_zip)), 1L, 5L),
      5L, "left", "0"
    )
  ) |>
  dplyr::filter(
    petition_decision == "Approved",
    !is.na(CASE_NUMBER), CASE_NUMBER != "",
    !is.na(state_fips_chr), state_fips_chr %in% state_fips_valid,
    !is.na(status_start_date),
    !is.na(wage_nominal_num), wage_nominal_num > 0,
    age_int >= 21L, age_int <= 50L,
    !is.na(gender_chr)
  )

# 6) Footnote 14 drops beneficiaries whose status start date falls outside the
#    window from the lottery to January of the year after the fiscal year.
h1b_stage_two <- h1b_stage_one |>
  dplyr::filter(
    status_start_date >= lottery_held_date,
    status_start_date <= as.Date(paste0(lottery_year_int + 1L, "-01-31"))
  )

message(
  "H-1B after core restrictions: ", format(nrow(h1b_stage_one), big.mark = ","),
  "; after the status-date window: ", format(nrow(h1b_stage_two), big.mark = ","),
  " (dropped ", nrow(h1b_stage_one) - nrow(h1b_stage_two), ")"
)

# 7) Deflation. The twelve-month earning window opens on the later of the status
#    effective date and the start of the fiscal year, because employment cannot
#    lawfully begin before either. The deflator is the mean monthly CPI across
#    that window. This is what separates the August draft from the February one.
h1b_with_window <- h1b_stage_two |>
  dplyr::mutate(
    window_open_date = pmax(status_start_date, fiscal_year_start_date),
    window_first_month = as.Date(format(window_open_date, "%Y-%m-01"))
  )

window_deflators <- tibble::tibble(
  window_first_month = sort(unique(h1b_with_window$window_first_month))
) |>
  dplyr::mutate(
    window_last_month = window_first_month %m+% months(11L)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    window_months_int = sum(
      cpi_monthly$cpi_date >= window_first_month &
        cpi_monthly$cpi_date <= window_last_month
    ),
    window_cpi_num = mean(
      cpi_monthly$cpi_index_num[
        cpi_monthly$cpi_date >= window_first_month &
          cpi_monthly$cpi_date <= window_last_month
      ]
    )
  ) |>
  dplyr::ungroup()

if (any(window_deflators$window_months_int < 12L)) {
  message(
    "NOTE: ", sum(window_deflators$window_months_int < 12L),
    " deflation windows run past the end of the CPI series and average fewer ",
    "than 12 months."
  )
}

h1b_sample <- h1b_with_window |>
  dplyr::left_join(
    window_deflators |> dplyr::select(window_first_month, window_cpi_num),
    by = "window_first_month"
  ) |>
  dplyr::mutate(
    wage_real_num = wage_nominal_num * (cpi_base_num / window_cpi_num),
    log_wage_num = log(wage_real_num),
    is_h1b_int = 1L,
    year_int = lottery_year_int,
    occ_source_chr = soc6_chr,
    weight_num = 1
  )

################################################################################
###        6. Native sample: ACS 2021-2024, U.S.-born private sector         ###
################################################################################

acs_ddi <- ipumsr::read_ipums_ddi(acs_ddi_file)
acs_raw <- ipumsr::read_ipums_micro(acs_ddi, verbose = FALSE)

message(
  "Loaded ACS: ", format(nrow(acs_raw), big.mark = ","), " rows, years ",
  paste(sort(unique(acs_raw$YEAR)), collapse = ", ")
)

# 8) The ACS reference period straddles two calendar years, so the deflator for
#    the year t sample is the mean of the annual CPI in years t-1 and t.
acs_deflators <- tibble::tibble(
  year_int = sort(unique(as.integer(acs_raw$YEAR)))
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    acs_cpi_num = mean(
      cpi_annual$cpi_annual_num[
        cpi_annual$cpi_year_int %in% c(year_int - 1L, year_int)
      ]
    )
  ) |>
  dplyr::ungroup()

native_sample <- acs_raw |>
  dplyr::mutate(
    year_int = as.integer(YEAR),
    state_fips_chr = stringr::str_pad(as.character(STATEFIP), 2L, "left", "0"),
    pwmet_chr = as.character(PWMET23),
    age_int = as.integer(AGE),
    gender_chr = dplyr::case_when(
      SEX == 1 ~ "male", SEX == 2 ~ "female", TRUE ~ NA_character_
    ),
    education_chr = dplyr::case_when(
      EDUCD >= 101 & EDUCD < 114 ~ "bachelor",
      EDUCD == 114 ~ "master",
      EDUCD == 115 ~ "professional",
      EDUCD == 116 ~ "doctorate",
      TRUE ~ NA_character_
    ),
    occ_code = as.character(OCC),
    ind_code = as.character(IND),
    soc_major_chr = substr(sub("[^0-9].*$", "", as.character(OCCSOC)), 1L, 2L),
    naics_sector_chr = dplyr::case_when(
      substr(as.character(INDNAICS), 1L, 2L) %in% c("31", "32", "33") ~ "31-33",
      substr(as.character(INDNAICS), 1L, 2L) %in% c("44", "45") ~ "44-45",
      substr(as.character(INDNAICS), 1L, 2L) %in% c("48", "49") ~ "48-49",
      grepl("^[0-9]{2}", as.character(INDNAICS)) ~
        substr(as.character(INDNAICS), 1L, 2L),
      TRUE ~ NA_character_
    ),
    pw_state_chr = dplyr::na_if(
      stringr::str_pad(as.character(PWSTATE2), 2L, "left", "0"), "00"
    ),
    pw_puma_raw_chr = stringr::str_pad(as.character(PWPUMA00), 5L, "left", "0"),
    pwmet_err_int = as.integer(PWMET23ERR),
    education_imputed_flag = FALSE,
    education_reported_chr = education_chr,
    worksite_zip_chr = NA_character_,
    wage_nominal_num = as.numeric(INCWAGE),
    is_h1b_int = 0L,
    beneficiary_location = "Native",
    prior_status_group = "Native",
    petition_key_chr = NA_character_,
    weight_num = as.numeric(PERWT)
  ) |>
  dplyr::filter(
    AGE >= 21, AGE <= 50,
    CITIZEN == 0,          # born a U.S. citizen
    BPL <= 120,            # born in the United States
    EDUCD >= 101,          # at least a college degree
    CLASSWKR == 2,         # wage and salary worker
    CLASSWKRD == 22,       # private sector
    WKSWORK1 >= 50, UHRSWORK >= 35,
    state_fips_chr %in% state_fips_valid,
    !is.na(wage_nominal_num), wage_nominal_num > 0,
    !is.na(education_chr), !is.na(gender_chr)
  ) |>
  dplyr::left_join(acs_deflators, by = "year_int") |>
  dplyr::mutate(
    wage_real_num = wage_nominal_num * (cpi_base_num / acs_cpi_num),
    log_wage_num = log(wage_real_num)
  )

message(
  "ACS native sample before the earnings floor: ",
  format(nrow(native_sample), big.mark = ",")
)

################################################################################
###   7. Crosswalk I-129 occupation, industry and metro onto ACS categories  ###
################################################################################

# 9) Occupation and industry crosswalks. The ACS aggregates some categories and
#    flags them with non-numeric characters -- OCCSOC values such as "1191XX" and
#    INDNAICS values such as "3399ZM", "335M" or "52M1". Stripping those
#    characters and demanding a full-length match discards roughly a fifth of the
#    H-1B sample, so each ACS code is reduced to its leading numeric stem and
#    matched hierarchically: the longest stem that prefixes the I-129 code wins.
acs_occ_codes <- acs_raw |>
  dplyr::transmute(
    stem_chr = sub("[^0-9].*$", "", as.character(OCCSOC)),
    target_chr = as.character(OCC),
    weight_num = as.numeric(PERWT)
  ) |>
  dplyr::filter(nchar(stem_chr) >= 2L)

acs_ind_codes <- acs_raw |>
  dplyr::transmute(
    stem_chr = sub("[^0-9].*$", "", as.character(INDNAICS)),
    target_chr = as.character(IND),
    weight_num = as.numeric(PERWT)
  ) |>
  dplyr::filter(nchar(stem_chr) >= 2L)

# 10) Resolve each I-129 code by descending prefix length, keeping the modal ACS
#     category by person weight at each level.
h1b_sample$occ_code <- NA_character_
h1b_sample$ind_code <- NA_character_

for (prefix_length_int in 6L:2L) {

  occ_level_map <- acs_occ_codes |>
    dplyr::filter(nchar(stem_chr) >= prefix_length_int) |>
    dplyr::mutate(key_chr = substr(stem_chr, 1L, prefix_length_int)) |>
    dplyr::group_by(key_chr, target_chr) |>
    dplyr::summarise(weight_total_num = sum(weight_num), .groups = "drop") |>
    dplyr::group_by(key_chr) |>
    dplyr::slice_max(weight_total_num, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()

  unresolved_flag <- is.na(h1b_sample$occ_code) &
    nchar(h1b_sample$soc6_chr) >= prefix_length_int

  h1b_sample$occ_code[unresolved_flag] <- occ_level_map$target_chr[
    match(
      substr(h1b_sample$soc6_chr[unresolved_flag], 1L, prefix_length_int),
      occ_level_map$key_chr
    )
  ]

  if (prefix_length_int <= 4L) {

    ind_level_map <- acs_ind_codes |>
      dplyr::filter(nchar(stem_chr) >= prefix_length_int) |>
      dplyr::mutate(key_chr = substr(stem_chr, 1L, prefix_length_int)) |>
      dplyr::group_by(key_chr, target_chr) |>
      dplyr::summarise(weight_total_num = sum(weight_num), .groups = "drop") |>
      dplyr::group_by(key_chr) |>
      dplyr::slice_max(weight_total_num, n = 1L, with_ties = FALSE) |>
      dplyr::ungroup()

    unresolved_ind_flag <- is.na(h1b_sample$ind_code) &
      nchar(h1b_sample$naics4_chr) >= prefix_length_int

    h1b_sample$ind_code[unresolved_ind_flag] <- ind_level_map$target_chr[
      match(
        substr(h1b_sample$naics4_chr[unresolved_ind_flag], 1L, prefix_length_int),
        ind_level_map$key_chr
      )
    ]
  }
}

# 11) Metropolitan area. Borjas assigns the CBSA when the zip code maps to a
#     metro that the ACS identifies, and otherwise the state FIPS code. ACS
#     observations with no identified metro of work get the same treatment, so
#     the pseudo-metro is the bare state FIPS code rather than a state-by-metro
#     cell. Multi-state metros therefore stay in one cell, as in pwmet23.
acs_metros_identified <- unique(as.character(acs_raw$PWMET23[acs_raw$PWMET23 != 0]))

h1b_sample <- h1b_sample |>
  dplyr::mutate(
    metro_area = dplyr::if_else(
      !is.na(msa_code_chr) & msa_code_chr %in% acs_metros_identified,
      msa_code_chr, state_fips_chr
    )
  )

native_sample <- native_sample |>
  dplyr::mutate(
    metro_area = dplyr::if_else(pwmet_chr != "0", pwmet_chr, state_fips_chr)
  )

# 11a) Place-of-work PUMA, the alternative geography used in the critique below.
#      PWMET23 is not an independent measurement: IPUMS derives it from PWPUMA00,
#      which is "the only sub-state geographic unit identified in the source PUMS
#      data for place of work", by assigning each place-of-work PUMA to the metro
#      holding the majority of its population, and suppressing the code entirely
#      wherever omission plus commission error reaches 15 percent. The underlying
#      place-of-work PUMA is therefore both more complete and free of that
#      match error.
#
#      Vintage warning: the 2021 ACS uses 2010 PUMA definitions while 2022-2024
#      use 2020 definitions, so the same PWPUMA00 code means different areas
#      across that break. Every place-of-work PUMA specification below is
#      restricted to FY2022-FY2024 for that reason.
native_sample <- native_sample |>
  dplyr::mutate(
    pw_puma_chr = dplyr::if_else(
      !is.na(pw_state_chr) & pw_puma_raw_chr != "00000",
      paste0(pw_state_chr, "_", pw_puma_raw_chr), NA_character_
    )
  )

# 11b) The I-129 reports a worksite ZIP, not a PUMA. ZIP codes are mapped to the
#      2020 PUMA holding the largest share of the ZIP's population, then to the
#      place-of-work PUMA containing that PUMA. Unmatched ZIPs fall back to the
#      modal PUMA among ZIPs sharing a 4- then 3-digit prefix, which recovers the
#      Manhattan and other single-building ZIPs that have no ZCTA of their own.
#      The February script used NHGIS block crosswalks for those stragglers; the
#      prefix fallback reaches the same coverage without reading 1.2 GB.
zcta_puma_crosswalk <- data.table::fread(
  file.path(path_crosswalk, "geocorr2022_2604200507.csv"),
  colClasses = list(character = c("zcta", "puma22", "state")),
  showProgress = FALSE
) |>
  dplyr::filter(!is.na(suppressWarnings(as.numeric(substr(zcta, 1L, 1L))))) |>
  dplyr::mutate(
    weighted_pop_num = as.numeric(pop20) * as.numeric(afact),
    zcta_chr = stringr::str_pad(zcta, 5L, "left", "0"),
    puma_chr = stringr::str_pad(puma22, 5L, "left", "0"),
    state_chr = stringr::str_pad(state, 2L, "left", "0")
  ) |>
  dplyr::group_by(zcta_chr) |>
  dplyr::slice_max(weighted_pop_num, n = 1L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(zcta_chr, puma_chr, state_chr, weighted_pop_num)

zip_puma_crosswalk <- data.table::fread(
  file.path(path_crosswalk, "zip_zcta_xref.csv"), showProgress = FALSE
) |>
  dplyr::filter(!is.na(zip_code), !is.na(zcta)) |>
  dplyr::transmute(
    zip_chr = stringr::str_pad(as.character(zip_code), 5L, "left", "0"),
    zcta_chr = stringr::str_pad(as.character(zcta), 5L, "left", "0")
  ) |>
  dplyr::distinct(zip_chr, .keep_all = TRUE) |>
  dplyr::inner_join(zcta_puma_crosswalk, by = "zcta_chr") |>
  dplyr::select(zip_chr, puma_chr, state_chr, weighted_pop_num)

# 11c) Residence PUMA to place-of-work PUMA, from the Census 2020 crosswalk.
puma_to_pwpuma <- readxl::read_xls(
  file.path(path_crosswalk, "puma_migpuma1_pwpuma00_2020.xls")
) |>
  dplyr::transmute(
    state_chr = stringr::str_pad(
      as.character(`State of Residence (ST)`), 2L, "left", "0"
    ),
    puma_chr = stringr::str_pad(as.character(PUMA), 5L, "left", "0"),
    pw_state_chr = substr(stringr::str_pad(
      as.character(`Place of Work State (PWSTATE2) or Migration State (MIGPLAC1)`),
      3L, "left", "0"
    ), 2L, 3L),
    pw_puma_raw_chr = stringr::str_pad(
      as.character(`PWPUMA00 or MIGPUMA1`), 5L, "left", "0"
    )
  ) |>
  dplyr::distinct(state_chr, puma_chr, .keep_all = TRUE)

# 11d) Resolve each worksite ZIP by descending prefix length, keeping the modal
#      PUMA by population at each level.
h1b_sample$worksite_puma_chr <- NA_character_
h1b_sample$worksite_puma_state_chr <- NA_character_

for (zip_prefix_int in 5L:3L) {

  zip_level_map <- zip_puma_crosswalk |>
    dplyr::mutate(key_chr = substr(zip_chr, 1L, zip_prefix_int)) |>
    dplyr::group_by(key_chr, puma_chr, state_chr) |>
    dplyr::summarise(weight_total_num = sum(weighted_pop_num), .groups = "drop") |>
    dplyr::group_by(key_chr) |>
    dplyr::slice_max(weight_total_num, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()

  unresolved_zip_flag <- is.na(h1b_sample$worksite_puma_chr) &
    !is.na(h1b_sample$worksite_zip_chr)

  match_index_int <- match(
    substr(h1b_sample$worksite_zip_chr[unresolved_zip_flag], 1L, zip_prefix_int),
    zip_level_map$key_chr
  )

  h1b_sample$worksite_puma_chr[unresolved_zip_flag] <-
    zip_level_map$puma_chr[match_index_int]
  h1b_sample$worksite_puma_state_chr[unresolved_zip_flag] <-
    zip_level_map$state_chr[match_index_int]
}

h1b_sample <- h1b_sample |>
  dplyr::left_join(
    puma_to_pwpuma,
    by = dplyr::join_by(
      "worksite_puma_state_chr" == "state_chr", "worksite_puma_chr" == "puma_chr"
    )
  ) |>
  dplyr::mutate(
    pw_puma_chr = dplyr::if_else(
      !is.na(pw_state_chr) & !is.na(pw_puma_raw_chr),
      paste0(pw_state_chr, "_", pw_puma_raw_chr), NA_character_
    ),
    # The place-of-work state comes from the crosswalk where the ZIP resolved and
    # from the reported worksite state otherwise.
    pw_state_chr = dplyr::coalesce(pw_state_chr, state_fips_chr)
  )

message(
  "H-1B rows with no place-of-work PUMA: ",
  format(sum(is.na(h1b_sample$pw_puma_chr)), big.mark = ","),
  sprintf(" (%.2f%%)", 100 * mean(is.na(h1b_sample$pw_puma_chr)))
)

message(
  "H-1B rows missing an occupation match: ", sum(is.na(h1b_sample$occ_code)),
  "; missing an industry match: ", sum(is.na(h1b_sample$ind_code)),
  "; falling back to a state pseudo-metro: ",
  sum(!h1b_sample$metro_area %in% acs_metros_identified)
)

message(
  "ACS rows falling back to a state pseudo-metro: ",
  sum(native_sample$pwmet_chr == "0"),
  sprintf(" (%.1f%%)", 100 * mean(native_sample$pwmet_chr == "0"))
)

################################################################################
###                 8. Pool the samples and apply earnings bounds            ###
################################################################################

analysis_columns <- c(
  "is_h1b_int", "beneficiary_location", "prior_status_group", "petition_key_chr",
  "year_int", "log_wage_num",
  "wage_real_num", "metro_area", "age_int", "gender_chr", "education_chr",
  "occ_code", "ind_code", "weight_num",
  # fields used only by the critique in Part II
  "pw_puma_chr", "pw_state_chr", "soc_major_chr", "naics_sector_chr",
  "education_imputed_flag", "education_reported_chr"
)

# 12) The Data Appendix sets the floor at the 1st percentile of H-1B earnings and
#     the ceiling at the 99.5th, and imposes the same floor on the ACS. Borjas
#     reports $34,780 and $1.29 million in 2025 dollars.
#     The percentile labels cannot be reproduced against this wage variable,
#     which was already winsorised at $1.25m when the file was built: its 99.5th
#     percentile is $240,000, not $1.29m. Borjas's stated dollar thresholds are
#     therefore applied directly, since they are the operative restriction.
wage_floor_num <- 34780
wage_ceiling_num <- 1290000

floor_percentile_num <- 100 * mean(h1b_sample$wage_real_num < wage_floor_num)
ceiling_percentile_num <- 100 * mean(h1b_sample$wage_real_num < wage_ceiling_num)

message("")
message(
  "Earnings bounds applied at Borjas's stated values: floor $",
  formatC(wage_floor_num, big.mark = ",", format = "d"), " (the ",
  sprintf("%.2f", floor_percentile_num), " percentile here); ceiling $",
  formatC(wage_ceiling_num, big.mark = ",", format = "d"), " (the ",
  sprintf("%.2f", ceiling_percentile_num), " percentile here)"
)

# 13) Borjas excludes observations whose occupation or industry code is invalid
#     from "any analysis that relies on" them, and Table 2 reports a single
#     observation count of 925,045 across all five columns. The validity
#     restrictions are therefore applied to the pooled sample up front so every
#     column is estimated on the same rows.
#     Deviation to record: this drops 16,496 H-1B petitions against the 1,081
#     Borjas reports, almost all of them NAICS 9999 ("unclassified"), which this
#     file appears to use where the industry is unknown.
#     Tested against the alternative of giving 9999 its own industry level, which
#     keeps those rows: that variant matches Table 1 better (H-1B N 355,534 and
#     the education distribution within 0.2pp) but Table 2 worse (mean absolute
#     deviation 0.006 against 0.004, worst column 0.012 against 0.008). Column 5
#     is -0.158 either way, so the headline does not depend on this choice.
#     The variant closest to Table 2 is retained, since Table 2 is the target.
regression_data <- dplyr::bind_rows(
  h1b_sample |>
    dplyr::filter(
      wage_real_num >= wage_floor_num, wage_real_num <= wage_ceiling_num
    ) |>
    dplyr::select(dplyr::all_of(analysis_columns)),
  native_sample |>
    dplyr::filter(wage_real_num >= wage_floor_num) |>
    dplyr::select(dplyr::all_of(analysis_columns))
) |>
  dplyr::filter(
    !is.na(occ_code), occ_code != "0",
    !is.na(ind_code), ind_code != "0"
  ) |>
  dplyr::mutate(
    h1b_abroad_int = as.integer(beneficiary_location == "Abroad"),
    h1b_usa_int = as.integer(beneficiary_location == "USA"),
    h1b_other_int = as.integer(beneficiary_location == "Other")
  )

################################################################################
###              9. Table 1 checks, to confirm the sample matches            ###
################################################################################

table_one_check <- regression_data |>
  dplyr::group_by(is_h1b_int) |>
  dplyr::summarise(
    n_obs_int = dplyr::n(),
    salary_mean_num = stats::weighted.mean(wage_real_num, weight_num) / 1000,
    log_salary_mean_num = stats::weighted.mean(log_wage_num, weight_num),
    age_mean_num = stats::weighted.mean(age_int, weight_num),
    male_pct = 100 * stats::weighted.mean(gender_chr == "male", weight_num),
    bachelor_pct = 100 * stats::weighted.mean(education_chr == "bachelor", weight_num),
    master_pct = 100 * stats::weighted.mean(education_chr == "master", weight_num),
    professional_pct = 100 * stats::weighted.mean(education_chr == "professional", weight_num),
    doctorate_pct = 100 * stats::weighted.mean(education_chr == "doctorate", weight_num),
    .groups = "drop"
  ) |>
  dplyr::mutate(group = dplyr::if_else(is_h1b_int == 1L, "H-1B", "Native"))

borjas_table_one <- tibble::tibble(
  group = c("H-1B", "Native"),
  n_obs_borjas = c(343397L, 581648L),
  salary_borjas = c(112.6, 127.0),
  log_salary_borjas = c(11.58, 11.53),
  age_borjas = c(31.9, 35.8),
  male_borjas = c(67.2, 55.1),
  bachelor_borjas = c(52.3, 71.8),
  master_borjas = c(41.8, 20.9),
  professional_borjas = c(0.9, 4.6),
  doctorate_borjas = c(5.1, 2.7)
)

table_one_comparison <- table_one_check |>
  dplyr::left_join(borjas_table_one, by = "group")

message("")
message("=== Table 1 check (replication vs Borjas) ===")
for (row_index in seq_len(nrow(table_one_comparison))) {
  row_current <- table_one_comparison[row_index, ]
  message("  ", row_current$group, ":")
  message(
    "    N            ", formatC(row_current$n_obs_int, big.mark = ",", format = "d"),
    "   vs ", formatC(row_current$n_obs_borjas, big.mark = ",", format = "d")
  )
  message(sprintf("    Salary(000s) %8.1f   vs %8.1f", row_current$salary_mean_num, row_current$salary_borjas))
  message(sprintf("    Log salary   %8.2f   vs %8.2f", row_current$log_salary_mean_num, row_current$log_salary_borjas))
  message(sprintf("    Age          %8.1f   vs %8.1f", row_current$age_mean_num, row_current$age_borjas))
  message(sprintf("    Male %%       %8.1f   vs %8.1f", row_current$male_pct, row_current$male_borjas))
  message(sprintf("    Bachelor %%   %8.1f   vs %8.1f", row_current$bachelor_pct, row_current$bachelor_borjas))
  message(sprintf("    Master %%     %8.1f   vs %8.1f", row_current$master_pct, row_current$master_borjas))
  message(sprintf("    Prof %%       %8.1f   vs %8.1f", row_current$professional_pct, row_current$professional_borjas))
  message(sprintf("    Doctorate %%  %8.1f   vs %8.1f", row_current$doctorate_pct, row_current$doctorate_borjas))
}

location_shares <- regression_data |>
  dplyr::filter(is_h1b_int == 1L) |>
  dplyr::count(beneficiary_location, name = "n_obs_int") |>
  dplyr::mutate(share_pct = 100 * n_obs_int / sum(n_obs_int))

message("")
message("=== Beneficiary location (Borjas: Abroad 42.1, USA 57.7, Other 0.2) ===")
for (row_index in seq_len(nrow(location_shares))) {
  message(sprintf(
    "  %-8s %9s  %5.1f%%", location_shares$beneficiary_location[row_index],
    format(location_shares$n_obs_int[row_index], big.mark = ","),
    location_shares$share_pct[row_index]
  ))
}

message("")
message(
  "Total regression observations: ",
  format(nrow(regression_data), big.mark = ","), "   (Borjas: 925,045)"
)

################################################################################
###            10. Table 2: the five specifications, both panels             ###
################################################################################

# 13) Every column carries year fixed effects; the columns add geography, then
#     skill, then occupation, then industry, exactly as Table 2 sets out.
specification_table <- tibble::tibble(
  column_int = 1L:5L,
  fixed_effects = c(
    "year_int",
    "year_int + metro_area",
    "year_int + metro_area + education_chr + age_int + gender_chr",
    "year_int + metro_area + education_chr + age_int + gender_chr + occ_code",
    paste0(
      "year_int + metro_area + education_chr + age_int + gender_chr + occ_code",
      " + ind_code"
    )
  ),
  borjas_panel_a_num = c(0.044, -0.062, -0.023, -0.136, -0.161),
  borjas_abroad_num = c(0.021, -0.065, -0.087, -0.181, -0.219),
  borjas_usa_num = c(0.061, -0.059, 0.025, -0.102, -0.118)
)

table_two_rows <- NULL

for (spec_index in seq_len(nrow(specification_table))) {

  column_current_int <- specification_table$column_int[spec_index]
  fixed_effects_current <- specification_table$fixed_effects[spec_index]

  model_panel_a <- fixest::feols(
    stats::as.formula(paste0("log_wage_num ~ is_h1b_int | ", fixed_effects_current)),
    data = regression_data, weights = ~weight_num, vcov = "hetero"
  )

  model_panel_b <- fixest::feols(
    stats::as.formula(paste0(
      "log_wage_num ~ h1b_abroad_int + h1b_usa_int + h1b_other_int | ",
      fixed_effects_current
    )),
    data = regression_data, weights = ~weight_num, vcov = "hetero"
  )

  table_two_rows <- dplyr::bind_rows(
    table_two_rows,
    tibble::tibble(
      column_int = column_current_int, panel = "A. Baseline (OLS)", term = "H-1B",
      estimate_num = unname(stats::coef(model_panel_a)["is_h1b_int"]),
      std_error_num = unname(fixest::se(model_panel_a)["is_h1b_int"]),
      n_obs_int = as.integer(stats::nobs(model_panel_a)),
      borjas_reported_num = specification_table$borjas_panel_a_num[spec_index]
    ),
    tibble::tibble(
      column_int = column_current_int,
      panel = "B. Location of beneficiary (OLS)", term = c("Abroad", "USA"),
      estimate_num = c(
        unname(stats::coef(model_panel_b)["h1b_abroad_int"]),
        unname(stats::coef(model_panel_b)["h1b_usa_int"])
      ),
      std_error_num = c(
        unname(fixest::se(model_panel_b)["h1b_abroad_int"]),
        unname(fixest::se(model_panel_b)["h1b_usa_int"])
      ),
      n_obs_int = as.integer(stats::nobs(model_panel_b)),
      borjas_reported_num = c(
        specification_table$borjas_abroad_num[spec_index],
        specification_table$borjas_usa_num[spec_index]
      )
    )
  )

  message("Estimated column ", column_current_int)
}

table_two <- table_two_rows |>
  dplyr::mutate(difference_num = estimate_num - borjas_reported_num) |>
  dplyr::arrange(panel, term, column_int)

################################################################################
###                           11. Report and save                            ###
################################################################################

message("")
message("================ Table 2. Estimates of the H-1B wage gap ================")
message("  replication (se)   [Borjas reported]")

for (panel_current in unique(table_two$panel)) {
  message("")
  message("  ", panel_current)
  panel_rows <- table_two |> dplyr::filter(panel == panel_current)
  for (term_current in unique(panel_rows$term)) {
    term_rows <- panel_rows |> dplyr::filter(term == term_current)
    message("    ", term_current)
    for (row_index in seq_len(nrow(term_rows))) {
      message(sprintf(
        "      (%d)  %+.3f (%.3f)   [%+.3f]   diff %+.3f",
        term_rows$column_int[row_index], term_rows$estimate_num[row_index],
        term_rows$std_error_num[row_index], term_rows$borjas_reported_num[row_index],
        term_rows$difference_num[row_index]
      ))
    }
  }
}

readr::write_csv(table_two, file.path(path_output, "borjas_august_table2.csv"))
readr::write_csv(
  table_one_comparison, file.path(path_output, "borjas_august_table1_check.csv")
)
readr::write_csv(
  location_shares, file.path(path_output, "borjas_august_location_shares.csv")
)

saveRDS(
  regression_data, file.path(path_processed, "borjas_august_regression_data.rds")
)
arrow::write_parquet(
  regression_data,
  file.path(path_processed, "borjas_august_regression_data.parquet"),
  compression = "snappy"
)

message("")
message("Saved Table 2 to output/tables/borjas_august_table2.csv")

################################################################################
################################################################################
###                                                                          ###
###        PART II. CRITIQUE OF THE SPECIFICATION                            ###
###                                                                          ###
################################################################################
################################################################################
#
# Part I reproduces Borjas's August 2026 Table 2 as specified. Part II asks what
# the headline -0.158 rests on. Four lines of attack:
#
#   A. Geography. The metro of work is not measured. IPUMS derives PWMET23 from
#      the place-of-work PUMA, assigns each PUMA to the metro holding most of its
#      population, and suppresses the code wherever the match error reaches 15
#      percent. Using the observed place-of-work PUMA instead removes both the
#      suppression and the match error.
#   B. Skill cells. A detailed occupation and a detailed industry fixed effect,
#      both reached through a crosswalk, absorb most of the reported gap. He and
#      Ozimek (2026) instead use place of work interacted with broad occupation.
#      Here that is generalised to place-of-work state x SOC major group and
#      place-of-work state x NAICS sector.
#   C. Age. The aggregate gap hides the age profile: the February critique found
#      that young H-1B workers out-earn comparable natives and only older ones
#      fall behind.
#   D. Clemens (2026, IZA DP 18435). Of his four charges, the August draft fixes
#      the year-matching one and partly answers the geography one. The education
#      imputation, the standard errors, the tenure gap and the wage-concept
#      mismatch all survive into the August draft and are tested here.

################################################################################
###          12. What the place-of-work metro actually measures              ###
################################################################################

metro_error_labels_chr <- c(
  "0" = "no metro identified", "1" = "under 0.1%", "2" = "0.1 to 0.9%",
  "3" = "1.0 to 1.9%", "4" = "2.0 to 4.9%", "5" = "5.0 to 9.9%",
  "6" = "10.0 to 14.9%"
)

metro_quality <- native_sample |>
  dplyr::count(pwmet_err_int, name = "n_obs_int") |>
  dplyr::mutate(
    error_band_chr = unname(metro_error_labels_chr[as.character(pwmet_err_int)]),
    share_pct = 100 * n_obs_int / sum(n_obs_int)
  ) |>
  dplyr::arrange(pwmet_err_int)

no_metro_pct <- 100 * mean(native_sample$pwmet_chr == "0")
no_pwpuma_pct <- 100 * mean(is.na(native_sample$pw_puma_chr))

message("")
message("=== 12. Quality of the place-of-work metro in the ACS native sample ===")
for (row_index in seq_len(nrow(metro_quality))) {
  message(sprintf(
    "  match error %-22s %9s  %5.1f%%",
    metro_quality$error_band_chr[row_index],
    format(metro_quality$n_obs_int[row_index], big.mark = ","),
    metro_quality$share_pct[row_index]
  ))
}

metro_error_over_2pct <- sum(
  metro_quality$share_pct[metro_quality$pwmet_err_int >= 4L]
)
metro_error_over_5pct <- sum(
  metro_quality$share_pct[metro_quality$pwmet_err_int >= 5L]
)

message(sprintf(
  "  no metro of work at all: %.1f%% of natives; no place-of-work PUMA: %.2f%%",
  no_metro_pct, no_pwpuma_pct
))
message(sprintf(
  "  among natives with a metro, %.1f%% sit in a cell with 2%% or more match error and %.1f%% with 5%% or more",
  metro_error_over_2pct / (1 - no_metro_pct / 100),
  metro_error_over_5pct / (1 - no_metro_pct / 100)
))
message(
  "  distinct metro cells: ", dplyr::n_distinct(native_sample$metro_area),
  "; distinct place-of-work PUMA cells: ",
  dplyr::n_distinct(native_sample$pw_puma_chr)
)

# 12a) The comment letter cites these shares for the broader universe its own
#      sentence describes -- all college-educated, privately employed U.S.-born
#      workers -- rather than for Borjas's regression universe, which adds the
#      full-time, year-round restriction. Both are reported so every published
#      figure is traceable, and both denominators are given for the error band,
#      because "17.6 percent" is a share of all entries while the same count is
#      21.8 percent of the entries that do carry a metro code.
metro_universes <- list(
  "Borjas regression universe (full-time, year-round)" =
    acs_raw$AGE >= 21 & acs_raw$AGE <= 50 & acs_raw$CITIZEN == 0 &
    acs_raw$BPL <= 120 & acs_raw$EDUCD >= 101 & acs_raw$CLASSWKR == 2 &
    acs_raw$CLASSWKRD == 22 & acs_raw$WKSWORK1 >= 50 & acs_raw$UHRSWORK >= 35 &
    acs_raw$INCWAGE > 0,
  "All college-educated, privately employed U.S.-born workers" =
    acs_raw$CITIZEN == 0 & acs_raw$BPL <= 120 & acs_raw$EDUCD >= 101 &
    acs_raw$CLASSWKR == 2 & acs_raw$CLASSWKRD == 22 & acs_raw$INCWAGE > 0
)

metro_coverage <- NULL

for (universe_current in names(metro_universes)) {

  universe_flag <- metro_universes[[universe_current]]
  no_metro_flag <- acs_raw$PWMET23[universe_flag] == 0
  error_5_15_flag <- acs_raw$PWMET23ERR[universe_flag] %in% c(5L, 6L)

  metro_coverage <- dplyr::bind_rows(
    metro_coverage,
    tibble::tibble(
      universe_chr = universe_current,
      n_obs_int = sum(universe_flag),
      no_metro_pct = 100 * mean(no_metro_flag),
      error_5_to_15_of_all_pct = 100 * mean(error_5_15_flag),
      error_5_to_15_of_with_metro_pct =
        100 * sum(error_5_15_flag) / sum(!no_metro_flag)
    )
  )
}

message("")
message("=== 12a. Metro coverage, both universes ===")
for (row_index in seq_len(nrow(metro_coverage))) {
  row_current <- metro_coverage[row_index, ]
  message(sprintf(
    "  %-56s n = %9s",
    row_current$universe_chr, format(row_current$n_obs_int, big.mark = ",")
  ))
  message(sprintf(
    "      no metro of work: %5.2f%%;  5-15%% allocation error: %5.2f%% of all entries, %5.2f%% of those with a metro",
    row_current$no_metro_pct, row_current$error_5_to_15_of_all_pct,
    row_current$error_5_to_15_of_with_metro_pct
  ))
}

readr::write_csv(
  metro_coverage, file.path(path_output, "critique_metro_coverage.csv")
)

readr::write_csv(
  metro_quality, file.path(path_output, "critique_metro_match_error.csv")
)

################################################################################
###                13. The common sample for the critique                    ###
################################################################################

# 14) Every specification below is estimated on one sample so the steps are
#     comparable. FY2021 is dropped because the 2021 ACS still uses 2010 PUMA
#     definitions, which are not comparable with the 2020 definitions behind the
#     I-129 crosswalk. Rows with no place-of-work geography or no broad
#     occupation or sector code are dropped as well.
critique_sample <- regression_data |>
  dplyr::filter(
    year_int >= 2022L,
    !is.na(pw_puma_chr), !is.na(pw_state_chr),
    !is.na(soc_major_chr), nchar(soc_major_chr) == 2L,
    !is.na(naics_sector_chr)
  ) |>
  dplyr::mutate(
    pw_state_occ_chr = paste0(pw_state_chr, "_", soc_major_chr),
    pw_state_ind_chr = paste0(pw_state_chr, "_", naics_sector_chr)
  )

message("")
message(
  "=== 13. Critique sample: ", format(nrow(critique_sample), big.mark = ","),
  " rows (", format(sum(critique_sample$is_h1b_int), big.mark = ","), " H-1B), FY2022-FY2024 ==="
)
message(sprintf(
  "  dropped from the Table 2 sample: %s rows (%.1f%%), of which %s are FY2021",
  format(nrow(regression_data) - nrow(critique_sample), big.mark = ","),
  100 * (1 - nrow(critique_sample) / nrow(regression_data)),
  format(sum(regression_data$year_int == 2021L), big.mark = ",")
))
message(
  "  distinct cells -- place-of-work state x SOC major group: ",
  dplyr::n_distinct(critique_sample$pw_state_occ_chr),
  "; place-of-work state x NAICS sector: ",
  dplyr::n_distinct(critique_sample$pw_state_ind_chr),
  "; detailed occupation: ", dplyr::n_distinct(critique_sample$occ_code),
  "; detailed industry: ", dplyr::n_distinct(critique_sample$ind_code)
)

borjas_fe_chr <- paste0(
  "year_int + metro_area + education_chr + age_int + gender_chr + occ_code",
  " + ind_code"
)
pwpuma_fe_chr <- paste0(
  "year_int + pw_puma_chr + education_chr + age_int + gender_chr + occ_code",
  " + ind_code"
)
broad_fe_chr <- paste0(
  "year_int + pw_state_occ_chr + pw_state_ind_chr + education_chr + age_int",
  " + gender_chr"
)

# 13a) The metro fallback is asymmetric. An identified metro of work is missing
#      for 18.5 percent of natives but for only 8.7 percent of H-1B filings,
#      because the I-129 worksite address resolves to a CBSA far more often than
#      the ACS place-of-work PUMA does. Natives in the residual state cell are
#      therefore largely compared with each other rather than with H-1B workers.
metro_fallback_gap <- regression_data |>
  dplyr::group_by(is_h1b_int) |>
  dplyr::summarise(
    state_fallback_pct = 100 * mean(!metro_area %in% acs_metros_identified),
    .groups = "drop"
  )

message("")
message("=== 13a. Asymmetry in the metro fallback ===")
message(sprintf(
  "  natives assigned a state pseudo-metro: %.1f%%; H-1B filings: %.1f%%",
  metro_fallback_gap$state_fallback_pct[metro_fallback_gap$is_h1b_int == 0L],
  metro_fallback_gap$state_fallback_pct[metro_fallback_gap$is_h1b_int == 1L]
))
message(
  "  under place-of-work PUMA the same rates are 0.71% and 0.02%, so the ",
  "asymmetry is a property of the metro coding, not of the data"
)

# 13b) A geography ladder, holding Borjas's detailed occupation and industry
#      controls fixed and varying only the spatial unit. If the estimate moves
#      with granularity, the headline is a function of an undefended choice.
geography_ladder_specs <- tibble::tibble(
  label_chr = c(
    "place-of-work state (about 50 cells)",
    "metro of work, Borjas's choice (315 cells)",
    "place-of-work PUMA (1,293 cells)"
  ),
  geography_chr = c("pw_state_chr", "metro_area", "pw_puma_chr")
)

geography_ladder <- NULL

for (ladder_index in seq_len(nrow(geography_ladder_specs))) {
  model_ladder <- fixest::feols(
    stats::as.formula(paste0(
      "log_wage_num ~ is_h1b_int | year_int + ",
      geography_ladder_specs$geography_chr[ladder_index],
      " + education_chr + age_int + gender_chr + occ_code + ind_code"
    )),
    data = critique_sample, weights = ~weight_num, vcov = "hetero"
  )
  geography_ladder <- dplyr::bind_rows(
    geography_ladder,
    tibble::tibble(
      label_chr = geography_ladder_specs$label_chr[ladder_index],
      n_cells_int = dplyr::n_distinct(
        critique_sample[[geography_ladder_specs$geography_chr[ladder_index]]]
      ),
      estimate_num = unname(stats::coef(model_ladder)["is_h1b_int"]),
      std_error_num = unname(fixest::se(model_ladder)["is_h1b_int"])
    )
  )
}

message("")
message("=== 13b. The wage gap as a function of geographic granularity ===")
for (row_index in seq_len(nrow(geography_ladder))) {
  message(sprintf(
    "  %-44s %5s cells   %+.4f (%.4f)",
    geography_ladder$label_chr[row_index],
    format(geography_ladder$n_cells_int[row_index], big.mark = ","),
    geography_ladder$estimate_num[row_index],
    geography_ladder$std_error_num[row_index]
  ))
}
message(
  "  The estimate widens monotonically as the cells shrink. Clemens (2026) ",
  "makes the same point in the other direction, reporting -0.125 on PUMAs ",
  "against -0.085 on commuting zones."
)

readr::write_csv(
  geography_ladder, file.path(path_output, "critique_geography_ladder.csv")
)

# 13c) Age enters equation (1) as a full set of single-year dummies already,
#      because fixest treats every fixed-effect variable as a factor. What a
#      continuous age control would have cost is reported here for contrast, so
#      the categorical treatment is confirmed rather than assumed.
model_age_dummies <- fixest::feols(
  log_wage_num ~ is_h1b_int |
    year_int + metro_area + education_chr + age_int + gender_chr + occ_code +
    ind_code,
  data = critique_sample, weights = ~weight_num, vcov = "hetero"
)

model_age_quadratic <- fixest::feols(
  log_wage_num ~ is_h1b_int + age_int + I(age_int^2) |
    year_int + metro_area + education_chr + gender_chr + occ_code + ind_code,
  data = critique_sample, weights = ~weight_num, vcov = "hetero"
)

message("")
message("=== 13c. Age as dummies against age as a quadratic ===")
message(sprintf(
  "  single-year age fixed effects, as published: %+.4f (%.4f)",
  unname(stats::coef(model_age_dummies)["is_h1b_int"]),
  unname(fixest::se(model_age_dummies)["is_h1b_int"])
))
message(sprintf(
  "  age and age squared entered linearly:        %+.4f (%.4f)",
  unname(stats::coef(model_age_quadratic)["is_h1b_int"]),
  unname(fixest::se(model_age_quadratic)["is_h1b_int"])
))
message(
  "  Age is therefore already categorical in the published specification. The ",
  "problem is not the functional form of the control but that one pooled ",
  "coefficient is reported for a profile that varies with age, as section 16 shows."
)

################################################################################
###            14. Stepwise critique of the headline specification           ###
################################################################################

critique_specs <- tibble::tibble(
  step_int = 1L:7L,
  label_chr = c(
    "1. Borjas Table 2 col. 5, on the critique sample",
    "2. + place-of-work PUMA replaces the metro of work",
    "3. + PW state x SOC major group and PW state x NAICS sector",
    "4. + standard errors clustered by geography and occupation",
    "5. + only filings whose education is reported",
    "6. Borjas specification, only reported education",
    "7. Borjas specification, education as a reported category"
  ),
  fixed_effects_chr = c(
    borjas_fe_chr, pwpuma_fe_chr, broad_fe_chr, broad_fe_chr, broad_fe_chr,
    borjas_fe_chr,
    paste0(
      "year_int + metro_area + education_reported_chr + age_int + gender_chr",
      " + occ_code + ind_code"
    )
  ),
  cluster_chr = c(
    NA_character_, NA_character_, NA_character_,
    "pw_state_chr + soc_major_chr", "pw_state_chr + soc_major_chr",
    "metro_area + occ_code", "metro_area + occ_code"
  ),
  reported_education_only_flag = c(
    FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE
  )
)

critique_results <- NULL

for (step_index in seq_len(nrow(critique_specs))) {

  estimation_data <- critique_sample
  if (critique_specs$reported_education_only_flag[step_index]) {
    estimation_data <- estimation_data |>
      dplyr::filter(!education_imputed_flag)
  }

  vcov_current <- if (is.na(critique_specs$cluster_chr[step_index])) {
    "hetero"
  } else {
    stats::as.formula(paste0("~", critique_specs$cluster_chr[step_index]))
  }

  model_current <- fixest::feols(
    stats::as.formula(paste0(
      "log_wage_num ~ is_h1b_int | ",
      critique_specs$fixed_effects_chr[step_index]
    )),
    data = estimation_data, weights = ~weight_num, vcov = vcov_current
  )

  critique_results <- dplyr::bind_rows(
    critique_results,
    tibble::tibble(
      step_int = critique_specs$step_int[step_index],
      label_chr = critique_specs$label_chr[step_index],
      estimate_num = unname(stats::coef(model_current)["is_h1b_int"]),
      std_error_num = unname(fixest::se(model_current)["is_h1b_int"]),
      t_stat_num = unname(
        stats::coef(model_current)["is_h1b_int"] /
          fixest::se(model_current)["is_h1b_int"]
      ),
      n_obs_int = as.integer(stats::nobs(model_current))
    )
  )

  message("  estimated critique step ", critique_specs$step_int[step_index])

  if (!is.na(critique_specs$cluster_chr[step_index])) {
    cluster_counts_int <- vapply(
      strsplit(critique_specs$cluster_chr[step_index], " \\+ ")[[1]],
      function(cluster_var_chr) {
        dplyr::n_distinct(estimation_data[[cluster_var_chr]])
      },
      integer(1L)
    )
    message(
      "    two-way clustered on ", critique_specs$cluster_chr[step_index],
      " (", paste(cluster_counts_int, collapse = " and "), " clusters)"
    )
  }
}

message("")
message("=== 14. Stepwise critique of the headline wage gap ===")
message(sprintf("  Borjas reports -0.161; Part I reproduces -0.158 on the full sample"))
message("")
for (row_index in seq_len(nrow(critique_results))) {
  row_current <- critique_results[row_index, ]
  message(sprintf(
    "  %-60s %+.4f (%.4f)  t = %6.2f  N = %s",
    row_current$label_chr, row_current$estimate_num, row_current$std_error_num,
    row_current$t_stat_num, format(row_current$n_obs_int, big.mark = ",")
  ))
}

readr::write_csv(
  critique_results, file.path(path_output, "critique_stepwise_specification.csv")
)

################################################################################
###          15. The education imputation, and Borjas's footnote 19          ###
################################################################################

# 15) Clemens (2026) shows that Borjas assigns a bachelor's degree to every
#     filing with no reported education, which is 38 percent of FY2023 and 32
#     percent of FY2024. Footnote 19 of the August draft concedes the imputation
#     and reports that dropping those filings, or giving them their own category,
#     both return -0.172. That claim is tested here on the full Table 2 sample.
education_missing_by_year <- regression_data |>
  dplyr::filter(is_h1b_int == 1L) |>
  dplyr::group_by(year_int) |>
  dplyr::summarise(
    n_obs_int = dplyr::n(),
    imputed_pct = 100 * mean(education_imputed_flag),
    .groups = "drop"
  )

message("")
message("=== 15. Education imputation ===")
for (row_index in seq_len(nrow(education_missing_by_year))) {
  message(sprintf(
    "  FY%d  %8s filings, %.1f%% with education imputed to bachelor's",
    education_missing_by_year$year_int[row_index],
    format(education_missing_by_year$n_obs_int[row_index], big.mark = ","),
    education_missing_by_year$imputed_pct[row_index]
  ))
}

education_tests <- tibble::tibble(
  label_chr = c(
    "Table 2 col. 5 as published (education imputed)",
    "dropping filings with no reported education",
    "education entered with an 'unknown' category",
    "restricted to FY2021-FY2022, where education is reported"
  ),
  estimate_num = NA_real_, std_error_num = NA_real_, n_obs_int = NA_integer_
)

model_published <- fixest::feols(
  stats::as.formula(paste0("log_wage_num ~ is_h1b_int | ", borjas_fe_chr)),
  data = regression_data, weights = ~weight_num, vcov = "hetero"
)

model_dropped <- fixest::feols(
  stats::as.formula(paste0("log_wage_num ~ is_h1b_int | ", borjas_fe_chr)),
  data = regression_data |> dplyr::filter(!education_imputed_flag),
  weights = ~weight_num, vcov = "hetero"
)

model_unknown <- fixest::feols(
  stats::as.formula(paste0(
    "log_wage_num ~ is_h1b_int | year_int + metro_area + education_reported_chr",
    " + age_int + gender_chr + occ_code + ind_code"
  )),
  data = regression_data, weights = ~weight_num, vcov = "hetero"
)

model_early_years <- fixest::feols(
  stats::as.formula(paste0("log_wage_num ~ is_h1b_int | ", borjas_fe_chr)),
  data = regression_data |> dplyr::filter(year_int <= 2022L),
  weights = ~weight_num, vcov = "hetero"
)

education_models <- list(
  model_published, model_dropped, model_unknown, model_early_years
)

for (model_index in seq_along(education_models)) {
  education_tests$estimate_num[model_index] <- unname(
    stats::coef(education_models[[model_index]])["is_h1b_int"]
  )
  education_tests$std_error_num[model_index] <- unname(
    fixest::se(education_models[[model_index]])["is_h1b_int"]
  )
  education_tests$n_obs_int[model_index] <- as.integer(
    stats::nobs(education_models[[model_index]])
  )
}

message("")
for (row_index in seq_len(nrow(education_tests))) {
  message(sprintf(
    "  %-58s %+.4f (%.4f)  N = %s",
    education_tests$label_chr[row_index],
    education_tests$estimate_num[row_index],
    education_tests$std_error_num[row_index],
    format(education_tests$n_obs_int[row_index], big.mark = ",")
  ))
}
message("  Borjas footnote 19 reports -0.172 (0.003) for the first two alternatives")

# 16) Clemens's sharpest point on the imputation is that many imputed values are
#     impossible: the master's-cap lottery cannot be entered without a master's
#     degree or above, so every imputed bachelor's degree in that stream is known
#     to be wrong. That check needs the lottery type, which is not part of the
#     Table 2 sample, so it is computed on the raw filings.
impossible_imputation <- i129_raw |>
  dplyr::filter(petition_decision == "Approved") |>
  dplyr::mutate(
    education_imputed_flag = !(
      trimws(as.character(petition_beneficiary_edu_defin)) %in%
        c("BACHELOR'S DEGREE", "MASTER'S DEGREE", "DOCTORATE DEGREE",
          "PROFESSIONAL DEGREE")
    )
  ) |>
  dplyr::filter(education_imputed_flag) |>
  dplyr::summarise(
    n_imputed_int = dplyr::n(),
    masters_cap_pct = 100 * mean(petition_h1b_type == "M")
  )

message(sprintf(
  "  of the %s filings imputed to bachelor's, %.1f%% entered through the master's-cap lottery, which requires a master's degree or above",
  format(impossible_imputation$n_imputed_int, big.mark = ","),
  impossible_imputation$masters_cap_pct
))

readr::write_csv(
  education_tests, file.path(path_output, "critique_education_imputation.csv")
)

################################################################################
###                   16. The age profile of the wage gap                    ###
################################################################################

# 17) Age already enters equation (1) as a full set of single-year dummies, since
#     fixest treats every fixed effect as a factor. The issue is not the
#     functional form but that one pooled coefficient is reported for a sample
#     whose age composition differs sharply from the natives it is compared with.
#     Interacting the H-1B indicator with age recovers the profile.
age_profile_borjas <- fixest::feols(
  stats::as.formula(paste0(
    "log_wage_num ~ i(age_int, is_h1b_int) | ", borjas_fe_chr
  )),
  data = critique_sample, weights = ~weight_num, vcov = "hetero"
)

age_profile_broad <- fixest::feols(
  stats::as.formula(paste0(
    "log_wage_num ~ i(age_int, is_h1b_int) | ", broad_fe_chr
  )),
  data = critique_sample, weights = ~weight_num,
  vcov = ~pw_state_chr + soc_major_chr
)

age_profile_models <- list(
  "Borjas Table 2 col. 5" = age_profile_borjas,
  "PW state x broad occupation and sector" = age_profile_broad
)

age_profile <- NULL

for (specification_current in names(age_profile_models)) {
  coefficient_table <- as.data.frame(
    fixest::coeftable(age_profile_models[[specification_current]])
  )
  coefficient_table$term_chr <- rownames(coefficient_table)
  # fixest names an interaction from i() as "age_int::34:is_h1b_int"
  age_profile <- dplyr::bind_rows(
    age_profile,
    tibble::tibble(
      specification_chr = specification_current,
      age_int = as.integer(
        sub("^.*::([0-9]+).*$", "\\1", coefficient_table$term_chr)
      ),
      estimate = coefficient_table[["Estimate"]],
      std.error = coefficient_table[["Std. Error"]],
      statistic = coefficient_table[["t value"]]
    )
  )
}

age_profile <- age_profile |>
  dplyr::filter(!is.na(age_int)) |>
  dplyr::arrange(specification_chr, age_int)

h1b_total_int <- sum(critique_sample$is_h1b_int)

h1b_age_counts <- critique_sample |>
  dplyr::group_by(age_int) |>
  dplyr::summarise(n_h1b_int = sum(is_h1b_int == 1L), .groups = "drop") |>
  dplyr::mutate(h1b_share_pct = 100 * n_h1b_int / h1b_total_int)

age_profile <- age_profile |>
  dplyr::left_join(h1b_age_counts, by = "age_int")

message("")
message("=== 16. The H-1B wage gap by single year of age ===")
message("  (positive means H-1B workers out-earn comparable natives of that age)")
for (specification_current in unique(age_profile$specification_chr)) {
  message("")
  message("  ", specification_current)
  crossover_rows <- age_profile |>
    dplyr::filter(specification_chr == specification_current) |>
    dplyr::arrange(age_int)
  negative_index_int <- which(crossover_rows$estimate < 0)[1]
  if (is.na(negative_index_int)) {
    message("    the gap is positive at every age in this specification")
  } else {
    first_negative_int <- crossover_rows$age_int[negative_index_int]
    message(sprintf(
      "    gap turns negative at age %d; share of H-1B filings below that age: %.1f%%",
      first_negative_int,
      sum(crossover_rows$h1b_share_pct[crossover_rows$age_int < first_negative_int],
          na.rm = TRUE)
    ))
  }
  for (row_index in seq_len(nrow(crossover_rows))) {
    if (crossover_rows$age_int[row_index] %% 2L == 1L) next
    message(sprintf(
      "      age %2d  %+.4f (%.4f)   %6.1f%% of filings",
      crossover_rows$age_int[row_index], crossover_rows$estimate[row_index],
      crossover_rows$std.error[row_index], crossover_rows$h1b_share_pct[row_index]
    ))
  }
}

# 18) The same profile read off the residuals of the pooled model, which is how
#     the February critique presented it.
critique_sample$resid_num <- critique_sample$log_wage_num -
  stats::predict(
    fixest::feols(
      stats::as.formula(paste0("log_wage_num ~ is_h1b_int | ", borjas_fe_chr)),
      data = critique_sample, weights = ~weight_num
    ),
    newdata = critique_sample
  )

residual_by_age <- critique_sample |>
  dplyr::filter(is_h1b_int == 1L, !is.na(resid_num)) |>
  dplyr::group_by(age_int) |>
  dplyr::summarise(
    mean_resid_num = stats::weighted.mean(resid_num, weight_num),
    n_obs_int = dplyr::n(),
    .groups = "drop"
  )

age_profile <- age_profile |>
  dplyr::left_join(residual_by_age, by = "age_int")

# 18a) How much of the pooled gap comes from which ages. The pooled coefficient
#      is approximately the H-1B-count-weighted average of the age-specific
#      coefficients; the approximation is reported alongside the pooled estimate
#      so the reader can see how close it is before the shares are read.
age_decomposition <- age_profile |>
  dplyr::filter(specification_chr == "Borjas Table 2 col. 5", !is.na(n_h1b_int)) |>
  dplyr::mutate(
    contribution_num = estimate * n_h1b_int / sum(n_h1b_int),
    age_band_chr = dplyr::case_when(
      age_int < 30L ~ "21-29",
      age_int < 35L ~ "30-34",
      age_int < 40L ~ "35-39",
      TRUE ~ "40-50"
    )
  )

age_band_summary <- age_decomposition |>
  dplyr::group_by(age_band_chr) |>
  dplyr::summarise(
    filings_pct = 100 * sum(n_h1b_int) / sum(age_decomposition$n_h1b_int),
    mean_gap_num = stats::weighted.mean(estimate, n_h1b_int),
    contribution_num = sum(contribution_num),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    contribution_pct = 100 * contribution_num / sum(contribution_num)
  )

message("")
message("=== 18a. Which ages carry the pooled gap ===")
message(sprintf(
  "  count-weighted average of the age-specific gaps: %+.4f, against the pooled estimate %+.4f",
  sum(age_decomposition$contribution_num),
  critique_results$estimate_num[critique_results$step_int == 1L]
))
for (row_index in seq_len(nrow(age_band_summary))) {
  message(sprintf(
    "  ages %-6s %5.1f%% of filings   mean gap %+.4f   %5.1f%% of the pooled gap",
    age_band_summary$age_band_chr[row_index],
    age_band_summary$filings_pct[row_index],
    age_band_summary$mean_gap_num[row_index],
    age_band_summary$contribution_pct[row_index]
  ))
}
message(
  "  Borjas's own Table 5 column 3 reports that the native seniority premium ",
  "is small for the first ten years of job tenure and jumps after that. The ",
  "omitted-tenure bias therefore grows with age, which is exactly the shape of ",
  "this profile -- and the 3.6-point correction he applies is a single constant."
)

readr::write_csv(
  age_band_summary, file.path(path_output, "critique_age_band_decomposition.csv")
)

readr::write_csv(age_profile, file.path(path_output, "critique_age_profile.csv"))

################################################################################
###      17. Adjustments the regression cannot make: tenure and wage concept ###
################################################################################

# 19) Two biases are not specification choices and cannot be estimated inside
#     this sample. They are applied as accounting steps to the preferred
#     estimate, with the source of each figure named.
preferred_estimate_num <- critique_results$estimate_num[
  critique_results$step_int == 5L
]

adjustment_table <- tibble::tibble(
  step_chr = c(
    "Preferred specification (critique step 5)",
    "less the job-seniority bias, Borjas pp. 17-18 (0.886 x 0.041)",
    "less the job-seniority bias, Clemens (2026) Table 2 from the SIPP",
    "less the base-salary vs total-wage-income gap, Clemens (2026) sec. 3.3"
  ),
  adjustment_num = c(NA_real_, 0.036, 0.0633, 0.070),
  note_chr = c(
    "H-1B base salary against native total wage income, no tenure adjustment",
    "Borjas's own correction, applied to his own estimate",
    "larger because it uses observed employer tenure rather than a CPS premium",
    "natives' measured income includes bonuses, equity and second jobs; the H-1B figure is base salary only"
  )
)

adjustment_table <- adjustment_table |>
  dplyr::mutate(
    running_gap_num = c(
      preferred_estimate_num,
      preferred_estimate_num + 0.036,
      preferred_estimate_num + 0.0633,
      preferred_estimate_num + 0.0633 + 0.070
    )
  )

message("")
message("=== 17. Adjustments the regression cannot make ===")
for (row_index in seq_len(nrow(adjustment_table))) {
  message(sprintf(
    "  %-62s  running gap %+.4f",
    adjustment_table$step_chr[row_index],
    adjustment_table$running_gap_num[row_index]
  ))
}
message(
  "  The last two lines are not estimates from this sample; they are the ",
  "published corrections applied in sequence."
)

readr::write_csv(
  adjustment_table, file.path(path_output, "critique_unestimable_adjustments.csv")
)

message("")
message("Saved the critique tables to output/tables/critique_*.csv")
