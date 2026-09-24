# run_all.R -- pipeline orchestrator for the H-1B fee comment letter
# Author - Jiaxin He
# research title - H-1B Fee Analysis and Comment Letter
# research question - Do the justifications for the H-1B fee hold up to scrutiny?
#
# Reproduces every number and figure in
# drafts/H1B Comment Letter Quantitative Section.pdf.
#
# The pipeline has two arms. Scripts 01-03 audit the DHS Technical Appendix and
# depend on nothing but the appendix tables. Scripts 04-08 run in strict sequence:
# each one reads a checkpoint written by the one before it.
#
#   01 -> appendix arithmetic, including the footnote 8 error
#   02 -> the mechanical identity behind the appendix's positive fee elasticity
#   03 -> the elasticity implied for consular receipts
#   04 -> replicates Borjas's August 2026 Table 2, writes the pooled sample
#   05 -> the labour demand model, writes the payroll savings
#   06 -> the four allocation regimes, writes the selection counts
#   07 -> prices the $103,265 fee against each regime
#   08 -> fee incidence by payroll-savings and wage quartile
#
# Script 04 needs a FRED API key. Put FRED_API_KEY=<your key> in ~/.Renviron.

rm(list = ls())
options(scipen = 999)
set.seed(42)

################################################################################
###                           1. Select scripts                              ###
################################################################################

# 1) Toggle scripts with explicit TRUE/FALSE flags. The Borjas arm must run in
#    order: 05, 06, 07 and 08 all read checkpoints written upstream.
run_01_appendix_audit_flag <- TRUE
run_02_mechanical_identity_sim_flag <- TRUE
run_03_implied_elasticity_flag <- TRUE
run_04_borjas_replication_flag <- TRUE
run_05_borjas_labor_demand_flag <- TRUE
run_06_allocation_simulation_flag <- TRUE
run_07_fee_under_reformed_rules_flag <- TRUE
run_08_fee_incidence_by_quartile_flag <- TRUE

# 2) Map each flag to one script; list order is execution order
script_flags <- list(
  "01_appendix_audit.R" = run_01_appendix_audit_flag,
  "02_mechanical_identity_sim.R" = run_02_mechanical_identity_sim_flag,
  "03_implied_elasticity.R" = run_03_implied_elasticity_flag,
  "04_borjas_replication.R" = run_04_borjas_replication_flag,
  "05_borjas_labor_demand.R" = run_05_borjas_labor_demand_flag,
  "06_allocation_simulation.R" = run_06_allocation_simulation_flag,
  "07_fee_under_reformed_rules.R" = run_07_fee_under_reformed_rules_flag,
  "08_fee_incidence_by_quartile.R" = run_08_fee_incidence_by_quartile_flag
)

scripts_to_run <- names(script_flags)[
  vapply(script_flags, isTRUE, logical(1L))
]

################################################################################
###                           2. Load packages                               ###
################################################################################

# 3) Packages required across the selected scripts
required_packages <- c(
  "tidyverse", "data.table", "fixest", "arrow", "ipumsr", "fredr",
  "tidycensus", "readxl", "Hmisc"
)

# 4) Verify each required package is installed before anything runs
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "),
    ". Install them before running the pipeline.",
    call. = FALSE
  )
}

################################################################################
###                              3. Set paths                                ###
################################################################################

# 5) Resolve the project root without a hard-coded, user-specific path
if (requireNamespace("here", quietly = TRUE)) {
  path_project <- here::here()
} else {
  path_project <- getwd()
  message(
    "Package 'here' is not installed; using the working directory as the ",
    "project root:\n  ", path_project,
    "\nRun this script from the project root, or install 'here'."
  )
}

path_code <- file.path(path_project, "code")
path_logs <- file.path(path_project, "output", "logs")

if (!dir.exists(path_logs)) {
  dir.create(path_logs, recursive = TRUE)
}

# 6) Fail fast when a selected script cannot possibly succeed
missing_scripts <- scripts_to_run[
  !file.exists(file.path(path_code, scripts_to_run))
]

if (length(missing_scripts) > 0L) {
  stop(
    "Selected script(s) not found in ", path_code, ": ",
    paste(missing_scripts, collapse = ", "),
    call. = FALSE
  )
}

if (isTRUE(run_04_borjas_replication_flag) && !nzchar(Sys.getenv("FRED_API_KEY"))) {
  stop(
    "04_borjas_replication.R needs a FRED API key. Add FRED_API_KEY=<your key> ",
    "to ~/.Renviron and restart R. Free keys: ",
    "https://fredaccount.stlouisfed.org/apikeys",
    call. = FALSE
  )
}

################################################################################
###                           4. Run the pipeline                            ###
################################################################################

log_file_path <- file.path(
  path_logs, paste0("run_all_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

run_record <- data.frame(
  script = character(0), status = character(0), minutes = numeric(0)
)

message("Pipeline start: ", format(Sys.time()))
message("Scripts selected: ", length(scripts_to_run))

for (script_name in scripts_to_run) {

  message("")
  message("--- ", script_name, " ---")
  script_start_time <- Sys.time()

  # 7) Each script runs in a fresh environment so nothing leaks between steps
  script_result <- tryCatch(
    {
      source(
        file.path(path_code, script_name),
        local = new.env(parent = globalenv()),
        echo = FALSE
      )
      "ok"
    },
    error = function(condition) {
      paste0("FAILED: ", conditionMessage(condition))
    }
  )

  elapsed_minutes_num <- as.numeric(
    difftime(Sys.time(), script_start_time, units = "mins")
  )

  run_record <- rbind(
    run_record,
    data.frame(
      script = script_name, status = script_result,
      minutes = round(elapsed_minutes_num, 2)
    )
  )

  message(
    "    ", script_result, " in ", sprintf("%.1f", elapsed_minutes_num), " minutes"
  )

  # 8) Stop the chain on failure: every later script reads this one's output
  if (script_result != "ok") {
    utils::write.csv(run_record, log_file_path, row.names = FALSE)
    stop(
      "Pipeline halted at ", script_name, ". ", script_result,
      "\nRun log: ", log_file_path,
      call. = FALSE
    )
  }
}

utils::write.csv(run_record, log_file_path, row.names = FALSE)

message("")
message("Pipeline complete: ", format(Sys.time()))
message("Total minutes: ", sprintf("%.1f", sum(run_record$minutes)))
message("Run log: ", log_file_path)
