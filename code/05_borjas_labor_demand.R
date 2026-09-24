# 05_borjas_labor_demand -- Borjas August 2026 draft, Figures 1-2, Tables 8-9
# Author - Jiaxin He
# research title - H-1B Fee Analysis and Comment Letter
# research question - Even if we take Borjas's methods, what is wrong with how the
#   DHS interpreted it?
#
# Implements the visa-fee labour demand model in Borjas, NBER WP 34793, revised
# August 2026, Sections V-VI (pp. 21-29).
#
# Native wage equation (6), estimated on natives only:
#     log w_n = X'b_n + m_n + d_n + t_n + e
# Oaxaca-Blinder prediction for H-1B worker h, equation (7):
#     log what_nh = X_h'b_n + mhat + dhat + that
# Payroll savings, equation (8): dw_h = log what_nh - log w_h
# Hiring rule with a one-time fee F, equation (8):
#     pi_h + log(1 + F / (w_h * exp(pi_h) * R)) <= dw_h
# which solves analytically for the largest fee at which worker h is still hired:
#     F*_h = w_h * exp(pi_h) * R * (exp(dw_h - pi_h) - 1)
# so a single draw of pi_h yields the whole fee curve by sorting F*.
#
# Extension for Table 9, equations (12)-(13): wages grow at g and job tenure T is
# geometric with a 9.4 percent annual separation rate, giving
#     F*_h = w_h * exp(pi_h) * Rg(T) * (exp(dw_h + log(R(T)/Rg(T)) - pi_h) - 1)
# with the truncation point for pi_h shifted to dw_h + log(R(T)/Rg(T)).
#
# Targets:
#   Figure 1  mean payroll savings 0.120 after the seniority adjustment; 68.3% positive
#   Figure 2  degenerate pi: 68% hired at F = 0, 45% at F = $100,000
#             all 85,000 visas used up to $87,000 (mu = 0, N = 170,000)
#   Table 8   revenue-maximising fee $117k / $140k / $164k at N = 170,000
#   Table 9   revenue-maximising fee  $95k / $115k / $136k at N = 170,000

rm(list = ls())
options(scipen = 999)
set.seed(42)

################################################################################
###                           1. Load packages                               ###
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(arrow)
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
path_figures <- file.path(path_project, "output", "figures")
path_style <- file.path(path_project, "Infrastructure", "style")

for (path_name in c(path_output, path_figures)) {
  if (!dir.exists(path_name)) dir.create(path_name, recursive = TRUE)
}

discount_rate_num <- 0.03          # 3 percent, stated in the paper
visa_term_int <- 6L                # six-year visa term
wage_growth_num <- 0.015           # 1.5 percent annual H-1B wage growth (Table 9)
separation_rate_num <- 0.094       # 9.4 percent annual separation rate (Table 9)
seniority_adjustment_num <- 0.036  # payroll savings shifted left by 3.6 points
savings_top_code_num <- 0.4        # footnote 33
savings_bottom_code_num <- -0.7    # footnote 33
sigma_pi_num <- 0.5 / 2.576        # 99 percent of pi in [-0.5, 0.5] when mu = 0
mu_pi_values <- c(-0.1, 0.0, 0.1)
n_replications_int <- 100L
visa_cap_int <- 85000L
excess_demand_values <- c(170000L, 255000L, 340000L)
fee_grid_num <- seq(0, 400000, by = 1000)

# 1) Payments are treated as made at the start of each year, so the first year is
#    undiscounted. R = 5.5797 on this convention and 5.4172 if payments fall at
#    year end; the paper does not state which, so both are reported against the
#    Figure 2 benchmark below and the closer one is used.
discount_factor_num <- sum(1 / (1 + discount_rate_num)^(0:(visa_term_int - 1L)))
growth_discount_factor_num <- sum(
  ((1 + wage_growth_num) / (1 + discount_rate_num))^(0:(visa_term_int - 1L))
)

message(
  "R = ", sprintf("%.4f", discount_factor_num),
  "; R_g = ", sprintf("%.4f", growth_discount_factor_num),
  "; sigma_pi = ", sprintf("%.4f", sigma_pi_num)
)

################################################################################
###          3. Native wage equation (6) and the Oaxaca-Blinder step         ###
################################################################################

regression_data <- readRDS(
  file.path(path_processed, "borjas_august_regression_data.rds")
)

native_data <- regression_data |> dplyr::filter(is_h1b_int == 0L)
h1b_data <- regression_data |> dplyr::filter(is_h1b_int == 1L)

message(
  "Loaded ", format(nrow(native_data), big.mark = ","), " natives and ",
  format(nrow(h1b_data), big.mark = ","), " H-1B petitions"
)

# 2) Equation (6) is estimated on the native sample alone, not on the pooled
#    sample used for Table 2.
native_wage_model <- fixest::feols(
  log_wage_num ~ 1 | year_int + metro_area + education_chr + age_int +
    gender_chr + occ_code + ind_code,
  data = native_data,
  weights = ~weight_num
)

# 3) Equation (7): predict what each H-1B worker would earn as a native.
h1b_data$log_wage_native_num <- stats::predict(
  native_wage_model, newdata = h1b_data
)

unpredicted_int <- sum(is.na(h1b_data$log_wage_native_num))

message(
  "H-1B petitions with no native counterfactual (unseen fixed-effect level): ",
  format(unpredicted_int, big.mark = ","),
  sprintf(" (%.2f%%)", 100 * unpredicted_int / nrow(h1b_data))
)

# 4) Payroll savings, then the seniority adjustment, then the footnote 33 trim.
h1b_savings <- h1b_data |>
  dplyr::filter(!is.na(log_wage_native_num)) |>
  dplyr::mutate(
    payroll_savings_raw_num = log_wage_native_num - log_wage_num,
    payroll_savings_num = payroll_savings_raw_num - seniority_adjustment_num,
    payroll_savings_trimmed_num = pmin(
      pmax(payroll_savings_num, savings_bottom_code_num), savings_top_code_num
    )
  )

message("")
message("=== Payroll savings distribution (Figure 1) ===")
message(sprintf(
  "  Mean before the seniority adjustment: %.3f   (Borjas 0.156)",
  mean(h1b_savings$payroll_savings_raw_num)
))
message(sprintf(
  "  Mean after the seniority adjustment:  %.3f   (Borjas 0.120)",
  mean(h1b_savings$payroll_savings_num)
))
message(sprintf(
  "  Share with positive payroll savings:  %.1f%%   (Borjas 68.3%%)",
  100 * mean(h1b_savings$payroll_savings_num > 0)
))

################################################################################
###                 4. Simulation engine over the fee grid                   ###
################################################################################

wage_vector_num <- h1b_savings$wage_real_num
savings_vector_num <- h1b_savings$payroll_savings_trimmed_num
n_workers_int <- length(wage_vector_num)

# 5) Tenure probabilities are geometric in the separation rate: the last cell
#    collects everyone who stays the full term.
tenure_probability_num <- c(
  separation_rate_num * (1 - separation_rate_num)^(0:(visa_term_int - 2L)),
  (1 - separation_rate_num)^(visa_term_int - 1L)
)

tenure_discount_num <- vapply(
  1:visa_term_int,
  function(tenure_int) sum(1 / (1 + discount_rate_num)^(0:(tenure_int - 1L))),
  numeric(1)
)

tenure_growth_discount_num <- vapply(
  1:visa_term_int,
  function(tenure_int) {
    sum(((1 + wage_growth_num) / (1 + discount_rate_num))^(0:(tenure_int - 1L)))
  },
  numeric(1)
)

# 6) One replication draws pi from a normal truncated above at the truncation
#    point, then solves the hiring rule for the largest fee each worker can bear.
#    Sorting those reservation fees turns the whole fee curve into a lookup.
simulation_results <- NULL

for (scenario_label in c("baseline", "growth_and_separations")) {
  for (mu_pi_num in mu_pi_values) {

    hiring_rate_matrix <- matrix(
      NA_real_, nrow = n_replications_int, ncol = length(fee_grid_num)
    )

    for (replication_index in seq_len(n_replications_int)) {

      if (scenario_label == "baseline") {
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

      # Inverse-CDF draw from N(mu, sigma) truncated above at the truncation point
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

      reservation_fee_sorted_num <- sort(reservation_fee_num)

      hiring_rate_matrix[replication_index, ] <- 1 -
        findInterval(fee_grid_num, reservation_fee_sorted_num) / n_workers_int
    }

    simulation_results <- dplyr::bind_rows(
      simulation_results,
      tibble::tibble(
        scenario = scenario_label,
        mu_pi_num = mu_pi_num,
        fee_num = fee_grid_num,
        hiring_rate_num = colMeans(hiring_rate_matrix)
      )
    )

    message(
      "Simulated ", scenario_label, ", mu_pi = ", sprintf("%+.1f", mu_pi_num)
    )
  }
}

# 7) The degenerate case used in the top panel of Figure 2, where pi collapses to
#    zero for every worker and no truncation applies.
degenerate_reservation_num <- wage_vector_num * discount_factor_num *
  (exp(savings_vector_num) - 1)

degenerate_curve <- tibble::tibble(
  scenario = "degenerate",
  mu_pi_num = NA_real_,
  fee_num = fee_grid_num,
  hiring_rate_num = 1 -
    findInterval(fee_grid_num, sort(degenerate_reservation_num)) / n_workers_int
)

simulation_results <- dplyr::bind_rows(simulation_results, degenerate_curve)

message("")
message("=== Figure 2 benchmarks, degenerate pi = 0 ===")
message(sprintf(
  "  Hiring rate at F = 0:        %.1f%%   (Borjas 68%%)",
  100 * degenerate_curve$hiring_rate_num[degenerate_curve$fee_num == 0]
))
message(sprintf(
  "  Hiring rate at F = $100,000: %.1f%%   (Borjas 45%%)",
  100 * degenerate_curve$hiring_rate_num[degenerate_curve$fee_num == 100000]
))

hiring_at_100k <- simulation_results |>
  dplyr::filter(scenario == "baseline", fee_num == 100000)

message(sprintf(
  "  Hiring rate at $100,000 across mu: %.1f%% to %.1f%%   (Borjas 45%% to 64%%)",
  100 * min(hiring_at_100k$hiring_rate_num),
  100 * max(hiring_at_100k$hiring_rate_num)
))

################################################################################
###             5. Visas demanded and the revenue-maximising fee             ###
################################################################################

# 8) Equation (9) caps demand at the 85,000 statutory limit.
revenue_curves <- tidyr::expand_grid(
  simulation_results |> dplyr::filter(scenario != "degenerate"),
  registrations_int = excess_demand_values
) |>
  dplyr::mutate(
    visas_demanded_num = pmin(hiring_rate_num * registrations_int, visa_cap_int),
    revenue_num = fee_num * visas_demanded_num
  )

fee_tables <- revenue_curves |>
  dplyr::group_by(scenario, registrations_int, mu_pi_num) |>
  dplyr::slice_max(revenue_num, n = 1L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    scenario, registrations_int, mu_pi_num,
    revenue_maximising_fee_1000s = fee_num / 1000,
    hiring_rate_pct = 100 * hiring_rate_num,
    visas_demanded_1000s = visas_demanded_num / 1000,
    total_revenue_billions = revenue_num / 1e9
  ) |>
  dplyr::arrange(scenario, registrations_int, dplyr::desc(mu_pi_num))

borjas_table_eight <- tibble::tribble(
  ~registrations_int, ~mu_pi_num, ~fee_borjas, ~hiring_borjas, ~visas_borjas, ~revenue_borjas,
  170000L, -0.1, 164.0, 45.3, 77.1, 12.6,
  170000L,  0.0, 140.0, 41.8, 71.1, 10.0,
  170000L,  0.1, 117.0, 39.0, 66.3,  7.8,
  255000L, -0.1, 210.0, 33.3, 84.9, 17.8,
  255000L,  0.0, 170.0, 33.4, 85.0, 14.5,
  255000L,  0.1, 135.0, 33.3, 84.9, 11.5,
  340000L, -0.1, 246.0, 25.1, 85.0, 20.9,
  340000L,  0.0, 204.0, 25.1, 85.0, 17.3,
  340000L,  0.1, 166.0, 24.9, 84.8, 14.1
) |> dplyr::mutate(scenario = "baseline")

borjas_table_nine <- tibble::tribble(
  ~registrations_int, ~mu_pi_num, ~fee_borjas, ~hiring_borjas, ~visas_borjas, ~revenue_borjas,
  170000L, -0.1, 136.0, 37.5, 63.7,  8.7,
  170000L,  0.0, 115.0, 35.0, 59.4,  6.8,
  170000L,  0.1,  95.0, 33.2, 56.4,  5.4,
  255000L, -0.1, 152.0, 33.2, 84.8, 12.9,
  255000L,  0.0, 121.0, 33.2, 84.6, 10.2,
  255000L,  0.1,  95.0, 33.2, 84.7,  8.0,
  340000L, -0.1, 188.0, 25.0, 85.0, 16.0,
  340000L,  0.0, 153.0, 24.9, 84.7, 13.0,
  340000L,  0.1, 122.0, 24.9, 84.8, 10.3
) |> dplyr::mutate(scenario = "growth_and_separations")

fee_tables_compared <- fee_tables |>
  dplyr::left_join(
    dplyr::bind_rows(borjas_table_eight, borjas_table_nine),
    by = c("scenario", "registrations_int", "mu_pi_num")
  ) |>
  dplyr::mutate(fee_difference_1000s = revenue_maximising_fee_1000s - fee_borjas)

for (scenario_current in c("baseline", "growth_and_separations")) {

  table_label <- dplyr::if_else(
    scenario_current == "baseline",
    "Table 8. Impact of a visa fee on the demand for H-1B workers",
    "Table 9. Impact of a visa fee, allowing for wage growth and job separations"
  )

  message("")
  message("================ ", table_label, " ================")
  message("  replication   [Borjas]")

  scenario_rows <- fee_tables_compared |>
    dplyr::filter(scenario == scenario_current)

  for (registrations_current in excess_demand_values) {
    panel_rows <- scenario_rows |>
      dplyr::filter(registrations_int == registrations_current)
    message("")
    message(
      "  N = ", format(registrations_current, big.mark = ","),
      "   (mu = -0.1, 0.0, +0.1)"
    )
    message(sprintf(
      "    Revenue-maximising fee ($1000s)  %s   [%s]",
      paste(sprintf("%7.1f", rev(panel_rows$revenue_maximising_fee_1000s)), collapse = " "),
      paste(sprintf("%7.1f", rev(panel_rows$fee_borjas)), collapse = " ")
    ))
    message(sprintf(
      "    Hiring rate (%%)                  %s   [%s]",
      paste(sprintf("%7.1f", rev(panel_rows$hiring_rate_pct)), collapse = " "),
      paste(sprintf("%7.1f", rev(panel_rows$hiring_borjas)), collapse = " ")
    ))
    message(sprintf(
      "    Visas demanded (1000s)           %s   [%s]",
      paste(sprintf("%7.1f", rev(panel_rows$visas_demanded_1000s)), collapse = " "),
      paste(sprintf("%7.1f", rev(panel_rows$visas_borjas)), collapse = " ")
    ))
    message(sprintf(
      "    Total revenue ($bn)              %s   [%s]",
      paste(sprintf("%7.1f", rev(panel_rows$total_revenue_billions)), collapse = " "),
      paste(sprintf("%7.1f", rev(panel_rows$revenue_borjas)), collapse = " ")
    ))
  }
}

################################################################################
###                              6. Save outputs                             ###
################################################################################

readr::write_csv(
  fee_tables_compared, file.path(path_output, "borjas_august_tables8_9.csv")
)

readr::write_csv(
  simulation_results |> dplyr::filter(fee_num %% 5000 == 0),
  file.path(path_output, "borjas_august_hiring_rate_curves.csv")
)

saveRDS(
  h1b_savings |>
    dplyr::select(
      petition_key_chr,
      payroll_savings_raw_num, payroll_savings_num, payroll_savings_trimmed_num,
      wage_real_num, beneficiary_location, prior_status_group, year_int
    ),
  file.path(path_processed, "borjas_august_payroll_savings.rds")
)

message("")
message("Saved Tables 8 and 9 to output/tables/borjas_august_tables8_9.csv")

################################################################################
###          7. Two further Figure 2 checkpoints stated in the text          ###
################################################################################

# 9) The paper states that at low excess demand all 85,000 visas are still used
#    for fees "as large as $87,000", and up to $147,000 if the average H-1B
#    embodies a 10 percent productivity advantage. The first figure is the worst
#    case across the three scenarios (mu = +0.1), not the mu = 0 case.
cap_thresholds <- revenue_curves |>
  dplyr::filter(
    scenario == "baseline", registrations_int == 170000L,
    visas_demanded_num >= visa_cap_int - 1
  ) |>
  dplyr::group_by(mu_pi_num) |>
  dplyr::summarise(highest_fee_at_cap_num = max(fee_num), .groups = "drop")

message("")
message("=== Highest fee at which all 85,000 visas are still used (N = 170,000) ===")
for (row_index in seq_len(nrow(cap_thresholds))) {
  message(sprintf(
    "  mu = %+.1f : $%s", cap_thresholds$mu_pi_num[row_index],
    formatC(cap_thresholds$highest_fee_at_cap_num[row_index],
            big.mark = ",", format = "d")
  ))
}
message("  Borjas: $87,000 worst case (mu = +0.1); $147,000 at mu = -0.1")

################################################################################
###                        8. Figures 1 and 2                                ###
################################################################################

source(file.path(path_style, "themes", "r", "eig_theme.R"))
eig_tokens <- eig_load_tokens(
  file.path(path_style, "themes", "r", "eig_tokens.R")
)
eig_assert_fonts(tokens = eig_tokens, allow_fallback = TRUE)

color_teal_900 <- eig_tokens$EIG_COLORS[["eig_teal_900"]]
color_blue_800 <- eig_tokens$EIG_COLORS[["eig_blue_800"]]
color_tan_500 <- eig_tokens$EIG_COLORS[["eig_tan_500"]]

source_line_chr <- paste0(
  "Source: Author's replication of Borjas, \"The H-1B Wage Gap, Visa Fees, and ",
  "Employer Demand\", NBER Working Paper 34793, revised August 2026."
)

# 10) Figure 1: the distribution of payroll savings after the seniority adjustment
figure_one <- ggplot2::ggplot(
  h1b_savings, ggplot2::aes(x = payroll_savings_num)
) +
  ggplot2::geom_histogram(
    binwidth = 0.02, fill = color_blue_800, colour = NA, alpha = 0.9
  ) +
  ggplot2::geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.4) +
  ggplot2::geom_vline(
    xintercept = mean(h1b_savings$payroll_savings_num),
    colour = color_tan_500, linewidth = 0.8, linetype = "dashed"
  ) +
  ggplot2::annotate(
    "text", x = mean(h1b_savings$payroll_savings_num), y = Inf,
    label = sprintf("  mean %.3f", mean(h1b_savings$payroll_savings_num)),
    hjust = 0, vjust = 2, size = 2.9, colour = color_tan_500, fontface = "bold"
  ) +
  ggplot2::scale_x_continuous(limits = c(-0.9, 0.9)) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    x = "Payroll savings from hiring an H-1B worker (log points)",
    y = "H-1B petitions",
    caption = paste0(
      "Figure 1. Distribution of estimated payroll savings across the H-1B ",
      "workforce\n\n",
      "Note: Payroll savings are the Oaxaca-Blinder gap between the wage a ",
      "comparable native would earn, predicted from a wage\n",
      "regression estimated on natives alone, and the wage on the I-129 ",
      "petition. Shifted left by 3.6 log points for the job\n",
      "seniority bias, as Borjas does. ",
      sprintf(
        "Mean %.3f and %.1f percent positive, against 0.120 and 68.3 percent.\n",
        mean(h1b_savings$payroll_savings_num),
        100 * mean(h1b_savings$payroll_savings_num > 0)
      ),
      source_line_chr
    )
  ) +
  eig_theme_ggplot(tokens = eig_tokens) +
  ggplot2::theme(
    plot.caption = ggplot2::element_text(
      size = 6.4, colour = "#444444", hjust = 0, lineheight = 1.25
    ),
    plot.caption.position = "plot"
  )

ggplot2::ggsave(
  file.path(path_figures, "fig01_borjas_payroll_savings.png"),
  figure_one, width = 6.5, height = 4.2, dpi = 300, bg = "white"
)

message("Saved fig01_borjas_payroll_savings.png")

# 11) Figure 2: hiring rate, visas demanded and revenue against the fee, at the
#     low excess demand scenario used in the paper's discussion
scenario_colors <- c(
  "No cost differential" = "grey45",
  "mean cost differential -0.1" = color_blue_800,
  "mean cost differential 0.0" = color_teal_900,
  "mean cost differential +0.1" = color_tan_500
)

figure_two_data <- dplyr::bind_rows(
  simulation_results |>
    dplyr::filter(scenario == "degenerate") |>
    dplyr::mutate(series = "No cost differential"),
  simulation_results |>
    dplyr::filter(scenario == "baseline") |>
    dplyr::mutate(series = paste0(
      "mean cost differential ", sprintf("%+.1f", mu_pi_num)
    ))
) |>
  dplyr::mutate(
    series = dplyr::recode(series, "mean cost differential +0.0" = "mean cost differential 0.0"),
    series = factor(series, levels = names(scenario_colors)),
    visas_demanded_num = pmin(hiring_rate_num * 170000, visa_cap_int),
    revenue_billions_num = fee_num * visas_demanded_num / 1e9
  )

panel_hiring <- ggplot2::ggplot(
  figure_two_data,
  ggplot2::aes(x = fee_num / 1000, y = 100 * hiring_rate_num, colour = series)
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::scale_colour_manual(values = scenario_colors, name = NULL) +
  ggplot2::scale_x_continuous(limits = c(0, 300), labels = scales::dollar) +
  ggplot2::labs(x = NULL, y = "Hiring rate (%)", subtitle = "A. Hiring rate") +
  eig_theme_ggplot(tokens = eig_tokens) +
  ggplot2::theme(legend.position = "top", legend.text = ggplot2::element_text(size = 7))

panel_visas <- ggplot2::ggplot(
  figure_two_data |> dplyr::filter(series != "No cost differential"),
  ggplot2::aes(x = fee_num / 1000, y = visas_demanded_num / 1000, colour = series)
) +
  ggplot2::geom_hline(yintercept = 85, colour = "grey60", linetype = "dashed", linewidth = 0.4) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::scale_colour_manual(values = scenario_colors, guide = "none") +
  ggplot2::scale_x_continuous(limits = c(0, 300), labels = scales::dollar) +
  ggplot2::labs(
    x = NULL, y = "Visas demanded (1000s)",
    subtitle = "B. Visas demanded, low excess demand (N = 170,000)"
  ) +
  eig_theme_ggplot(tokens = eig_tokens)

panel_revenue <- ggplot2::ggplot(
  figure_two_data |> dplyr::filter(series != "No cost differential"),
  ggplot2::aes(x = fee_num / 1000, y = revenue_billions_num, colour = series)
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::scale_colour_manual(values = scenario_colors, guide = "none") +
  ggplot2::scale_x_continuous(limits = c(0, 300), labels = scales::dollar) +
  ggplot2::labs(
    x = "One-time visa fee ($1000s)", y = "Revenue ($bn)",
    subtitle = "C. Government revenue, low excess demand"
  ) +
  eig_theme_ggplot(tokens = eig_tokens)

figure_two <- patchwork::wrap_plots(
  panel_hiring, panel_visas, panel_revenue, ncol = 1L
) +
  patchwork::plot_annotation(
    caption = paste0(
      "Figure 2. Simulated effect of a one-time visa fee on hiring, visas ",
      "demanded and revenue\n\n",
      "Note: Hiring rates are averages over 100 replications. The unobserved ",
      "cost differential is normal with 99 percent of its mass in\n",
      "[-0.5, 0.5], truncated above at each worker's payroll savings. Panels B ",
      "and C assume 170,000 lottery registrations and cap\n",
      "visas at 85,000. A 3 percent discount rate is used over the six-year ",
      "visa term.\n",
      source_line_chr
    ),
    theme = ggplot2::theme(
      plot.caption = ggplot2::element_text(
        size = 6.4, colour = "#444444", hjust = 0, lineheight = 1.25
      ),
      plot.caption.position = "plot"
    )
  )

ggplot2::ggsave(
  file.path(path_figures, "fig02_borjas_fee_simulation.png"),
  figure_two, width = 6.5, height = 8.2, dpi = 300, bg = "white"
)

message("Saved fig02_borjas_fee_simulation.png")
