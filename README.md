# EIG public comment on the proposed H-1B fee
This code underlies EIG's comment letter to the Department of Homeland Security in response to its [proposed](https://www.federalregister.gov/documents/2026/08/25/2026-17324/fee-for-certain-h-1b-petitions) $103,265 fee on all cap-subject H-1B petitions. Contact jiaxin@eig.org and sam@eig.org with any questions.

---

## How to run it

```bash
Rscript code/run_all.R
```

Toggle individual steps with the `TRUE`/`FALSE` flags at the top of `run_all.R`.

**Requirements**

- R 4.6 or later.
- Packages: `tidyverse`, `data.table`, `fixest`, `arrow`, `ipumsr`, `fredr`, `tidycensus`, `readxl`, `Hmisc`, and optionally `here`. `run_all.R` checks for all of them before running anything.
- A free FRED API key from <https://fredaccount.stlouisfed.org/apikeys>, set in `~/.Renviron` as `FRED_API_KEY=your_key_here`. Script 04 fetches monthly CPI-U at runtime and stops with instructions if the key is missing.

**Order matters.** Scripts 01–03 are independent of everything else. Scripts 04–08 are a chain: each reads a checkpoint written by the one before it, so they cannot be run out of order from a clean state. The file names encode the order.

---

## What each script does

### The DHS Technical Appendix (scripts 01–03)

These read the seven tables transcribed from the appendix into `data/raw/DHS Technical Appendix/`. They need no microdata and run in under a minute.

**`01_appendix_audit.R` — checks the appendix's arithmetic.**
Recomputes 14 numerical claims from the appendix's own published tables. The finding the letter uses: footnote 8 states `(20,799/398,281) = 5.07 percent`, but that ratio is 0.0522,
i.e. 5.22 percent — which is the figure the appendix's own body text uses two sentences later.
→ `output/tables/appendix_audit_findings.csv`

**`02_mechanical_identity_sim.R` — shows the fee elasticity is an identity, not a result.**
DHS regresses each firm's application volume on the total fees that firm paid. But total fees are just the fee schedule multiplied by volume, so the regressor contains the
dependent variable. This script simulates data with a *known* true elasticity and shows DHS's cross-sectional specification returns a coefficient near +1 regardless of the truth: across true elasticities of 0, −0.5 and −2.0 the estimate lands between **+0.966 and +1.017**. A variance decomposition shows 90 percent of the variation in log total fees is the dependent variable itself. (The panel two-way fixed-effects variant is less degenerate, 0.690 to 1.208, and the table flags those rows as falling outside DHS's reported range.)
→ `output/tables/mechanical_identity_results.csv`, `mechanical_identity_summary.csv`,
`regressor_variance_decomposition.csv`

**`03_implied_elasticity.R` — shows what DHS's own parameter predicts.**
Applies DHS's estimated elasticity to the 2025 proclamation's $100,000 consular fee and compares the prediction with what actually happened: a 91 percent collapse in consular receipts. Also records how far the $460 → $1,380 fee change is being extrapolated to reach $103,265.
→ `output/tables/implied_elasticity.csv`, `dhs_parameter_prediction_vs_observed.csv`,
`extrapolation_range.csv`

### Replicating and testing Borjas (scripts 04–05)

**`04_borjas_replication.R` — replicates Table 2, then stress-tests it.**
Builds the pooled sample: approved I-129 H-1B petitions FY2021–24 against U.S.-born ACS workers aged 21–50 with a college degree, working full-time year-round in the private sector. Wages go to 2025 dollars on monthly CPI-U over each worker's actual earnings window. Estimates equation (1) with year, place-of-work metro, education, age, gender, occupation and industry fixed effects.

*Replication check:* column 5 reproduces at **−0.158** against Borjas's reported −0.161; all five columns are within 0.008.

The same script then runs the critique: an alternative place-of-work PUMA geography, broad occupation and industry cells interacted with place-of-work state, clustered standard errors, the education-imputation tests, and the age profile of the gap.
→ `output/tables/borjas_august_table2.csv`, `critique_*.csv`
→ checkpoint `data/processed/borjas_august_regression_data.rds`

**`05_borjas_labor_demand.R` — replicates his fee model.**
Estimates the native wage equation, predicts what each H-1B worker would earn as a native (Oaxaca–Blinder), and derives the payroll saving. Then applies his hiring rule: a firm pays fee *F* for worker *h* only while *F* stays under that worker's reservation fee. Reproduces his Tables 8 and 9 revenue-maximising fees to within $1–2k.
→ `output/tables/borjas_august_tables8_9.csv`, `borjas_august_hiring_rate_curves.csv`
→ checkpoint `data/processed/borjas_august_payroll_savings.rds`

### What the new rules do (scripts 06–08)

**`06_allocation_simulation.R` — the wage gap under four allocation regimes.**
Builds a synthetic registrant pool from FY2022–24 petitions, scaled to observed registration volumes, then allocates 85,000 visas four ways and re-estimates the wage gap on the winners each time: the old unweighted lottery, the weighted lottery on old OFLC wage levels, the weighted lottery on the proposed new wage levels, and EIG's lifetime-earnings ranking proposal. 100 replications, drawn in five batches of 20 to stay within memory.
→ `output/tables/FY2022-24 Borjas FE Model Simulated Coefficients.csv`
→ checkpoint `data/processed/borjas_allocation_selection_counts.rds`

**`07_fee_under_reformed_rules.R` — prices the $103,265 fee against each regime.**
Takes the winners each regime produces and asks how many employers would actually pay. Reports the six-year payroll saving per winner, the share willing to pay, FY2027 volumes with the fee falling only on within-U.S. adjustments (applicants from abroad already paid the 2025 proclamation's $100,000), and Borjas's own revenue-maximising fee under each regime.
→ `output/tables/fee_regime_payroll_savings.csv`, `fee_fy2027_by_allocation_regime.csv`,
`fee_regime_revenue_maximising.csv`

**`08_fee_incidence_by_quartile.R` — who the fee removes.**
Splits the within-U.S. cohort into payroll-savings quartiles and wage quartiles and reports the expected application reduction in each, plus the median wage, the F-1 share, and the average age before and after the fee.
→ `output/tables/fee_incidence_by_savings_quartile.csv`, `fee_incidence_f1_and_age_shift.csv`,
`output/tables/datawrapper/*.csv`

---

## How the code works

### Layout

```
code/               nine R scripts: run_all.R plus 01-08 in execution order
data/raw/           inputs as received (not distributed; see Data below)
data/processed/     cleaned inputs and the checkpoints scripts pass between each other
output/tables/      every published number, as CSV
output/tables/datawrapper/   chart-ready exports
output/figures/     replication-check figures
output/logs/        one log per pipeline run
```

Nothing is written outside `data/processed`, `output/`, and the run log. No script reads from or writes to any location outside the repository, and none calls `setwd()`.

### Data flow

Scripts 01–03 are self-contained: they read the seven DHS appendix tables and write their own outputs. Scripts 04–08 form a chain in which each step writes a checkpoint the next one reads.

```
                        data/raw/DHS Technical Appendix/
                                      |
                         01  ---  02  ---  03          (independent of everything else)


  data/raw/ACS + FOIA registrations + I-129 + crosswalks + FRED
                                      |
                                     04  --->  borjas_august_regression_data.rds
                                      |              |            |
                                      v              |            |
                                     05  --->  borjas_august_payroll_savings.rds
                                      |              |            |
        OFLC wage levels ---------->  06  --->  borjas_allocation_selection_counts.rds
                                      |              |            |    borjas_allocation_base_sample.rds
                        +-------------+--------------+            |
                        v                                          v
                       07                                         08
```

Concretely:

| Script | Reads | Writes (checkpoint) |
|---|---|---|
| 04 | ACS extract, I-129 petitions, geographic crosswalks, FRED | `borjas_august_regression_data.rds` |
| 05 | 04's checkpoint | `borjas_august_payroll_savings.rds` |
| 06 | 04's checkpoint, OFLC wage levels, registration totals | `borjas_allocation_selection_counts.rds`, `borjas_allocation_base_sample.rds` |
| 07 | 05's and 06's checkpoints | `fee_fy2027_by_allocation_regime.rds` |
| 08 | 04's, 05's, and 06's checkpoints | `fee_incidence_by_savings_quartile.rds` |

Every checkpoint is written twice, as `.rds` for exact R types and as `.parquet` for anyone working outside R. The two files always hold the same rows in the same order.

### Running part of the pipeline

`run_all.R` has one `TRUE`/`FALSE` flag per script at the top. Set the ones you do not want to `FALSE`. The chain constraint is the only rule: because 05, 06, 07, and 08 read checkpoints, a checkpoint must either already exist on disk or be produced earlier in the same run. Re-running 07 alone after editing it is fine; re-running 07 alone after editing 04 is not, because 04's checkpoint would be stale.

Each script is sourced into a fresh environment, so nothing leaks between steps, and the runner halts the moment a script fails rather than carrying a broken checkpoint downstream. Every script also runs correctly on its own with `Rscript code/<name>.R`, provided its inputs exist.

### Conventions

Every script opens with `rm(list = ls())`, `options(scipen = 999)`, and `set.seed(42)`, in that order, so that running it directly is reproducible. 

Section banners are numbered in processing order, and inline `# n)` comments mark each step.

The project root is resolved with `here::here()`, falling back to `getwd()` with a warning. Hard code your local path as needed.

### Six implementation details worth knowing

These are the places where the obvious implementation does not work, and a reader comparing the code to the methodology will want to know why.

**1. Joining two different cleanings of the same FOIA release.**
The recomputed OFLC wage levels live in a file whose `applicant_id` is a fresh 1..N sequence, so it cannot be joined to the petition file on the identifier. Script 04 therefore writes a composite key — lottery year, LCA case number stripped of punctuation, petition receive date, petition decision date, annual pay, and sex — which script 06 joins on. It matches 98.6 percent of records, and the merge is validated on beneficiary age, a field deliberately excluded from the key: it agrees on 100.000 percent of matched rows. `SOC_CODE` cannot be used in the key, because the two cleanings code occupation differently and agreement falls to 55 percent.

**2. Hierarchical crosswalks instead of exact matching.**
The ACS aggregates some occupation and industry categories and flags them with letters (`1191XX`, `3399ZM`, `52M1`). Stripping the letters and demanding a full-length match discards about a fifth of the H-1B sample. Script 04 instead reduces each ACS code to its leading numeric stem and matches on the longest stem that prefixes the I-129 code, keeping the modal ACS category by person weight at each level. Unmatched occupations fall from 62,970 to one.

**3. ZIP codes to place-of-work PUMAs.**
The I-129 reports a worksite ZIP, not a PUMA. Script 04 maps each ZIP to the 2020 PUMA holding the largest share of its population, then to the place-of-work PUMA containing it. ZIPs that do not resolve exactly — mostly single-building Manhattan ZIPs with no ZCTA of their own — fall back to the modal PUMA among ZIPs sharing a four-, then three-digit prefix. Coverage reaches 99.98 percent without reading the 1.2 GB of block-level crosswalks an exact method would require.

**4. Weighted sampling without replacement.** The weighted lottery draws 65,000 winners from
a pool of several hundred thousand with selection probability proportional to wage level.
R's `sample(prob =, replace = FALSE)` does this by successive sampling, which is O(*nk*) —
roughly 3 × 10¹⁰ operations per draw at these sizes, and it does not finish. Script 06 uses
the Efraimidis–Spirakis key method instead: draw *U*ᵢ uniform, compute log(*U*ᵢ)/*w*ᵢ, and
take the *k* largest. This is distributionally identical and O(*n* log *n*). Verified against
`sample(prob =)` over 200,000 replications: inclusion probabilities agree to within 0.002.

**5. The reservation fee is solved, not searched.** Borjas's hiring rule is an inequality in
the fee. Rather than evaluating it on a grid, scripts 05, 07, and 08 solve it analytically
for the largest fee each worker can bear, then sort those reservation fees. The share of
employers willing to pay any fee is then a single lookup into the sorted vector, so the whole
fee-response curve costs one sort rather than one regression per grid point.

**6. The simulation runs in batches.** At 100 replications the synthetic registrant pool
would be about 141 million rows. Script 06 draws it in five batches of 20, accumulating
results and per-petition selection counts across batches. The draws are statistically
identical to drawing all 100 at once; peak memory stays near 2 GB instead of exceeding 10 GB.
`sim_batch_size_int` at the top of the script controls this.

---

## Every claim in the letter, and where to check it

| Claim in the letter | Value | Script | File |
|---|---|---|---|
| Borjas headline replicates | −0.158 vs his −0.161 | 04 | `borjas_august_table2.csv` |
| Within-U.S. gap 11.8%, abroad 21.9% | Borjas's own reported values | 04 | `borjas_august_table2.csv`, `borjas_reported_num` |
| 19.1% of ACS entries have no metro of work | 19.058% | 04 | `critique_metro_coverage.csv` |
| 5–15% allocation error | 17.620% of all entries | 04 | `critique_metro_coverage.csv` |
| Wage premiums for the young, deficits for the old | +19.3% at age 22, −33.4% at 40 | 04 | `critique_age_profile.csv` |
| Weighted lottery alone cuts the gap to 9.3% | −9.27% | 06 | `...Simulated Coefficients.csv` |
| Weighted lottery + new wage levels → 10.5% premium | +10.45% | 06 | `...Simulated Coefficients.csv` |
| Average six-year wage saving −$80,700 | −$80,749 | 07 | `fee_regime_payroll_savings.csv` |
| 1,600-visa deficit, $164m revenue short | 1,588 visas, $163.9m | 07 | `fee_fy2027_by_allocation_regime.csv` |
| 9,300-visa shortfall, nearly $1bn short | 9,265 visas, $956.7m | 07 | `fee_fy2027_by_allocation_regime.csv` |
| 27,000-visa shortfall, $2.8bn short | 27,000 visas, $2.788bn | 07 | `fee_fy2027_by_allocation_regime.csv` |
| 58,000 fee-paying H-1B workers | 58,000 | 07 | `fee_fy2027_by_allocation_regime.csv` |
| Bottom savings quartile loses nearly 90% | 86.7% | 08 | `datawrapper/..._datawrapper.csv` |
| Top savings quartile loses just 15% | 14.6% | 08 | `datawrapper/..._datawrapper.csv` |
| 91% consular decline | −91.2% | 03 | `dhs_parameter_prediction_vs_observed.csv` |
| Coefficient "very close to 1" | +0.966 to +1.017 (cross-section) | 02 | `mechanical_identity_results.csv` |
| Footnote 8: 5.07 should be 5.22 percent | 0.0522 | 01 | `appendix_audit_findings.csv` |

Numbers cited to Borjas or to DHS are *their* published values, carried in these files as
comparison columns, not estimates produced here.

### The three charts

The published charts were built in Datawrapper from these CSVs, not in R.

| Chart | Source |
|---|---|
| p.3 — application reduction by payroll-savings quartile | `08` → `datawrapper/fee_application_reduction_by_savings_quartile_datawrapper.csv` |
| p.7 — wage gap by allocation regime | `06` → `FY2022-24 Borjas FE Model Simulated Coefficients.csv` |
| p.8 — six-year saving, visas filled, revenue shortfall | `07` → `fee_regime_payroll_savings.csv`, `fee_fy2027_by_allocation_regime.csv` |

---

## Data

Raw inputs are **not** distributed with this repository. Every source is obtainable; two
require a request or a free account.

| Input | Source | Access |
|---|---|---|
| I-129 H-1B petition microdata FY2021–24 | Bloomberg Law FOIA release | restricted; USCIS applied (b)(3), (b)(6), (b)(7)(c) exemptions |
| H-1B registration files `TRK_13139_*` | USCIS FOIA | restricted |
| ACS 2021–2024 1-year, extract `usa_00108` | IPUMS USA, <https://usa.ipums.org> | free account; the `.xml` DDI lists the exact variables |
| DHS Technical Appendix tables A1–A7 | transcribed from the NPRM appendix | included, 28 KB |
| OFLC wage levels / LCA disclosure data | DOL OFLC | public |
| Regional price parities | BEA | derived file included |
| Geographic crosswalks (Geocorr 2022, ZIP–ZCTA, PUMA→POWPUMA) | MCDC, Census Bureau | public |
| CPI-U (`CPIAUCNS`) | FRED, fetched at runtime | free API key |

`data/processed/Old Cleaned FOIA - for Borjas Replication/` and
`FY2022-2024 I-129 Data with New Wage Levels.csv` were built by an earlier EIG project (He
and Ozimek, February 2026) from the I-129 and LCA sources above. This pipeline consumes
them directly and does not rebuild them.
