# Immigrant Health, Migration, and Mortality in a Nordic Welfare State

Replication and analysis code for:

> Paglino, E., Lyytinen, M. K. K., Tarkiainen, L., & Martikainen, P. *Immigrant Health, Migration, and Mortality in a Nordic Welfare State*.

An archival copy of the code is also available on OSF at [https://osf.io/jzy2p](https://osf.io/jzy2p).

## Overview

This repository contains the R scripts used to construct the analytical dataset, fit survival and sensitivity models, and generate all tables for our study of immigrant–native differentials in all-cause mortality and six chronic health conditions (arthritis, asthma, diabetes, heart disease, hypertension, and malignant neoplasms) in Finland between 2005 and 2019. It also includes the code for testing the salmon bias hypothesis by modeling return and secondary out-migration as a function of chronic disease diagnosis.

Because the underlying individual-level data cannot be shared publicly, the primary purpose of this repository is transparency: allowing readers and reviewers to inspect how register records are linked, how sample eligibility and covariates are defined, and how every model and table in the paper is produced.

## Data Availability & Access

No data are stored in this repository due to data protection regulations governing Finnish administrative registers. The analysis uses pseudonymized individual-level data maintained and linked under Statistics Finland permission **TK-53-1519-09**, combining:

1. **Statistics Finland (FOLK modules):** Annual sociodemographic records (`FOLK_perus`), household income and composition (`FOLK_askun`), international migration histories (`FOLK_MUUTT_C`), and retrospective socioeconomic status (`retroses.dta`).
2. **Finnish Institute for Health and Welfare (THL):** Care Register for Health Care (HILMO) specialized inpatient and outpatient diagnosis records (`u1259_hilmo_paa_*.dta`).
3. **Social Insurance Institution of Finland (Kela):** Special reimbursement rights for long-term prescription medications (`u1259_laakekorvausoikeud_korj_s.dta`).

Researchers interested in replicating the study can apply for access to the register data through [Statistics Finland](https://stat.fi/tup/mikroaineistot/index_en.html) (remote access environment, Fiona).

## Expected Directory Structure

The scripts use the `here` package with `here::i_am("R/<script>.R")` and expect the following project directory layout alongside the external register directories on the Statistics Finland remote access system:

project_root/
├── R/                  # All R scripts in this repository (001a_*.R – 210_*.R)
├── data/               # Local auxiliary input files (not included in repo)
│   ├── country_codes.txt   # Lookup table mapping Statistics Finland country codes to names & regions (columns: code, country, group)
│   └── retroses.dta        # Retrospective occupational/SES register file (columns: shnro, vuosi, sesx)
├── output/             # Intermediate .arrow (Feather) datasets and model coefficient .csv files (created by scripts)
├── figures/            # Exploratory and diagnostic .svg plots (created by scripts)
├── tables/             # Formatted .docx tables for the manuscript and supplement (created by scripts)
└── LICENSE

### External Register Data Paths

Scripts `001a`–`009` read raw Stata (`.dta`) files in chunks from the following external directories (reflecting the Statistics Finland remote access environment; update `in_dir` at the top of each script if your paths differ):

| Script(s) | Default `in_dir` / Source Path | Files Read |
| --- | --- | --- |
| `001a`, `001b` | `W:/Non-continuously updated folk modules/FOLK_MUUTT_C` | International migration records (`*.dta`) |
| `002a`, `002b` | `D:/ready-made/FOLK_perus_8800a` | FOLK basic module, 1988–2000 (latest `*.dta`) |
| `004` | `D:/d31/external/u1259_al5_thl_aineisto_hilmot` | THL HILMO visits (`u1259_hilmo_paa_*.dta`, excl. `1970_1986`) |
| `006` | `D:/d31/external/u1259_al5_kela_aineisto` | Kela medication reimbursements (`u1259_laakekorvausoikeud_korj_s.dta`) |
| `008` | `D:/ready-made/FOLK_perus_0110a`, `FOLK_perus_11a` | FOLK basic modules, 2001–2010 and 2011+ (latest `*.dta`) |
| `009` | `D:/ready-made/FOLK_askun_0110a`, `FOLK_askun_11a` | FOLK household modules, 2001–2010 and 2011+ (latest `*.dta`) |
| `010` | `data/retroses.dta` | Retrospective SES file |

## Software Requirements

The analysis was conducted in **R**. The scripts rely on the following CRAN packages:

* **Data manipulation & I/O:** `data.table`, `tidyverse`, `arrow`, `haven`, `here`, `progress`, `plyr`
* **Statistical modeling:** `survival`, `mgcv`, `broom`, `msm`
* **Table generation:** `gt`, `gtsummary`

## Workflow & Script Guide

Scripts are numbered sequentially in three stages: data preparation (`001`–`012`), statistical modeling (`101`–`113`), and table/life-table generation (`201`–`210`).

### 1. Data Preparation (`001a`–`012`)

* `001a_read_mig_histories.R` / `001b_read_mig_histories_native.R`: Extract and clean international immigration and emigration histories through 2019 for foreign-born and Finnish-born residents, excluding individuals with >6 migration records.
* `002a_read_foreign_data.R` / `002b_read_native_data.R`: Extract baseline FOLK records (1988–2000) for foreign-born immigrants arriving in 1990–2000 and an initial sample of native-born Finns.
* `003_combine_data.R`: Combine foreign-born and native-born sample rosters into a single ID list.
* `004_read_healthcare_data.R` / `005_clean_visits.R`: Read THL HILMO specialized healthcare episodes, filter to sample IDs, and parse visit dates and ICD-9/ICD-10 codes. *(Note: `005_clean_visits_old.R` is a legacy CSV-based version retained for reference and is not part of the active pipeline.)*
* `006_read_med_reimbursements_data.R`: Read Kela special medication reimbursement rights (1990+) and map reimbursement codes to the study's chronic conditions.
* `007_create_health_longitudinal.R`: Combine HILMO diagnoses and Kela reimbursement rights to determine the earliest diagnosis date per individual and condition.
* `008_read_basic_data.R` / `009_read_household_data.R` / `010_read_ses_data.R`: Extract longitudinal demographic, household disposable income, and retrospective occupational SES data (2001–2019) for the study sample.
* `011_create_model_data.R`: Merge all register modules, apply study eligibility criteria (alive and residing in Finland on Dec 31, 2004; ages 25–84; 2004–2019), construct covariates (SES, equivalized/household income quintiles, employment status, regions of birth), and draw the final 200,000 native-born sample (`set.seed(42)`).
* `012_health_eligibility.R`: Identify condition-specific washout eligibility (no diagnosis or reimbursement record during 2000–2004).

### 2. Statistical Models (`101`–`113`)

* `101_mortality_models.R` *(and `101b_mortality_models_4564.R`, `101c_mortality_models_6584.R`)*: Baseline and SES-adjusted Cox proportional hazards models (plus exploratory negative binomial GAMs) for all-cause mortality by nativity and age group (25–44, 45–64, 65–84), run separately by sex. Scripts `101b` and `101c` re-level the reference age group to 45–64 and 65–84 to obtain direct age-specific hazard ratios and confidence intervals.
* `102_health_models.R` *(and `102b_health_models_4564.R`, `102c_health_models_6584.R`)*: Equivalent Cox and GAM models for first diagnosis of each chronic condition among disease-free individuals.
* `103_mortality_models_pob.R` / `104_health_models_pob.R`: Baseline and SES-adjusted Cox models for mortality and chronic conditions stratified by region of birth (Africa; Americas, Asia, and Oceania; Europe; Middle East; Russia; reference: Finland).
* `105_mortality_models_pob_ageing.R` / `106_health_models_pob_ageing.R`: Exploratory models interacting region of birth with age group (25–64 vs. 65+).
* `107_salmon_models.R` / `108_salmon_models_return.R` / `109_salmon_models_secondary.R`: Cox models among immigrants testing whether diagnosis of each chronic condition during follow-up predicts subsequent out-migration (all re-migration, return migration to country of birth, and secondary migration to a third country).
* `110_health_mortality_models.R`: Cox models for mortality interacting diagnosis status with immigrant status to evaluate potential healthcare underutilization and severity at diagnosis.
* `111_ses_stratified_models.R`: Mortality Cox models stratified by occupational status.
* `112_health_logit_models.R`: Logistic regression sensitivity analysis of condition prevalence (including washout-period diagnoses).
* `113_mortality_models_mig_cohort.R`: Mortality sensitivity analysis stratifying immigrants by arrival cohort (1990–1995 vs. 1996–2000).

### 3. Tables & Life Tables (`201`–`210`)

| Script | Inputs (`output/`) | Output (`tables/`) | Manuscript / Supplement Reference |
| --- | --- | --- | --- |
| `201_descriptive_stats.R` | `011_general_model_data.arrow` | `201_descriptive_stats.docx` | **Table 1** (Baseline sample characteristics, Dec 31, 2004) |
| `202_life_tables.R` | `011_general_model_data.arrow`, `012_health_eligibility.arrow` | `202_e25_85_by_year.docx`, `202_ex_lx.docx`, `202_survival_by_condition.docx` | **Tables E2 & E3** (Temporary/remaining life expectancy & survival probabilities) |
| `203_cox_main_tables.R` | `101_coef_table.csv`, `102_coef_table.csv` | `203_cox_main_table_female.docx`, `203_cox_main_table_male.docx` | **Tables 2 & 3** (Main Cox models, females & males) |
| `204_cox_tables_pob.R` | `103_coef_table.csv`, `104_coef_table.csv` | `204_cox_pob_table_female.docx`, `204_cox_pob_table_male.docx` | **Tables 4 & 5** (Place-of-birth Cox models, females & males) |
| `205_cox_tables_salmon.R` | `107_coef_table.csv`, `108_coef_table.csv`, `109_coef_table.csv` | `205_cox_salmon_table.docx` | **Table 6** (Re-migration / salmon bias models) |
| `206_cox_tables_healthcare.R` | `110_coef_table.csv` | `206_cox_healthcare_table.docx` | **Table S3** (Post-diagnosis mortality interaction models) |
| `207_age_coefs_tables.R` | `101`/`101b`/`101c` & `102`/`102b`/`102c` `coef_table.csv` | `207_age_coefs_tables.docx` | **Table S1** (Age-specific immigrant hazard ratios) |
| `208_cox_ses_stratified_tables.R` | `111_coef_table.csv` | `209_cox_ses_stratified_table.docx` | **Table S4** (Occupation-stratified mortality models) |
| `209_logit_tables.R` | `112_coef_table.csv` | `209_logit_table.docx` | Supplementary logistic regression check |
| `210_cox_tables_mig_cohort.R` | `113_coef_table.csv` | `210_cox_table_mig_cohort.docx` | **Table S2** (Arrival cohort sensitivity analysis) |

## License

See the `LICENSE` file in this repository for license details.