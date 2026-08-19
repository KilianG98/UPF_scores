# Beyond total ultra-processed food intake: consumption profiles and risk of cancer, cardiometabolic disease and mortality

**Author:** Kilian Gandolf
**Last edited:** 19.08.2026
**DOI:** *(to be added)*

This repository contains the code used for data cleaning, descriptive analyses, and the main statistical analyses for the paper.

---

## Data Preparation & Cleaning

**`1_Modifizierte_UPF_Subgroups_KR.do`**

**Stata script used to define the ultra-processed food (UPF) subgroups.**

**`2_dietscore.R`**

**Calculates dietary scores based on the baseline nutritional data.**

**`3_orc.R`**

**Generates the obesity-related cancers (ORC) variable.**

**`4_cleaning.R`**

**Performs the final data cleaning steps and prepares the analysis dataset.**
*   **Inputs:** `"UKB_data_all3.rds"` (Dataset with added variables for ORC, dietscore, and UPF subgroups).
*   **Outputs:** `"working_file3.rds"` (Cleaned dataset with exclusion criteria applied and unnecessary variables removed).

**`5_0_subgroups_main.R`**

**Performs residual energy adjustment of UPF subgroups and creates quartiles.**
*   **Inputs:** `"working_file3.rds"` (Cleaned dataset).
*   **Outputs:** `"working_file_w_SG3.rds"` (Main dataset with quartiles of energy-adjusted subgroups).

**`5_1_subgroups_crude.R`** *(for sensitivity analysis)*

**Generates quartiles without residual energy adjustment.**
*   **Inputs:** `"working_file3.rds"` (Cleaned dataset).
*   **Outputs:** `"working_file_w_SG3_crude.rds"` (Dataset containing quartiles of crude subgroups).

**`5_2_lag_time.R`** *(for sensitivity analysis)*

**Generates the file required for lag-time analysis.**
*   **Inputs:** `"working_file_w_SG3.rds"` (Main dataset).
*   **Outputs:** `"working_file_w_SG3_lag.rds"` (Dataset with lag time implemented).

---

## Analysis

**`6_main_analysis.R`**

**Performs the primary statistical analyses.**
*   **Inputs:** `"working_file_w_SG3.rds"` (Main dataset), `"X_analysis_functions.R"` (Functions used in the script).
*   **Outputs:** `"table_weights.csv"` (All weights for all outcomes), `"table_hr_sg.csv"` (Hazard ratios for each subgroup and outcome), `"table_HR_Cind.csv"` (Hazard ratios and C-indices for both scores).

**`7_validation.R`**

**Runs cross-validation.**
*   **Inputs:** `"working_file_w_SG3.rds"` (Main dataset), `"X_analysis_functions.R"` (Functions used in the script), `"filtered_hr_results_validation.rds"` (Hazard ratios/beta values derived in the other cohort).
*   **Outputs:** `"HR_Cind_crossvalidation.csv"` (Hazard ratios and C-indices for the weighted score in the cross-validation).

**`8_sensitivity.R`**

**Performs sensitivity analyses by alternating between datasets, exposure variables, and adjustments.**
*   **Inputs:** `"X_analysis_functions.R"`, `"working_file_w_SG3.rds"`, `"working_file_w_SG3_lag.rds"`, or `"working_file_w_SG3_crude.rds"` (Depending on the target analysis).
*   **Outputs:** `"table_weights_[suffix].csv"` and `"table_HR_cind_[suffix].csv"` (Suffix depending the type of sensitivity analysis).


**`X_analysis_functions.R`**

**Contains statistical helper functions sourced by the analysis scripts.**