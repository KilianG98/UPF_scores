**Author:** Kilian Gandolf

**Last edited:** 19.08.2026

This repository contains the code used for data cleaning, descriptive analyses, and the main statistical analyses for the paper:

**Beyond total ultra-processed food intake: consumption profiles and risk of cancer, cardiometabolic disease and mortality**

**DOI:** *(to be added)*

---

## Phase 1: Data Preparation & Cleaning

**`1_Modifizierte_UPF_Subgroups_KR.do`**
*   **Purpose:** Stata script used to define the ultra-processed food (UPF) subgroups.

**`2_dietscore.R`**
*   **Purpose:** Calculates dietary scores based on the baseline nutritional data.

**`3_orc.R`**
*   **Purpose:** Generates the obesity-related cancers (ORC) variable.

**`4_cleaning.R`**
*   **Purpose:** Performs the final data cleaning steps and prepares the analysis dataset.
*   **Inputs:** `"UKB_data_all3.rds"` (Dataset with added variables for ORC, dietscore, and UPF subgroups).
*   **Outputs:** `"working_file3.rds"` (Cleaned dataset with exclusion criteria applied and unnecessary variables removed).

---

## Phase 2: Subgroup Adjustments

**`5_0_subgroups_main.R`**
*   **Performs residual energy adjustment of UPF subgroups and creates quartiles.**
*   **Inputs:** `"working_file3.rds"` (Cleaned dataset).
*   **Outputs:** `"working_file_w_SG3.rds"` (Main dataset with quartiles of energy-adjusted subgroups).

**`5_1_subgroups_crude.R`** *(for sensitivity analyisis)*
*   **Purpose:** Generates quartiles without residual energy adjustment.
*   **Inputs:** `"working_file3.rds"`
*   **Outputs:** `"working_file_w_SG3_crude.rds"` (Dataset containing quartiles of crude subgroups).

**`5_2_lag_time.R`** *(for sensitivity analyisis)*
*   **Purpose:** Generates the file required for lag-time analysis.
*   **Inputs:** `"working_file_w_SG3.rds"`
*   **Outputs:** `"working_file_w_SG3_lag.rds"` (Dataset with lag time implemented).

---

## Phase 3: Primary Analysis & Validation

**`6_main_analysis.R`**
*   **Purpose:** Performs the primary statistical analyses.
*   **Inputs:** 
    *   `"working_file_w_SG3.rds"`
    *   `"X_analysis_functions.R"`
*   **Outputs:** 
    *   `"table_weights.csv"` (All weights for all outcomes).
    *   `"table_hr_sg.csv"` (Hazard ratios for each subgroup and outcome).
    *   `"table_HR_Cind.csv"` (Hazard ratios and C-indices for both scores).
    *   `"list_of_plot_dfs.rds"` (List of dataframes for plotting HRs across scores and consumption profiles).

**`7_validation.R`**
*   **Purpose:** Runs cross-validation.
*   **Inputs:** 
    *   `"working_file_w_SG3.rds"`
    *   `"X_analysis_functions.R"`
    *   `"filtered_hr_results_validation.rds"` (Hazard ratios/beta values derived in the other cohort).
*   **Outputs:** `"HR_Cind_crossvalidation.csv"` (Hazard ratios and C-indices for the weighted score in the cross-validation).

**`8_sensitivity.R`**
*   **Purpose:** Performs sensitivity analyses by alternating between datasets, exposure variables, and adjustments.
*   **Inputs:** 
    *   `"X_analysis_functions.R"`
    *   `"working_file_w_SG3.rds"`, `"working_file_w_SG3_lag.rds"`, or `"working_file_w_SG3_crude.rds"` (Depending on the target analysis).
*   **Outputs:** 
    *   `"table_weights_[suffix].csv"`
    *   `"table_HR_cind_[suffix].csv"` *(Note: Suffix dynamically adapts to the type of sensitivity analysis).*

---

## Helper Scripts

**`X_analysis_functions.R`**
*   **Purpose:** Contains statistical helper functions sourced by the main analysis scripts.