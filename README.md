**Author:** Kilian Gandolf

**Last edited:** 19.08.2026

This repository contains the code used for data cleaning, descriptive analyses, and the main statistical analyses for the paper:

**Beyond total ultra-processed food intake: consumption profiles and risk of cancer, cardiometabolic disease and mortality**

**DOI:** *(to be added)*

---

# Scripts

`1_Modifzierte_UPF_Subgroups_KR.do`
Stata script used to  define the ultra-processed food (UPF) subgroups.

`2_dietscore.R`
Script used to calculate dietary scores based on the baseline nutritional data.

`3_orc.R`
Script used to generate obesity-related Cancers variable.

`4_cleaning.R`
Script used to perform the final data cleaning steps and prepare the analysis dataset. 

Input:
"UKB_data_all3.rds": dataset with added variables for ORC, dietscore, and UPF subgroups.
Output:
"working_file3.rds": dataset with cleaned variables, exclusion criteria applied, unneccessary variables removed.

`5_0_subgroups_main.R`
Script used for residual Energy adjustemnt of UPF subgroups and creation of quartiles.

Input:
"working_file3.rds": cleaned dataset
Output:
"working_file_w_SG3.rds": main dataset with quartiles of energy adjusted subgroups.

`5_1_subgroups_crude.R`
*(Optional step)* Script used to generate quartiles without residual energy adjustment.

Input:
"working_file3.rds": cleaned dataset, does not have energy adjusted subgroups
Output:
"working_file_w_SG3_crude.rds": main dataset, now contains variables with quartiles of energy adjusted subgroups
`5_2_lag_time.R`
*(Optional step)* Script used to generate file for lag-time Analysis.

Input:
"working_file_w_SG3.rds": main dataset.
Output:
"working_file_w_SG3_lag.rds": dataset with lag time implemented.

`6_main_analysis.R`
Script used to perform the primary statistical analyses. 

Inputs:
"working_file_w_SG3.rds": main dataset.
"X_analysis_functions.R": r file with functions used in the script.
Outputs: 
"table_weights.csv": table of all weights for all outcomes.
"table_hr_sg.csv": table of hazard ratios for each subgroup and outcome.
"table_HR_Cind.csv": table of hazard ratios and C-indices for both scores.
"list_of_plot_dfs.rds": list of dataframes which contain all information for plotting of HRs across both scores and all consumption profiles

`7_validation.R`
Script used to run crossvalidation.

Inputs:
"working_file_w_SG3.rds": main dataset.
"X_analysis_functions.R": r file with functions used in the script.
"filtered_hr_results_validation.rds": dataset with hazard ratios/beta values derived in the other cohort.
Output:
"HR_Cind_crossvalidation.csv": table fo Hazard ratios and C-Indices for the weighted score in the crossvalidation.

`8_sensitivity.R`
Script used to perform sensitivity analyses. Alternate between datasets, exposure variables, and adjustment to generate desired outputs.

Inputs:
"X_analysis_functions.R": r file with functions used in the script.
"working_file_w_SG3.rds": main dataset
"working_file_w_SG3_lag.rds": dataset for lag-time analysis.
"working_file_w_SG3_crude.rds": dataset for analysis withou energy adjustment.
Outputs:
"table_weights_suffix.csv": table of all weights for all outcomes. 
"table_HR_cind_suffix.csv": table of hazard ratiosn and c-indices for both scores and all outcomes.
Suffix will be adapted to the type of sensitivity analysis.

`X_analysis_functions.R`
Contains statistical helper functions.


---