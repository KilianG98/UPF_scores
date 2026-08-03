library(dplyr)
library(tibble)
rm(list=ls())

library(survival)
library(dplyr)
library(haven)
library(stringr)
library(forestplot)
library(rms)
setwd("C:/Users/kiliang98/phd/EPIC/alysha/analysis")

source("scripts/X_analysis_functions_v2.R")

# df<- readRDS("data/working_file_w_SG2.rds")

#16.2.26 new version, based on alternative definition of animalp subgroup
df<- readRDS("data/working_file_w_SG3.rds")

################################################################################
#define vars--------------------------------------------------------------------
################################################################################

covars <- c("energy_intake_overall", "alc_stat_0_0_touch",  "smoke_stat_0_0", "pa_total_mets", "education_cat2", 
												"score_diet", "ever_hrt_0_0", "menopause_status_0_0" ) # "bmi_m_0_0"
stratify <- c("assess_cen",  "age_cat_5yr", "sex")


#PBA
# upf_sg_5_q <- c("UPF_sg1_rs_q", "UPF_sg2_rs_q", "UPF_sg3_rs_q", "UPF_sg4_rs_q", "UPF_sg5_rs_q" )
#sweets
upf_sg_5_q <- c("UPF_s1_rs_q", "UPF_s2_rs_q", "UPF_s3_rs_q", "UPF_s4_rs_q", "UPF_s5_rs_q")

#PBA
#upf_sg_5_bin <- c("UPF_sg1_rs_bin", "UPF_sg2_rs_bin", "UPF_sg3_rs_bin", "UPF_sg4_rs_bin", "UPF_sg5_rs_bin" )
#sweets
upf_sg_5_bin <-c("UPF_s1_rs_bin", "UPF_s2_rs_bin", "UPF_s3_rs_bin", "UPF_s4_rs_bin", "UPF_s5_rs_bin" )



#set quartiles or binary classification
upf_q_or_bin <- upf_sg_5_q
#set max level accordingly
max_l <- ifelse(all(upf_q_or_bin == upf_sg_5_q), 3, 1)
#helper_vars indicating grouping
sweet_or_pba <- ifelse(startsWith(upf_q_or_bin[1], "UPF_s1"), "sweet", "pba")
q_or_bin<- ifelse(all(upf_q_or_bin == upf_sg_5_q), "q", "bin")

#calc unweighted score
df$UPF_sg_sum <- rowSums(
	data.frame(lapply(df[, upf_q_or_bin], function(x) {
		# 17.02 Convert factor → character → numeric, recoding "nC" as 0
		# x <- as.character(x)
		# x[x == "nC"] <- "0"
		as.numeric(x)
	}))
)
df$UPF_sg_sum_sd <- df$UPF_sg_sum / sd(df$UPF_sg_sum)

#define outcomes
outcome_s <- c("obesity_cancer", "allcvd_inc", "t2dm_inc" , "total_mortality" )
outcome_t <- c( "age_exit_first_cancer", "age_exit_first_CVD", "age_exit_first_T2DM", "age_exit_first_mortality")
outcome_labels <- c("ORC", "CVD","T2DM", "Death")
interactions <- list()

#19.02 gate adjustment
gate_adjust <- paste0(upf_q_or_bin, "nC")
covars_gate <- c(covars, gate_adjust)

################################################################################
#modelling, calculating weights-------------------------------------------------
################################################################################

#introduce lvl_ids to df. lvl_ids indicate if var == lvl
#one lvl id for each lvl of each var will be created.
#This will be used later for calculation of the score.
tmp_res <- get_lvl_ids(vars=upf_q_or_bin,max_l= max_l, df=df)
df <- tmp_res$df
lvl_vars <- tmp_res$new_vars

#fit models with binary or quartiles of subgroups
cox_models1 <- fit_cox_models(outcome_t, outcome_s, upf_q_or_bin, covars_gate, stratify, df, outcome_labels, interactions)

# Extract hazard ratios and beta values from the results
hr_results <- extract_hr_results(cox_models1)

#filter for beta values of subgroups and interactions
filtered_hr_results <- lapply(hr_results, function(df) {
	df[(grepl(rownames(df), pattern = paste0(upf_q_or_bin, collapse = "|")) |
						!grepl(rownames(df), pattern = paste0(covars, collapse = "|"))), ]
})

#19.02 remove gate variables, they are not weighted
filtered_hr_results1 <- lapply(filtered_hr_results, function(df) {
	df[!grepl("nC", rownames(df)), ]
})


#calculate the weighted sum of subgroups
#based on beta values from the earlier cox models
df <- calculate_weighted_score(df,outcome_labels, filtered_hr_results1)

weighted_scores <- c()
for(oc in outcome_labels){ weighted_scores <- cbind (weighted_scores, paste0(oc, "_UPF_sg_weighted_sd"))}


#fit weighted vs unweighted models
cox_models_specific_scaled <- fit_models(outcome_t, outcome_s, scores=weighted_scores , covars_gate, stratify, df, outcome_labels)
cox_models_nonspecific_scaled <- fit_models(outcome_t, outcome_s, scores=c("UPF_sg_sum_sd", "UPF_sg_sum_sd", "UPF_sg_sum_sd", "UPF_sg_sum_sd") , covars_gate, stratify, df, outcome_labels)


################################################################################
#test violations
################################################################################

# schoenfeld
for (oc in outcome_labels){
	print(oc)
	cox <- cox_models1[[oc]]
	ph_test<- cox.zph(cox)
	print(ph_test)
}

ph_test	<- cox.zph(cox_models1$T2DM)
ph_test
plot(ph_test)


for (oc in outcome_labels){
	print(oc)
	cox <- cox_models_nonspecific_scaled[[oc]]
	ph_test<- cox.zph(cox)
	print(ph_test)
}
for (oc in outcome_labels){
	print(oc)
	cox <- cox_models_specific_scaled[[oc]]
	ph_test<- cox.zph(cox)
	print(ph_test)
}



#test covariates alcohol and energy for non-linearity


#define rcs in covars
covars_energy_rcs <- c("rcs(energy_intake_overall,4)", "alc_stat_0_0_touch",  "smoke_stat_0_0", "pa_total_mets", "education_cat2", 
																							"score_diet", "ever_hrt_0_0", "menopause_status_0_0", "UPF_s1_rs_qnC",
																							"UPF_s2_rs_qnC", "UPF_s3_rs_qnC", "UPF_s4_rs_qnC", "UPF_s5_rs_qnC")
covars_pa_rcs <- c("energy_intake_overall", "alc_stat_0_0_touch",  "smoke_stat_0_0", "rcs(pa_total_mets,4)", "education_cat2", 
																			"score_diet", "ever_hrt_0_0", "menopause_status_0_0", "UPF_s1_rs_qnC",
																			"UPF_s2_rs_qnC", "UPF_s3_rs_qnC", "UPF_s4_rs_qnC", "UPF_s5_rs_qnC")
#define rcs in covars
covars_diet_rcs <- c("energy_intake_overall", "alc_stat_0_0_touch",  "smoke_stat_0_0", "pa_total_mets", "education_cat2", 
																					"rcs(score_diet,4)", "ever_hrt_0_0", "menopause_status_0_0", "UPF_s1_rs_qnC",
																					"UPF_s2_rs_qnC", "UPF_s3_rs_qnC", "UPF_s4_rs_qnC", "UPF_s5_rs_qnC")



#fit models with rcs
cox_models_energy_rcs <-fit_cox_models(outcome_t, outcome_s, upf_q_or_bin, covars_energy_rcs, stratify, df, outcome_labels, interactions)

cox_models_pa_rcs <-fit_cox_models(outcome_t, outcome_s, upf_q_or_bin, covars_pa_rcs, stratify, df, outcome_labels, interactions)

cox_models_diet_rcs <-fit_cox_models(outcome_t, outcome_s, upf_q_or_bin, covars_diet_rcs, stratify, df, outcome_labels, interactions)

#test for significance
for (oc in outcome_labels){
	print(oc)
	ano <-anova(cox_models1[[oc]], cox_models_energy_rcs[[oc]], test = "Chisq")
	print(ano)
}

for (oc in outcome_labels){
	print(oc)
	ano <-anova(cox_models1[[oc]], cox_models_pa_rcs[[oc]], test = "Chisq")
	print(ano)
}

for (oc in outcome_labels){
	print(oc)
	ano <-anova(cox_models1[[oc]], cox_models_diet_rcs[[oc]], test = "Chisq")
	print(ano)
}

######extract

#test scores for non-linearity

#define rcs scores
weighted_scores_rcs <- c("rcs(ORC_UPF_sg_weighted_sd,4)", "rcs(CVD_UPF_sg_weighted_sd,4)", "rcs(T2DM_UPF_sg_weighted_sd,4)", "rcs(Death_UPF_sg_weighted_sd,4)")
unweighted_scores_rcs <- c("rcs(UPF_sg_sum_sd,4)", "rcs(UPF_sg_sum_sd,4)", "rcs(UPF_sg_sum_sd,4)", "rcs(UPF_sg_sum_sd,4)")

#fit models with rcs scores
cox_models_weighted_rcs <- fit_models(outcome_t, outcome_s, scores=weighted_scores_rcs , covars_gate, stratify, df, outcome_labels)
cox_models_unweighted_rcs <- fit_models(outcome_t, outcome_s, scores=unweighted_scores_rcs , covars_gate, stratify, df, outcome_labels)

#test for significance
for (oc in outcome_labels){
	print(oc)
	ano <-anova(cox_models_specific_scaled[[oc]], cox_models_weighted_rcs[[oc]], test = "Chisq")
	print(ano)
}

for (oc in outcome_labels){
	print(oc)
	ano <-anova(cox_models_nonspecific_scaled[[oc]], cox_models_unweighted_rcs[[oc]], test = "Chisq")
	print(ano)
}


extract_ph_violations <- function(model_list, outcome_labels, alpha = 0.05){
	
	results <- list()
	
	for (oc in outcome_labels){
		
		ph <- cox.zph(model_list[[oc]])
		ph_table <- as.data.frame(ph$table)
		ph_table$term <- rownames(ph_table)
		ph_table$outcome <- oc
		
		# rename columns safely
		colnames(ph_table)[which(colnames(ph_table) == "p")] <- "p_value"
		
		# keep significant
		sig <- ph_table %>%
			filter(p_value < alpha)
		
		if(nrow(sig) > 0){
			results[[oc]] <- sig
		}
	}
	
	bind_rows(results)
}


extract_lrt_violations <- function(model_linear, model_rcs, outcome_labels, test_name, alpha = 0.05){
	
	results <- list()
	
	for (oc in outcome_labels){
		
		ano <- anova(model_linear[[oc]], model_rcs[[oc]], test = "Chisq")
		ano_df <- as.data.frame(ano)
		
		# second row contains LRT comparison
		p_value <- ano_df$`Pr(>|Chi|)`[2]
		chisq   <- ano_df$Chisq[2]
		df_diff <- ano_df$Df[2]
		
		print(p_value)
		if(!is.na(p_value) && p_value < alpha){
			results[[oc]] <- data.frame(
				outcome = oc,
				test = test_name,
				chisq = chisq,
				df = df_diff,
				p_value = p_value
			)
		}
	}
	
	bind_rows(results)
}

ph_violations_main <- extract_ph_violations(cox_models1, outcome_labels)

ph_violations_nonspecific_scaled <- extract_ph_violations(cox_models_nonspecific_scaled, outcome_labels)

ph_violations_specific_scaled <- extract_ph_violations(cox_models_specific_scaled, outcome_labels)


lrt_energy <- extract_lrt_violations(
	cox_models1,
	cox_models_energy_rcs,
	outcome_labels,
	test_name = "energy_nonlinearity"
)

lrt_pa <- extract_lrt_violations(
	cox_models1,
	cox_models_pa_rcs,
	outcome_labels,
	test_name = "pa_nonlinearity"
)

lrt_diet <- extract_lrt_violations(
	cox_models1,
	cox_models_diet_rcs,
	outcome_labels,
	test_name = "diet_nonlinearity"
)


lrt_weighted_scores <- extract_lrt_violations(
	cox_models_specific_scaled,
	cox_models_weighted_rcs,
	outcome_labels,
	test_name = "weighted_score_nonlinearity"
)


lrt_unweighted_scores <- extract_lrt_violations(
	cox_models_nonspecific_scaled,
	cox_models_unweighted_rcs,
	outcome_labels,
	test_name = "unweighted_score_nonlinearity"
)