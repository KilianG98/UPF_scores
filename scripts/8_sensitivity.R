#Sensitivity analyes. Follows the same workflow as main analysis.
#Has to be manually adjusted for each sensitivity analysis 
#(read in altneraitve datasets (Lower/Upper, lag-time, crude UPF), adjust for BMI, binary exposure, interactions)

rm(list=ls())

library(survival)
library(dplyr)
library(haven)
library(stringr)
library(forestplot)
library(rms)

source("scripts/nogate/X_analysis_functions_nogate.R")

# df<- readRDS("data/working_file_w_SG3.rds")
#for lag time
# df<- readRDS("data/working_file_w_SG3_lag.rds")
#for crude variable
df<- readRDS("data/working_file_w_SG3_crude.rds")

summary(df$UPF_g_day_noalc)
sd(df$UPF_g_day_noalc)

################################################################################
#define vars--------------------------------------------------------------------
################################################################################

covars <- c("energy_intake_overall", "alc_stat_0_0_touch",  "smoke_stat_0_0", "pa_total_mets", "education_cat2","score_diet", 
												 "ever_hrt_0_0", "menopause_status_0_0") # "bmi_m_0_0"
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
		x <- as.character(x)
		x[x == "nC"] <- "0"
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
#get significant interactions between subgroups--------------------------------
################################################################################
# #uncommment entire section to perform interaction analysis.
# 
# #get a list of interactions and p-values of improving the models as compared to
# #models with no interactions.
# #This is done for all outcomes.
# interactions_w_p <- list()
# for (i in seq_along(outcome_t)) {
# 	inter <- get_significant_interactions(
# 		outcome_t[i], outcome_s[i], upf_q_or_bin,
# 		covars, stratify, df, outcome_labels, max_l = 3)
# 
# 	if (!is.null(inter) && length(inter) > 0) {
# 		interactions_w_p[[outcome_labels[i]]] <- inter
# 	} else {
# 		interactions[[outcome_labels[i]]] <- NULL
# 	}
# }
# print(interactions_w_p)
# 
# #use forward selection approach to add interactions one by one, stop when modelling
# #is not significantly improved. Start with lowest p-values
# #This is done for all outcomes
# for (i in seq_along(outcome_labels)) {
# 	oc <- outcome_labels[i]
# 	outcome_time <- outcome_t[i]
# 	outcome_stat <- outcome_s[i]
# 
# 	pvals <- interactions_w_p[[oc]]$p_values
# 
# 	if (!is.null(pvals) && length(pvals) > 0) {
# 		interactions_sorted <- interactions_w_p[[oc]]$interactions[order(pvals)]
# 
# 		interactions[[oc]] <- select_inters(
# 			interactions_sorted,
# 			outcome_time, outcome_stat,
# 			exposures = upf_q_or_bin,
# 			covariates = covars,
# 			strat = stratify,
# 			data = df
# 		)
# 
# 	} else {
# 		print("isnull")
# 		interactions[[oc]] <- NULL
# 	}
# }
# 
# saveRDS(interactions, "data/interactions_wpba_q.rds")

# ################################################################################
# 
# ################################################################################
#modelling, calculating weights-------------------------------------------------
################################################################################

#introduce lvl_ids to df. lvl_ids indicate if var == lvl
#one lvl id for each lvl of each var will be created.
#This will be used later for calculation of the score.
tmp_res <- get_lvl_ids(vars=upf_q_or_bin,max_l= max_l, df=df)
df <- tmp_res$df
lvl_vars <- tmp_res$new_vars

#fit models with binary or quartiles of subgroups
cox_models1 <- fit_cox_models(outcome_t, outcome_s, upf_q_or_bin, covars, stratify, df, outcome_labels, interactions)

# Extract hazard ratios and beta values from the results
hr_results <- extract_hr_results(cox_models = cox_models1)

#filter for beta values of subgroups and interactions
upf_pattern <- paste(upf_q_or_bin, collapse = "|")
filtered_hr_results<- lapply(hr_results, function(df) {
	df[grepl(upf_pattern, rownames(df)), ]
})

#19.02 filter gate variables, not needed for score calc
# filtered_hr_results1 <- lapply(filtered_hr_results, function(df) {
#   df[!grepl("nC", rownames(df)), ]
# })

#calculate the weighted sum of subgroups
#based on beta values from the earlier cox models
df <- calculate_weighted_score(df,outcome_labels, filtered_hr_results )

weighted_scores <- c()
for(oc in outcome_labels){ weighted_scores <- cbind (weighted_scores, paste0(oc, "_UPF_sg_weighted_sd"))}

#fit weighted vs unweighted models
cox_models_specific_scaled <- fit_models(outcome_t, outcome_s, scores=weighted_scores , covars, stratify, df, outcome_labels)
cox_models_nonspecific_scaled <- fit_models(outcome_t, outcome_s, scores=c("UPF_sg_sum_sd", "UPF_sg_sum_sd", "UPF_sg_sum_sd", "UPF_sg_sum_sd") , covars_gate, stratify, df, outcome_labels)

################################################################################
#extracting results-------------------------------------------------------------
################################################################################

#extract HR for weighted and unweighted score
hr_results_1 <- extract_hr_for_plot(cox_models_specific_scaled)
hr_results_2 <- extract_hr_for_plot(cox_models_nonspecific_scaled)

#merge into 1
hr_combined1 <- rbind(hr_results_1, hr_results_2)
hr_combined<- split(hr_combined1, hr_combined1$variable)

#get dataframe with weights for plotting
#weights are for each quartile of each subgroup
weights <- calc_weights(filtered_hr_results, df, outcome_labels)

#create all possible consumption profiles of subgroups using only highest vs lowest category
consumption_profiles <- expand.grid(
	setNames(
		lapply(upf_q_or_bin, function(x) {
			if (x %in% upf_sg_5_q) c(0, 3) else c(0, 1)
		}),
		upf_q_or_bin
	),
	stringsAsFactors = FALSE
)
#sum up consumption profiles to create unweighted scores and order correctly
consumption_profiles$UPF_sg_sum<- rowSums(consumption_profiles)
consumption_profiles <- consumption_profiles[order(consumption_profiles$UPF_sg_sum), ]


#split weights by outcome, this is useful for further steps
weights_split_oc <- split(weights, weights$Outcome)
#Now, outcome col can be removed. 
#also use exposure (q of each subgroup) as rownames and remove exposure col
weights_split_oc <- lapply(weights_split_oc, function(df) {
	df$Outcome <- NULL   
	rownames(df) <- df$Exposure 
	df$Exposure <- NULL  
	return(df)
})
#19.02 filter gate vars, not needed for plot
weights_split_oc_plot <- lapply(weights_split_oc, function(df) {
	df[!grepl("nC", rownames(df)), ]
})

#calculate weighted score for plotting, merge into dataframe
plot_df <- weighted_score_for_plot(consumption_profiles, outcome_labels, weights_split_oc_plot)
#scale unweighted score by sd
plot_df$UPF_sg_sum_sd <- plot_df$UPF_sg_sum / sd(df$UPF_sg_sum)


################################################################################
#plotting-----------------------------------------------------------------------
################################################################################

#plot the HR for weighted vs unweighted sum of UPFs
#calculated as HR^score, dataframe is returned withe the corresponding numbers
#each plot is stored as p_{outcome}
create_outcome_plot(plot_df,hr_combined, outcome_labels)

#plot all outcomes in one plot
grid.newpage()
#viewport with 4 cols and correct width of each col
width <- unit(convertX(unit(1, "npc"), unitTo = "npc", valueOnly = TRUE)/4, "npc")
pushViewport(viewport(layout = grid.layout(nrow = 1,
																																											ncol = 4,
																																											widths = unit.c(width,
																																																											width,
																																																											width))
)
)
#show all plots
pushViewport(viewport(layout.pos.row = 1,
																						layout.pos.col = 1))
p_ORC
upViewport()
pushViewport(viewport(layout.pos.row = 1,
																						layout.pos.col = 2))
p_CVD
upViewport()
pushViewport(viewport(layout.pos.row = 1,
																						layout.pos.col = 3))
p_T2DM
upViewport()
pushViewport(viewport(layout.pos.row = 1,
																						layout.pos.col = 4))
p_Death
upViewport()

################################################################################
###tables#
################################################################################

#####weights

df_main <- data.frame()
df_interactions<- list()

for (oc in outcome_labels) {
	df_w <- weights_split_oc[[oc]][, "Weight", drop = FALSE]
	df_main_part <- df_w[!grepl(":", rownames(df_w)), , drop = FALSE]
	colnames(df_main_part) <- oc  # name column after outcome
	
	# combine by rownames
	if (nrow(df_main) == 0) {
		df_main <- df_main_part
	} else {
		df_main <- merge(df_main, df_main_part, by = "row.names", all = TRUE)
		rownames(df_main) <- df_main$Row.names
		df_main$Row.names <- NULL
	}
	df_interactions[[oc]] <- df_w[grep(":", rownames(df_w)), , drop = FALSE]
}

#inster empty rows for ref cat
if(q_or_bin == "q" && length(upf_q_or_bin) ==5){
	df_main
	empty_r <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(df_main)))
	colnames(empty_r) <- colnames(df_main)
	df_main <- rbind(empty_r, df_main)
	df_main <- rbind(df_main[1:5, ], empty_r, df_main[6:nrow(df_main), ])
	df_main <- rbind(df_main[1:10, ], empty_r, df_main[11:nrow(df_main), ])
	df_main <- rbind(df_main[1:15, ], empty_r, df_main[16:nrow(df_main), ])
	df_main <- rbind(df_main[1:20, ], empty_r, df_main[21:nrow(df_main), ])
}

if (length(upf_q_or_bin) ==4){
	df_main
	empty_r <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(df_main)))
	colnames(empty_r) <- colnames(df_main)
	df_main <- rbind(empty_r, df_main)
	df_main <- rbind(df_main[1:4, ], empty_r, df_main[5:nrow(df_main), ])
	df_main <- rbind(df_main[1:8, ], empty_r, df_main[9:nrow(df_main), ])
	df_main <- rbind(df_main[1:12, ], empty_r, df_main[13:nrow(df_main), ])
}
#######HR, CI, Cindex, CI

df_HR_Cind <- data.frame(
	HR_w = c(1, 2, 3, 4),
	HR_uw = c(1, 2, 3, 4),
	Cindex_w =  c(1, 2, 3, 4),
	Cindex_uw = c(1, 2, 3, 4)
)

# Set row names
rownames(df_HR_Cind) <- c("ORC", "CVD", "T2DM", "Death")
for (oc in outcome_labels){
	suma <- summary(cox_models_specific_scaled[[oc]])
	df_HR_Cind[oc, "HR_w"] <- paste0(round(suma$coefficients[1,2],2), " (",round(suma$conf.int[1,3],2), "-", round(suma$conf.int[1,4],2), ")")
	concordance <- suma$concordance[[1]]
	conc_se <- suma$concordance[[2]]
	conc_l <- round(concordance - 1.96 * conc_se,3)
	conc_u <- round(concordance + 1.96 * conc_se, 3)
	
	df_HR_Cind[oc, "Cindex_w"] <- paste0(round(concordance,3), " (", conc_l, "-" ,conc_u,")")
	
	suma <- summary(cox_models_nonspecific_scaled[[oc]])
	df_HR_Cind[oc, "HR_uw"] <- paste0(round(suma$coefficients[1,2],2), " (",round(suma$conf.int[1,3],2), "-", round(suma$conf.int[1,4],2), ")")
	concordance <- suma$concordance[[1]]
	conc_se <- suma$concordance[[2]]
	conc_l <- round(concordance - 1.96 * conc_se,3)
	conc_u <- round(concordance + 1.96 * conc_se, 3)
	
	df_HR_Cind[oc, "Cindex_uw"] <- paste0(round(concordance,3), " (", conc_l, "-" ,conc_u,")")
	
}

hr_combined1 <- hr_combined1[!grepl("nC", hr_combined1$variable), ]




inter <- if (length(interactions) == 0) "noint" else "int"
if(any(grep ("bmi", covars))){ inter <- paste0(inter, "_bmi_adj")}


# Build filename prefix
file_prefix <- paste0(sweet_or_pba, "_", q_or_bin, "_", inter, "_nogate_sensi_lag")

# write.csv(plot_df, "results/plot_df_ukb_nogate.csv" )
# write.csv(hr_combined1, "results/hr_combined_ukb_nogate.csv" )
write.csv(df_main, paste0("results/table_weights_ukb_", file_prefix, "_crude.csv" ))
write.csv(df_HR_Cind, paste0("results/table_HR_ukb_", file_prefix, "_crude.csv" ))
# saveRDS(df_interactions, paste0("results/df_interactions_", file_prefix, ".rds" ))

