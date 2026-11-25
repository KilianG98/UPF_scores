################################################################################
#Crossvalidation of the weighted score
#Data is split into training and test set
#Weights derived in training set, score calculated in test set (out of fold -OOF)
#OOF scores are pooled, scaled by SD and tested in the whole dataset
################################################################################

rm(list=ls())

library(survival)
library(dplyr)
library(haven)
library(stringr)
library(forestplot)
library(rms)

source("scripts/X_analysis_functions.R")

df<- readRDS("data/working_file_w_SG3.rds")

################################################################################
#define vars--------------------------------------------------------------------
################################################################################

#modelling variables
covars <- c("energy_intake_overall", "alc_stat_0_0_touch",  "smoke_stat_0_0", "pa_total_mets", "education_cat2","score_diet", 
												"ever_hrt_0_0", "menopause_status_0_0" )
stratify <- c("assess_cen",  "age_cat_5yr", "sex")

#sweets
upf_sg_5_q <- c("UPF_s1_rs_q", "UPF_s2_rs_q", "UPF_s3_rs_q", "UPF_s4_rs_q", "UPF_s5_rs_q")

#set quartiles or binary classification
upf_q_or_bin <- upf_sg_5_q
#set max level accordingly
max_l <- ifelse(all(upf_q_or_bin == upf_sg_5_q), 3, 1)
min_l =-1
#helper_vars indicating grouping
sweet_or_pba <- ifelse(startsWith(upf_q_or_bin[1], "UPF_s1"), "sweet", "pba")
q_or_bin<- ifelse(all(upf_q_or_bin == upf_sg_5_q), "q", "bin")

#calc unweighted score
df$UPF_sg_sum <- rowSums(
	data.frame(lapply(df[, upf_q_or_bin], function(x) {
		# 17.02 Convert factor → character → numeric, recoding "nC" as 0
		x <- as.character(x)
		as.numeric(x)
	}))
)
#scale by SD
df$UPF_sg_sum_sd <- df$UPF_sg_sum / sd(df$UPF_sg_sum)

#define outcomes
outcome_s <- c("obesity_cancer", "allcvd_inc", "t2dm_inc" , "total_mortality" )
outcome_t <- c( "age_exit_first_cancer", "age_exit_first_CVD", "age_exit_first_T2DM", "age_exit_first_mortality")
outcome_labels <- c("ORC", "CVD","T2DM", "Death")
interactions <- list()

#19.02 gate adjustment for unweighted score
gate_adjust <- paste0(upf_q_or_bin, "nC")
covars_gate <- c(covars, gate_adjust)

#helper variables
upf_pattern     <- paste(upf_q_or_bin, collapse = "|")
weighted_scores <- paste0(outcome_labels, "_UPF_sg_weighted_sd")

################################################################################
#CV-----------------------------------------------------------------------------
################################################################################

### SETUP ###

#introduce lvl_ids to df. lvl_ids indicate if var == lvl
#one lvl id for each lvl of each var will be created.
#This will be used later for calculation of the score.
tmp_res <- get_lvl_ids(vars=upf_q_or_bin,max_l= max_l,min_l =min_l, df=df)
df <- tmp_res$df
lvl_vars <- tmp_res$new_vars

#function to apply random folds to the dataframe
make_folds <- function(df, k){
	df$folds <- sample(rep_len(1:k, nrow(df)))
	return(df)
}

#CV settings
k <-5          #number of folds
n_repeats <- 10    #number of repetitions of the k-fold split
seed <- 2026     #for reproducible fold assignment

#list to store results
cv_hr    <- vector("list", n_repeats)

#set seed
set.seed(seed)

### RUN CV ###

for (i in seq(n_repeats)){
	
	cat("starting repeat ", i, " of ", n_repeats, "\n" )
	#assign folds to the dataframe
	df1 <- make_folds(df, k)
	#make empty column for weighted scores
	df1[weighted_scores] <- NA_real_
	
	#loop over all folds
	for (f in seq(k)) {
		
		cat("fold ", f, " of ", k, "\n")
		
		#split into train and test set
		train_idx <- which(df1$folds != f)
		test_idx  <- which(df1$folds == f)
		df_train  <- df1[train_idx, ]
		df_test   <- df1[test_idx, ]
		
		#fit weight models on training folds
		cox_models1 <- fit_cox_models(outcome_t, outcome_s, upf_q_or_bin, covars, stratify, df_train, outcome_labels, interactions)
		
		#extract beta values/weights
		hr_results  <- extract_hr_results(cox_models = cox_models1)
		filtered_hr_results <- lapply(hr_results, function(x) {
			x[grepl(upf_pattern, rownames(x)), ]
		})
		
		#apply training weights to the test set
		#score is scaled by SD, this is done to ensure all OOF scores are on the same scale
		df_test <- calculate_weighted_score(df_test, outcome_labels, filtered_hr_results)
		
		#save the weighted score in df1
		for (col in weighted_scores) {
			df1[[col]][test_idx] <- df_test[[col]]
		}
	}
	
	#scale the weighted OOF scores by their total sd again, so that it has SD of 1
	for (j in seq_along(outcome_labels)){
		df1[[weighted_scores[j]]] <- df1[[weighted_scores[j]]] / sd(df1[[weighted_scores[j]]])
	}
	
	#run the model on the whole dataset
	#each participant has a score that was calculated out of fold, and is scaled by SD
	#save the results in a list
	cv_hr[[i]]    <- fit_models(outcome_t, outcome_s, scores = weighted_scores, covars, stratify, df1, outcome_labels)
}

#sanitiy check, shoulb be all 0
colSums(is.na(df1[weighted_scores]))
	
summarise_cv <- function(cv_list, outcome_labels, suffix){
	res <- lapply(outcome_labels, function(oc){
		term <- paste0(oc, suffix)
		est <- sapply(cv_list, function(r) summary(r[[oc]])$coefficients[term, "coef"])
		se  <- sapply(cv_list, function(r) summary(r[[oc]])$coefficients[term, "se(coef)"])
		b   <- mean(est)
		s   <- sqrt(mean(se^2 + (est - b)^2))
		data.frame(outcome = oc,
													beta = b, se = s,
													beta_lower = b - 1.96*s, beta_upper = b + 1.96*s,
													HR = exp(b), HR_lower = exp(b - 1.96*s), HR_upper = exp(b + 1.96*s),
													min_rep = min(est), max_rep = max(est))
	})
	do.call(rbind, res)
}

#cross-fitted HR per SD
cv_hr_summary <- summarise_cv(cv_hr, outcome_labels, "_UPF_sg_weighted_sd")
cv_hr_summary[, c("outcome", "HR", "HR_lower", "HR_upper")]

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
list_of_plot_dfs <- create_outcome_plot(plot_df,hr_combined, outcome_labels)

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

library(dplyr)
library(purrr)
library(tibble)

# 1. Daten extrahieren, zusammenfügen und Spalten ordnen
final_table <- imap(filtered_hr_results, function(df, outcome_name) {
	df %>%
		as.data.frame() %>%
		rownames_to_column("Variable") %>%
		mutate(
			HR_CI = sprintf("%.2f (%.2f-%.2f)", hazard_ratio, lower_ci, upper_ci)
		) %>%
		select(Variable, HR_CI) %>%
		rename(!!sym(outcome_name) := HR_CI)
}) %>%
	reduce(full_join, by = "Variable") %>%
	# Hier wird die neue Reihenfolge festgelegt
	select(Variable, ORC, CVD, T2DM, Death) 

# 2. Basis-Namen der Variablen extrahieren (z. B. "UPF_s1_rs")
base_vars <- unique(sub("(_q.*)", "", final_table$Variable))

# 3. Referenz-Zeilen generieren
ref_rows <- tibble(
	Variable = paste0(base_vars, "_ref"),
	ORC = "Ref",
	CVD = "Ref",
	T2DM = "Ref",
	Death = "Ref"
)

# 4. Zusammenfügen und korrekt sortieren
final_table_with_refs <- bind_rows(ref_rows, final_table) %>%
	# Erstellt eine Hilfsspalte zum Sortieren (damit "ref" immer als Erstes steht)
	mutate(
		BaseGroup = sub("(_q.*|_ref)", "", Variable),
		IsRef = ifelse(grepl("_ref$", Variable), 0, 1)
	) %>%
	arrange(BaseGroup, IsRef, Variable) %>%
	select(-BaseGroup, -IsRef)

print(final_table_with_refs)
# write.csv(final_table_with_refs, "results/table_hr_sg.csv")
# write.csv(plot_df, "results/plot_df.csv" )
# write.csv(hr_combined1, "results/hr_combined.csv" )
# write.csv(df_main, "results/table_weights.csv" )
# write.csv(df_HR_Cind, "results/table_HR_Cind.csv" )
# saveRDS(list_of_plot_dfs, "results/list_of_plot_dfs.rds")

################################################################################
#save plots#
# ################################################################################
# inter <- if (length(interactions) == 0) "noint" else "int"
# if(any(grep ("bmi", covars))){ inter <- paste0(inter, "_bmi_adj")}
# # or "noint"
# 
# # Build filename prefix
# file_prefix <- paste0("orc_", sweet_or_pba, "_", q_or_bin, "_", inter, "_nogate")
# 
# outcome_labels <- c("ORC", "CVD","T2DM", "Death")
# 
# #plot the HR for weightes vs unweighted sum of UPFs
# #calculated as HR^score, dataframe is returned withe the corresponding numbers
# 
# create_outcome_plot(plot_df,hr_combined, outcome_labels)
# 
# library(forestplot)
# library(grid)
# w <- ifelse(q_or_bin == "q", 20, 19)
# 
# svg(paste0("results/plot_", file_prefix, ".svg"), width = w, height = 12) ######132.8!!!!!!!!!!!!!!
# 
# grid.newpage()
# grid.newpage()
# if (q_or_bin == "q") {
# 	widths <- unit(c(10, 10, 15, 10), "null")
# } else {
# widths <- unit(rep(15, 4), "null")
# }
# 	
# pushViewport(viewport(
# 	layout = grid.layout(
# 		nrow = 1,
# 		ncol = 4,
# 		widths = widths
# 	)
# ))
# 
# draw_fp <- function(fp_obj, row, col) {
# 	pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
# 	print(fp_obj)  # must explicitly print the forestplot
# 	upViewport()
# }
# 
# # Draw the 4 forestplots
# draw_fp(p_ORC, 1, 1)
# draw_fp(p_CVD, 1, 2)
# draw_fp(p_T2DM, 1, 3)
# draw_fp(p_Death, 1, 4)
# 
# dev.off()
# 
# 