# Load necessary libraries
libs <- function(){
	library(haven)
	library(dplyr)
	library(ggplot2)
	library(survival)
	library(forestplot)
	library(tidyr)
	library(ggplotify)
	library(patchwork)
	library(gridExtra)
	library(tibble)
	library(forestplot)
	library(wesanderson)
	
}

################################################################################
# functions for interaction#
################################################################################

####Function to get significant interactions 
#only considering 2-way interactions, can be adapted for higher order interaction
#receives: outcometimes -status(censored or not) -label/name of outcome
#main exposures, covariates, stratification vars and outcomew
#returns: significatn interactions in a df that is seperated by outcome
# Helper: generate all possible pairwise interactions from a set of variable names
get_significant_interactions <- function(outcometimes, outcome_status, exposures, covariates, strat, data, outcome_labels, max_l) {
	
	# Base Cox model without interactions
	current_cox <- get_cox(outcometimes, outcome_status, exposures, covariates, strat, data)
	
	significant_interactions <- c()
	p_values <- c()
	
	# Generate interaction terms
	interactions <- get_interactions_hlp(exposures, max_l)
	
	# Test each interaction individually
	for (interaction in interactions) {
		#print(interaction)
		inter_cox <- get_cox(outcometimes, outcome_status, c(exposures, interaction), covariates, strat, data)
		cur_anova <- anova(current_cox, inter_cox, test = "Chisq")
		p_value <- cur_anova[2, "Pr(>|Chi|)"]
		
		if (!is.na(p_value) && p_value < 0.05) {
			significant_interactions <- c(significant_interactions, interaction)
			p_values <- c(p_values, p_value)
		}
	}
	
	return(list(interactions = significant_interactions, p_values = p_values))
	
}

#helperfunction for get_significnat interactions
#returns all combninations of interactions, all levels of each var.
get_interactions_hlp <- function(vars, max_l) {
	
	if(length(vars) < 2) {
		return(character(0))  # No interactions possible
	}
	
	interactions <- combn(vars, 2, FUN = function(x) paste(x, collapse = ":"))
	return(interactions)
}

#helperfunction for get_significnat interactions
#receives neccessary vars, returns cox model
get_cox <- function(outcometimes, outcome_status, exposures,covariates, strat, data){
	exposures <- as.character(exposures)
	covariates <- as.character(covariates)
	predictors <- c(exposures, covariates)
	formula <- as.formula(paste("Surv(", "age_recru", ", ", outcometimes, ", ", outcome_status, ") ~ ", 
																													paste(predictors, collapse = " + "), 
																													" + strata(", paste(strat, collapse = " , "), ")" , collapse =""))
	
	
	cox <- coxph(formula, data = data)
	#print(summary(cox))
	return(cox)
	
}

#selects interactions that impove the model
#uses forward selection alfgorithm, until lrt indicates no significant improvement (p >= 0.05)
select_inters <- function(inters_sorted, outcome_t, outcome_s, exposures, covariates, strat, data) {
	
	cox_old <- get_cox(outcometimes = outcome_t, outcome_status = outcome_s, exposures, covariates, strat, data)
	final_inters <- c()
	
	for(inter in inters_sorted) {
		exposures_tmp <- c(exposures, inter)
		cox_new <- get_cox(outcometimes = outcome_t, outcome_status = outcome_s, exposures_tmp, covariates, strat, data)
		cur_anova <- anova(cox_old, cox_new, test = "Chisq")
		p_value <- cur_anova$`Pr(>|Chi|)`[2]
		print(cur_anova)
		
		
		if(!is.na(p_value) && p_value <= 0.05) {
			final_inters <- c(final_inters, inter)
			exposures <- exposures_tmp
			cox_old <- cox_new
		} else {
			break
		}
	}
	
	return(final_inters)
}

################################################################################
#functions for modelling, extracting results------------------------------------
################################################################################

#make vars indicating whether var == level
get_lvl_ids <- function(vars, max_l,min_l, df){
	new_vars <- c() 
	
	for(var in vars){
		
		for (i in min_l:max_l){
			new_var_name <- paste0(var, i)
			df[[new_var_name]] <- df[[var]] == i
			new_vars <- c(new_vars, new_var_name)
		}
		# #3.7.26 adapted to also included non-consumers (hardcoded, shame on you)
		# new_var_name <- paste0(var, "nC")
		# df[[new_var_name]] <- df[[var]] == "nC"
		# new_vars <- c(new_vars, new_var_name)
	}
	
	return(list(
		df = df,
		new_vars = new_vars
	))
}

###Function to fit Cox models for multiple outcomes
#receives lists of outcometimes, status(censored or no) and labels( =name of outcome) these have to be in the same order
#also receives exposures, cov, strat, interactions
#returns list of coxmodels (contains 1 model per outcome)
fit_cox_models <- function(outcometimes, outcome_status, exposures, covariates, strat, data, outcome_labels ,interactions) {
	#create formulas
	covariates_formula <- paste(covariates, collapse = " + ")
	strat_formula <- paste0("strata(", paste(strat, collapse = " , "), ")")
	
	#initiate list
	cox_models <- list()
	
	#create modwl for each outcome
	for (i in seq_along(outcometimes)) {
		
		#create formula, contains interactions if present for outcome
		if(any(grepl(outcome_labels[i], names(interactions)))){
			interaction_formula <- paste(c(paste(exposures, collapse = " + "), interactions[[outcome_labels[i]]]), collapse = " + ")
		}else{interaction_formula <- paste(exposures, collapse = " + ")}
		
		#final formula
		formula_str <- paste0(
			"Surv(", "age_recru", ", ", outcometimes[i], ", ", outcome_status[i], ") ~ ", 
			interaction_formula, " + ", covariates_formula , "+",
			strat_formula
		)
		
		#append model to list
		model_name <- paste0(outcome_labels[i])
		cox_models[[model_name]] <- coxph(as.formula(formula_str), data = data)
		
	}
	#return all models
	return(cox_models)
}

###Function to extract HR/beta and CI values from list of coxmodels
#also extracts interactions
extract_hr_results <- function(cox_models) {
	
	#set up df
	
	hr_results <- data.frame(
		model = character(),
		variable = character(),
		beta = numeric(),
		se_beta = numeric(),
		beta_l = numeric(),
		beta_u = numeric(),
		hazard_ratio = numeric(),
		lower_ci = numeric(),
		upper_ci = numeric(),
		p_value = numeric(),
		stringsAsFactors = FALSE
	)
	
	#extract data from each model
	for (model_name in names(cox_models)) {
		cox_model <- cox_models[[model_name]]
		model_summary <- summary(cox_model)
		coef_table <- model_summary$coefficients
		ci_table <- model_summary$conf.int
		
		for (var_name in rownames(coef_table)) {
			beta <- coef_table[var_name, "coef"]
			se_beta <- coef_table[var_name, "se(coef)"]
			beta_l <- beta - 1.96 * se_beta
			beta_u <- beta + 1.96 * se_beta
			hazard_ratio <- coef_table[var_name, "exp(coef)"]
			lower_ci <- ci_table[var_name, "lower .95"]
			upper_ci <- ci_table[var_name, "upper .95"]
			p_value <- coef_table[var_name, "Pr(>|z|)"]
			
			hr_results <- rbind(hr_results, data.frame(
				model = model_name,
				variable = var_name,
				beta = beta,
				se_beta = se_beta,
				beta_l = beta_l,
				beta_u = beta_u,
				hazard_ratio = hazard_ratio,
				lower_ci = lower_ci,
				upper_ci = upper_ci,
				p_value = p_value
			))
		}
		
	}
	#split dataframe by model, rename the rows to reflect exposures
	hr_sep <- split(hr_results, hr_results$model)
	hr_sep <- lapply(hr_sep, function(df) {
		df <- df[, setdiff(names(df), "model")]
		rownames(df) <- df$variable
		df <- df[, -which(names(df) == "variable")] 
		return(df)
	})
	return(hr_sep)
}

###function to calculate weighted score
#receives dataset, outcomes, beta-values for each exposure
calculate_weighted_score <- function(data, outcome_labels, expo_betas) {
	
	for (outcome in outcome_labels) {
		
		#initiate 0-column for current weighted score
		data[[paste0(outcome, "_UPF_sg_weighted")]] <- 0  
		
		for (expo in rownames(expo_betas[[outcome]])) {
			beta <- as.numeric(expo_betas[[outcome]][expo, "beta"])
			
			#creates interaction vars (->is interaction present? 1/0)
			if (grepl(":", expo)) {
				inter <- strsplit(expo,":")[[1]]
				data[[expo]] <- data[[inter[1]]] * data[[inter[2]]]
			}
			
			data[[paste0(outcome, "_UPF_sg_weighted")]] <- data[[paste0(outcome, "_UPF_sg_weighted")]]+ as.numeric(data[[expo]]) * beta
		}
		#scale weighted score by dividing by sd
		data[[paste0(outcome, "_UPF_sg_weighted_sd")]] <- data[[paste0(outcome, "_UPF_sg_weighted")]]/sd(data[[paste0(outcome, "_UPF_sg_weighted")]], na.rm = TRUE)
	}
	return(data)
}

###function to calculate weighted score
#receives dataset, outcomes, beta-values for each exposure
calculate_weighted_score_validation <- function(data, outcome_labels, weights_valid) {
	
	for (outcome in outcome_labels) {
		
		#initiate 0-column for current weighted score
		data[[paste0(outcome, "_UPF_sg_weighted")]] <- 0  
		
		for (expo in rownames(weights_valid)) {
			weight <- as.numeric(weights_valid[expo,outcome])
			data[[paste0(outcome, "_UPF_sg_weighted_sd")]] <- data[[paste0(outcome, "_UPF_sg_weighted")]]+ as.numeric(data[[expo]]) * weight
		}
		# #scale weighted score by dividing by sd
		# data[[paste0(outcome, "_UPF_sg_weighted_sd")]] <- data[[paste0(outcome, "_UPF_sg_weighted")]]/sd(data[[paste0(outcome, "_UPF_sg_weighted")]], na.rm = TRUE)
	}
	return(data)
}

#receives beta values, dataset, outcomes
#returns weights
calc_weights <- function(hr_df, data, outcomes) {
	
	# Initialize an empty data frame to store weights
	weights_df <- data.frame(
		Outcome = character(),
		Exposure = character(),
		Weight = numeric(),
		weight_l = numeric(),
		weigth_u = numeric(),
		stringsAsFactors = FALSE
	)
	
	#loop all outcomes
	for (oc in outcomes) {
		# Loop all subgroups/exposures
		for (exp in rownames(hr_df[[oc]])) {
			#extract beta
			beta <- as.numeric(hr_df[[oc]][exp, "beta"]) 
			
			# scale by sd
			score_sd <- sd(data[[paste0(oc, "_UPF_sg_weighted")]], na.rm = TRUE)
			weight <- beta / score_sd
			weight_l <- hr_df[[oc]][exp, "beta_l"] / score_sd 
			weight_u <- hr_df[[oc]][exp, "beta_u"] / score_sd
			# Add a new row to the weights data frame
			weights_df <- rbind(
				weights_df,
				data.frame(Outcome = oc, Exposure = exp, Weight = weight,weight_l = weight_l, weight_u = weight_u, stringsAsFactors = FALSE)
			)
		}
	}
	
	return(weights_df)
}

#calculate coxmodel of UPF vs ddUPF models
#probably this function is not needed, can maybe use one of the previous functions
fit_models <- function(outcometimes, outcome_status, scores, covariates, strat, data, outcome_labels) {
	
	covariates_formula <- paste(covariates, collapse = " + ")
	strat_formula <- paste(strat, collapse = " , ")
	cox_models_spec <- list()
	
	for (i in seq_along(scores)) {
		formula_str <- paste0(
			"Surv(", "age_recru", ", ", outcometimes[i], ", ", outcome_status[i], ") ~ ", 
			scores[i], " + ",
			covariates_formula, " + strata(", strat_formula, ")"
		)
		print(formula_str)
		
		
		model_name <- paste0(outcome_labels[i])
		cox_models_spec[[model_name]] <- coxph(as.formula(formula_str), data = data)
	}
	
	return(cox_models_spec)
}

###cacluate c index for list of models
get_c_index <-function(cox_models){
	
	results <- lapply(names(cox_models), function(name) {
		model <- cox_models[[name]]
		concord <- concordance(model)
		
		# Extract concordance and standard error
		concordance <- concord$concordance[1]
		std_error <- sqrt(concord$var)
		lower_ci <- if (!is.na(std_error)) concordance - 1.96 * std_error else NA
		upper_ci <- if (!is.na(std_error)) concordance + 1.96 * std_error else NA
		
		# Return as data frame
		data.frame(
			Model = name,
			Concordance = concordance,
			StdError = std_error,
			Lower95CI = lower_ci,
			Upper95CI = upper_ci
		)
	})
	
	# Combine results into a single data frame
	results_df <- do.call(rbind, results)
	return(results_df)
}

################################################################################
#functions for prepping and plotting--------------------------------------------
################################################################################

#gets list of cox models and a string. 
#extract HR and ci for weighted and unweighted score
#returns in dataframe with all outcomes
extract_hr_for_plot <- function(cox_models, var_str="UPF") {
	#initiate df
	hr_results <- data.frame(
		variable = character(),
		hazard_ratio = numeric(),
		lower_ci = numeric(),
		upper_ci = numeric(),
		model_label = character(),
		stringsAsFactors = FALSE
	)
	
	#loop through models
	for (model_name in names(cox_models)) {
		cox_model <- cox_models[[model_name]]
		model_summary <- summary(cox_model)
		coef_table <- model_summary$coefficients
		ci_table <- model_summary$conf.int
		
		#extract values of interest
		for (var_name in rownames(coef_table)) {
			if (grepl(var_str, var_name)){
				hazard_ratio <- exp(coef_table[var_name, "coef"])
				lower_ci <- ci_table[var_name, "lower .95"]
				upper_ci <- ci_table[var_name, "upper .95"]
				
				hr_results <- rbind(hr_results, data.frame(
					variable = var_name,
					hazard_ratio = hazard_ratio,
					lower_ci = lower_ci,
					upper_ci = upper_ci,
					model_label = model_name
				))
			}
		}
	}
	
	return(hr_results)
}

#receives consumption profiles, outcomes, weights (already scaled by sd)
#returns df with weighted-score per profile and outcome
weighted_score_for_plot <- function(data, outcome_labels, expo_weights) {
	
	for (outcome in outcome_labels) {
		
		data[[paste0(outcome, "_UPF_sg_weighted_sd")]] <- 0 
		
		#loop over each exposure to calculate weighted score based on weights and presence of exposure in data
		for (expo in rownames(expo_weights[[outcome]])) {
			print(expo_weights[[outcome]][expo,])
			beta <- as.numeric(expo_weights[[outcome]][expo, "Weight"])
			
			#handles interactions
			if (grepl(":", expo)) {
				print("inter")
				inter <- strsplit(expo,":")[[1]]
				exp1 <- substr(inter[1], 1, nchar(inter[1]) - 1)
				lev1 <- str_sub(inter[1], -1)
				exp2 <- substr(inter[2], 1, nchar(inter[2]) - 1)
				lev2 <- str_sub(inter[2], -1)
				
				data[[paste0(outcome, "_UPF_sg_weighted_sd")]] <- data[[paste0(outcome, "_UPF_sg_weighted_sd")]] + beta * as.numeric(data[[exp1]] == lev1) *  as.numeric(data[[exp2]] == lev2)
			} else {
				exp <- substr(expo, 1, nchar(expo) - 1)
				lev <- str_sub(expo, -1)
				#non-consumers are not considered in plot, -1 breaks the code otheriwse, 
				#bc the characteres are longer then
				if (endsWith(exp, "-")) {
					next 
				}
				data[[paste0(outcome, "_UPF_sg_weighted_sd")]] <- data[[paste0(outcome, "_UPF_sg_weighted_sd")]] + beta * as.numeric(data[[exp]] == lev)
			}
		}
		
		
	}
	return(data)
}

#receives df with HR for exposure and outcome, combionations of exposures, list of outcomes
#plots HR^UPF sum vs HR^weighted_score for each outcome 
#retunrs df with coresponding values
create_outcome_plot <- function(plot_df, hr_combined, outcome_labels){
	
	#create list of dfs for each outcome
	#each df contains consumption profiles, outcome specific HRs for weighted and
	#unweighted score with CI
	list_of_plot_dfs <- list()
	for (outcome in outcome_labels) {
		
		#get consumption profile columns
		temp_df <- plot_df[, !grepl("UPF_sg_", colnames(plot_df))]
		
		#calculate weighted score^HR for each profile
		temp_df[[paste0(outcome, "_UPF_sg_weighted_HR")]]   <- hr_combined[[paste0(outcome, "_UPF_sg_weighted_sd")]]$hazard_ratio[1] ** plot_df[[paste0(outcome, "_UPF_sg_weighted_sd")]]
		#calculate lower and upper bound
		temp_df[[paste0(outcome, "_UPF_sg_weighted_l")]] <- mapply(
			function(x) if (x > 0) hr_combined[[paste0(outcome, "_UPF_sg_weighted_sd")]]$lower_ci[1] ** x else hr_combined[[paste0(outcome, "_UPF_sg_weighted_sd")]]$upper_ci[1] ** x,
			plot_df[[paste0(outcome, "_UPF_sg_weighted_sd")]]
		)
		temp_df[[paste0(outcome, "_UPF_sg_weighted_u")]] <- mapply(
			function(x) if (x > 0) hr_combined[[paste0(outcome, "_UPF_sg_weighted_sd")]]$upper_ci[1] ** x else hr_combined[[paste0(outcome, "_UPF_sg_weighted_sd")]]$lower_ci[1] ** x,
			plot_df[[paste0(outcome, "_UPF_sg_weighted_sd")]]
		)
		
		#unweighted score
		temp_df[["UPF_sg_sum_sd_HR"]]     <- subset(hr_combined[["UPF_sg_sum_sd"]], model_label == outcome)$hazard_ratio[1] ** plot_df[["UPF_sg_sum_sd"]]
		temp_df[["UPF_sg_sum_sd_HR_l"]]   <- subset(hr_combined[["UPF_sg_sum_sd"]], model_label == outcome)$lower_ci[1] ** plot_df[["UPF_sg_sum_sd"]]
		temp_df[["UPF_sg_sum_sd_HR_u"]]   <- subset(hr_combined[["UPF_sg_sum_sd"]], model_label == outcome)$upper_ci[1] ** plot_df[["UPF_sg_sum_sd"]]
		
		#add to list of plots
		list_of_plot_dfs[[outcome]] <- temp_df
	}
	
	#plot for each outcome
	#plots are stored as plot_{outcome}
	for(outcome in outcome_labels) {
		
		#variables are named differently, based on which subgroups are considered
		prefix <- ifelse(sweet_or_pba == "pba", "UPF_sg", "UPF_s")
		#variables are named depending on quartiles or binary subgroups
		suffix <- ifelse(q_or_bin == "bin", "_rs_bin", "_rs_q")
		
		#this is current df, contains profiles, HRs for weighted and unweighted with CI
		tmp_plot_df <- list_of_plot_dfs[[outcome]]
		
		#unweighted model
		mod1 <- data.frame(
			model="UPF_sg_sum_sd",
			mean  = tmp_plot_df$UPF_sg_sum_sd_HR,
			lower = tmp_plot_df$UPF_sg_sum_sd_HR_l,
			upper = tmp_plot_df$UPF_sg_sum_sd_HR_u,
			expo  = paste0(
				UPF_s1 = as.character(tmp_plot_df[[paste0(prefix, "1", suffix)]]),
				UPF_s2 = as.character(tmp_plot_df[[paste0(prefix, "2", suffix)]]),
				UPF_s3 = as.character(tmp_plot_df[[paste0(prefix, "3", suffix)]]),
				UPF_s4 = as.character(tmp_plot_df[[paste0(prefix, "4", suffix)]]),
				UPF_s5 = as.character(tmp_plot_df[[paste0(prefix, "5", suffix)]])
			)
		)
		
		#weighted model
		mod2 <- data.frame(
			model="UPF_sg_weighted_sd",
			mean  = tmp_plot_df[[paste0(outcome, "_UPF_sg_weighted_HR")]],
			lower = tmp_plot_df[[paste0(outcome, "_UPF_sg_weighted_l")]],
			upper = tmp_plot_df[[paste0(outcome, "_UPF_sg_weighted_u")]],
			expo  = paste0(
				UPF_s1 = as.character(tmp_plot_df[[paste0(prefix, "1", suffix)]]),
				UPF_s2 = as.character(tmp_plot_df[[paste0(prefix, "2", suffix)]]),
				UPF_s3 = as.character(tmp_plot_df[[paste0(prefix, "3", suffix)]]),
				UPF_s4 = as.character(tmp_plot_df[[paste0(prefix, "4",suffix)]]),
				UPF_s5 = as.character(tmp_plot_df[[paste0(prefix, "5", suffix)]])
			)
		)
		
		#merge, df is now in long format and ready for plotting
		est <- bind_rows(mod1, mod2) %>%
			mutate(
				expo  = factor(expo, levels = unique(as.character(expo))),
				model = factor(model, levels = c("UPF_sg_sum_sd", "UPF_sg_weighted_sd"))
			) %>%
			arrange(expo, model)
		
		print(est)
		print(q_or_bin)
		xmax <- if (outcome == "T2DM" && q_or_bin == "q") 2.5 else 2
		gwid <- if (outcome == "T2DM" && q_or_bin == "q") 14 else 10
		print( xmax)
		fp_args <- list(
			labeltext = matrix(NA, nrow = length(unique(est$expo)), ncol = 1),
			
			mean = cbind(
				est %>% filter(model == "UPF_sg_sum_sd") %>% pull(mean),
				est %>% filter(model == "UPF_sg_weighted_sd") %>% pull(mean)
			),
			lower = cbind(
				est %>% filter(model == "UPF_sg_sum_sd") %>% pull(lower),
				est %>% filter(model == "UPF_sg_weighted_sd") %>% pull(lower)
			),
			upper = cbind(
				est %>% filter(model == "UPF_sg_sum_sd") %>% pull(upper),
				est %>% filter(model == "UPF_sg_weighted_sd") %>% pull(upper)
			),
			
			fn.ci_norm = c(fpDrawCircleCI, fpDrawCircleCI),
			title = outcome,
			xlab = "Hazard ratio",
			clip = c(0.7, xmax),
			lineheight = "lines",
			lwd.zero = 1.5,
			line.margin = .1,
			boxsize = 0.3,
			zero = 1,
			graphwidth = unit(gwid, "cm"),
			colgap = unit(8, "mm"),
			lwd.ci = 0.8,
			xticks = seq(0.7, xmax, 0.1),
			xlog = FALSE,
			col = fpColors(zero = "black"),
			txt_gp=fpTxtGp(xlab = gpar(cex=1.1), ticks = gpar(cex=1.05))
		)
		
		# Add legend only for CVD
		if (outcome == "CVD") {
			fp_args$legend <- c("Unweighted score", "Weighted score")
			fp_args$legend_args <- fpLegend(pos = list(x = 1.08, y=1.04), gp = gpar(lwd = 2, lty = 1, fontsize = 20))
		}
		
		# Run forestplot with dynamic arguments
		p <- do.call(forestplot, fp_args) |>
			fp_set_style(
				box = c("#3399CC", "#660033"),
				lines = c("#3399CC", "#660033" )  # same colors as the boxes
			)|>
			fp_add_lines(h_2 = gpar(lty = 2, col = "black"), h_7 = gpar(lty = 2, col = "black"),
																h_17 = gpar(lty = 2, col = "black"), h_27 = gpar(lty = 2, col = "black"),h_32 = gpar(lty = 2, col = "black"))|>
			fp_set_zebra_style("#F2F2F2", "white")
		
		#show plot
		print(p)
		
		#make plot globally available
		assign(paste0("p_", outcome), p, envir = .GlobalEnv)
		
	}
	return(list_of_plot_dfs)
	
}


###############################################################################
#stuff--------------------------------------------------------------------------
################################################################################
#firtst plot gets profiles as labeltext, others no text
# if (outcome == outcome_labels[1]) {
#   exp1 <- as.data.frame(as.factor(paste0(
#   UPF_s1 = as.character(tmp_plot_df[[paste0(prefix, "1", suffix)]]),
#   UPF_s2 = as.character(tmp_plot_df[[paste0(prefix, "2", suffix)]]),
#   UPF_s3 = as.character(tmp_plot_df[[paste0(prefix, "3", suffix)]]),
#   UPF_s4 = as.character(tmp_plot_df[[paste0(prefix, "4",suffix)]]),
#   UPF_s5 = as.character(tmp_plot_df[[paste0(prefix, "5", suffix)]])
# )))
# } else {
# n_rows <- nrow(df[[outcome]])
# exp1 <- data.frame(expo = factor(rep(" ", n_rows)))
# }