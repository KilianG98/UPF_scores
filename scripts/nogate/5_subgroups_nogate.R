# 06.07.2026: gate variables

rm(list=ls())
setwd("C:/Users/kiliang98/phd/EPIC/alysha/analysis/UPF_scores")
library(haven)
library(dplyr)


df <- readRDS("data/working_file3.rds")


#generating energy adjusted UPF subgroups using the residual method-------------

upf_s_5 <- c( "UPF_beverages_g_day_noalc","UPF_animalp_g_day","UPF_sweets_g_day","UPF_bc_g_day",  "UPF_other_g_day" )
upf_s_5_rs <- c("UPF_s1_rs", "UPF_s2_rs", "UPF_s3_rs", "UPF_s4_rs", "UPF_s5_rs" )

#grouping with PBA included, instead of sweets
upf_sg_5 <- c( "UPF_beverages_g_day_noalc","UPF_animalp_g_day","UPF_PBA_g_day","UPF_bc_g_day",  "UPF_otherv2_g_day" )
upf_sg_5_rs <- c("UPF_sg1_rs", "UPF_sg2_rs", "UPF_sg3_rs", "UPF_sg4_rs", "UPF_sg5_rs" )

make_resid_vars <- function(vars, outnames, data) {
	
	for(i in seq_along(vars)) {
		
		# create formula
		f <- as.formula(paste0(vars[i], " ~ energy_intake_overall + assess_cen"))
		
		# subset rows where variable > 0
		idx <- which(data[[vars[i]]] > 0)
		data2 <- data[idx, ]
		
		# fit model
		m <- lm(f, data = data2)
		
		# create empty column in full dataset
		data[[outnames[i]]] <- NA
		
		# assign residuals back to correct rows
		data[idx, outnames[i]] <- rstandard(m)
	}
	
	return(data)
}

df <- make_resid_vars(upf_s_5, upf_s_5_rs, df)
df <- make_resid_vars(upf_sg_5, upf_sg_5_rs, df)


#17.2, nCs are introduced (non consumer)
#19.02 nC change to 0
#make quantiles and binary calssification
df <- df %>%
	mutate(across(
		all_of(upf_s_5_rs),
		~ factor(ifelse(is.na(.), "nC", ntile(., 4) - 1)),
		.names = "{.col}_q"
	))


df <- df %>%
	mutate(across(
		all_of(upf_s_5_rs),
		~ factor(ifelse(is.na(.), "nC", ntile(., 2) - 1)),
		.names = "{.col}_bin"
	))

df <- df %>%
	mutate(across(
		all_of(upf_sg_5_rs),
		~ factor(ifelse(is.na(.), "nC", ntile(., 4) - 1)),
		.names = "{.col}_q"
	))





# m_tot <- lm(qe_m_n4_noalc ~ qe_energy + center, data = df)
# df$UPF_tot_rs <- rstandard(m_tot)

upf_sg_5_q <- c("UPF_s1_rs_q", "UPF_s2_rs_q", "UPF_s3_rs_q", "UPF_s4_rs_q", "UPF_s5_rs_q") 


#save file
#saveRDS(df, file="data/working_file_w_SG2.rds")

saveRDS(df, file="data/working_file_w_SG3.rds")

#How many people dont consume UPFs?
for ( var in upf_s_5){
	print(var)
	print(sum(df[[var]] == 0)/ nrow(df))
}


(df$UPF_s2_rs_q)
summary(df$UPF_s2_rs[df$UPF_animalp_g_day == 0])



