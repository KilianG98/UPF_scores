# subgroups with crude variable, no reesidual energy adjustment

rm(list=ls())
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
		# assign residuals back to correct rows
		data[,outnames[i]] <- data[, vars[i]]
	}

	return(data)
}

df <- make_resid_vars(upf_s_5, upf_s_5_rs, df)


#17.2, nCs are introduced (non consumer)
#19.02 nC change to 0
#make quantiles and binary calssification
df <- df %>%
	mutate(across(all_of(upf_s_5_rs), ~ na_if(., 0))) %>%
	mutate(across(
		all_of(upf_s_5_rs),
		~ factor(ifelse(is.na(.), "nC", ntile(., 4) - 1)),
		.names = "{.col}_q"
	))



#save file
#saveRDS(df, file="data/working_file_w_SG2.rds")

saveRDS(df, file="data/working_file_w_SG3_crude.rds")

#How many people dont consume UPFs?
sum(df$UPF_g_day_noalc == 0)

(df$UPF_s2_rs_q)
summary(df$UPF_s2_rs[df$UPF_animalp_g_day == 0])



