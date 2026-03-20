rm(list=ls())
setwd("C:/Users/kiliang98/phd/EPIC/alysha/analysis")
library(haven)
library(dplyr)
# df <- readRDS("data/working_file2.rds")

#16.2.26 new version, based on alternative definition of animalp subgroup
df <- readRDS("data/working_file3.rds")


#generating energy adjusted UPF subgroups using the residual method-------------

# upf_s_5 <- c( "UPF_beverages_g_day_noalc","UPF_animalp_g_day","UPF_sweets_g_day","UPF_bc_g_day",  "UPF_other_g_day" )
# upf_s_5_rs <- c("UPF_s1_rs", "UPF_s2_rs", "UPF_s3_rs", "UPF_s4_rs", "UPF_s5_rs" )

#16.2.26 new version, based on alternative definition of animalp subgroup
upf_s_5 <- c( "UPF_beverages_g_day_noalc","UPF_animalp_g_day","UPF_sweets_g_day","UPF_bc_g_day",  "UPF_other3_g_day" )
upf_s_5_rs <- c("UPF_s1_rs", "UPF_s2_rs", "UPF_s3_rs", "UPF_s4_rs", "UPF_s5_rs" )

#grouping with PBA included, instead of sweets
upf_sg_5 <- c( "UPF_beverages_g_day_noalc","UPF_animalp_g_day","UPF_PBA_g_day","UPF_bc_g_day",  "UPF_otherv2_g_day" )
upf_sg_5_rs <- c("UPF_sg1_rs", "UPF_sg2_rs", "UPF_sg3_rs", "UPF_sg4_rs", "UPF_sg5_rs" )

#fucntion for residual energy adjustment, receives original vars, names of the vars to be created and the data
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
		~ factor(ifelse(is.na(.), "0", ntile(., 4) - 1)),
		.names = "{.col}_q"
	))


df <- df %>%
	mutate(across(
		all_of(upf_s_5_rs),
		~ factor(ifelse(is.na(.), "0", ntile(., 2) - 1)),
		.names = "{.col}_bin"
	))

#repeat for pba
df <- df %>%
	mutate(across(
		all_of(upf_sg_5_rs),
		~ factor(ifelse(is.na(.), "0", ntile(., 4) - 1)),
		.names = "{.col}_q"
	))


df <- df %>%
	mutate(across(
		all_of(upf_sg_5_rs),
		~ factor(ifelse(is.na(.), "0", ntile(., 2) - 1)),
		.names = "{.col}_bin"
	))

##19.02 make gate variables
df <- df %>%
	mutate(across(
		all_of(upf_s_5_rs),
		~ factor(ifelse(is.na(.), 1,0)),
		.names = "{.col}_qnC"
	))
df <- df %>%
	mutate(across(
		all_of(upf_s_5_rs),
		~ factor(ifelse(is.na(.), 1,0)),
		.names = "{.col}_binnC"
	))
df <- df %>%
	mutate(across(
		all_of(upf_sg_5_rs),
		~ factor(ifelse(is.na(.), 1,0)),
		.names = "{.col}_qnC"
	))
df <- df %>%
	mutate(across(
		all_of(upf_sg_5_rs),
		~ factor(ifelse(is.na(.), 1,0)),
		.names = "{.col}_binnC"
	))






# m_tot <- lm(qe_m_n4_noalc ~ qe_energy + center, data = df)
# df$UPF_tot_rs <- rstandard(m_tot)

upf_sg_5_q <- c("UPF_s1_rs_q", "UPF_s2_rs_q", "UPF_s3_rs_q", "UPF_s4_rs_q", "UPF_s5_rs_q") 


#save file
#saveRDS(df, file="data/working_file_w_SG2.rds")

#16.2.26 new version, based on alternative definition of animalp subgroup
saveRDS(df, file="data/working_file_w_SG3.rds")

#How many people dont consume UPFs?
sum(df$UPF_g_day_noalc == 0)

(df$UPF_s2_rs_q)
summary(df$UPF_s2_rs[df$UPF_animalp_g_day == 0])



