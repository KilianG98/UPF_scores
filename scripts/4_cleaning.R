rm(list=ls())
setwd("C:/Users/kiliang98/phd/EPIC/alysha/analysis")

library(haven)
library(tidyr)
library(tidyverse)
library(haven)
library(dplyr)
library(table1)

#df <- readRDS("data/UKB_data_all2.rds")

#16.2.26 new version, based on alternative definition of animalp subgroup
df <- readRDS("data/UKB_data_all3.rds")

################################################################################

# vars <- c("eid", "recruit_date0_0", "education_cat2", "pa_total_mets",
# 												"smoke_stat_0_0", "alc_stat_0_0_touch", "bmi_m_0_0",
# 												"energy_intake_overall", "diet_plausiblerecalls", "score_diet" ,
# 												"menopause_status_0_0", "ever_hrt_0_0", "UPF_g_day_noalc", "UPF_beverages_g_day_noalc", "UPF_sweets_g_day",
# 												"UPF_bc_g_day", "UPF_animalp_g_day", "UPF_other_g_day", "UPF_breads_g_day", 
# 										"UPF_otherv2_g_day", "UPF_PBA_g_day", "sex", "age_recru", "assess_cen"
# 		)

#16.2.26 new version, based on alternative definition of animalp subgroup
vars <- c("eid", "recruit_date0_0", "education_cat2", "pa_total_mets",
												"smoke_stat_0_0", "alc_stat_0_0_touch", "bmi_m_0_0",
												"energy_intake_overall", "diet_plausiblerecalls", "score_diet" ,
												"menopause_status_0_0", "ever_hrt_0_0", "UPF_g_day_noalc", "UPF_beverages_g_day_noalc", "UPF_sweets_g_day",
												"UPF_bc_g_day", "UPF_animalp_g_day", "UPF_other_g_day", "UPF_breads_g_day",
										"UPF_otherv2_g_day", "UPF_PBA_g_day", "sex", "age_recru", "assess_cen", "UPF_other3_g_day","UPF_animalp2_g_day"
		)
#selfreported and hopsital inpatient prevalences
prevalence<- c("prevalent_ca", "sr_cancer_0", "sr_cvd_0", "cvd_prev", "t2dm_prev", "sr_t2dm_0" )

outcome_status <- c("obesity_cancer", "allcvd_inc", "t2dm_inc" , "total_mortality" )
outcome_time <- c( "age_exit_first_cancer", "age_exit_first_CVD", "age_exit_first_T2DM", "age_exit_first_mortality")

keep <- c(vars,prevalence, outcome_status, outcome_time)

#kepp only necesssary vars
df2 <- df %>% 
	select(any_of(keep))



#exclude all prevalences
	for (p in prevalence) {
		print(nrow(df2))
		filtered_out <- nrow(df2) - nrow(df2 <- df2 %>%
																																				filter(!!sym(p) == 0))
		print(paste("Filtered out: ",p, "  ", filtered_out)) # 0
	}

#filter missing covars
--------------------------------------------------------------------------------
	
#energy and diet score have no missings
sum(is.na(df2$energy_intake_overall)) #0
sum(is.na(df2$score_diet))#0
	
# education_cat2
# 1	Low 
# 2	Medium
# 3	High
# 9	Missing
sum(is.na(df2$education_cat2))#0
print(nrow(df2))
filtered_out <- nrow(df2) - nrow(df2 <- df2 %>%
																															filter(education_cat2 != 9))
print(paste("Filtered out: Education ", filtered_out))


#physicl activiy is numeric
sum(is.na(df2$pa_total_mets)) #2812
print(nrow(df2))
filtered_out <- nrow(df2) - nrow(df2 <- df2 %>%
																																		filter(!is.na(pa_total_mets)))
print(paste("Filtered out: PA ", filtered_out))

# smoke_stat_0_0	
# 0	Never
# 1	Previous
# 2	Current 
# 9	Missing 
sum(is.na(df2$smoke_stat_0_0)) #0
print(nrow(df2))
filtered_out <- nrow(df2) - nrow(df2 <- df2 %>%
																																		filter(smoke_stat_0_0 != 9))
print(paste("Filtered out: Smoke ", filtered_out))

# Alc_stat_0_0_touch	
# 0	Never
# 1	Former
# 2	Current
# 9	Missing
sum(is.na(df$alc_stat_0_0_touch)) #0
print(nrow(df2))
filtered_out <- nrow(df2) - nrow(df2 <- df2 %>%
																																		filter(alc_stat_0_0_touch != 9))
print(paste("Filtered out: ALC ", filtered_out))

# 0	No
# 1	Yes
# 2	Not sure – had a hysterectomy 
# 3	Not sure – other reason
# 9	Missing
# 99	Men 
sum(is.na(df$menopause_status_0_0)) #0
print(nrow(df2))
filtered_out <- nrow(df2) - nrow(df2 <- df2 %>%
																																		filter(menopause_status_0_0 != 9))
print(paste("Filtered out: MENO ", filtered_out))

# ever_hrt_0_0	
# 0	No
# 1 Yes
sum(is.na(df2$ever_hrt_0_0)) #0
sum(is.na(df2$ever_hrt_0_0[df2$sex == "F"])) #176
sum(!is.na(df2$ever_hrt_0_0[df2$sex == "M"])) #0
print(nrow(df2))
filtered_out <- nrow(df2) - nrow(df2 <- df2 %>%
																																		filter(!(is.na(ever_hrt_0_0) & sex == "F" )))
print(paste("Filtered out: HRT ", filtered_out))

df2$ever_hrt_0_0[ df2$sex == "M" & is.na(df2$ever_hrt_0_0) ] <- 99

# sum(is.na(df2$bmi_m_0_0)) #375
# print(nrow(df2))
# filtered_out <- nrow(df2) - nrow(df2 <- df2 %>%
# 																																		filter(!(is.na(bmi_m_0_0) )))
# print(paste("Filtered out: BMI ", filtered_out))



################################################################################


df2$age_cat_5yr <- cut(
	df2$age_recru,
	breaks = seq(floor(min(df2$age_recru)), ceiling(max(df2$age_recru)) + 5, by = 5),
	right = TRUE,
	include.lowest = TRUE
)

df2$ever_hrt_0_0<- as.factor(df2$ever_hrt_0_0)
df2$menopause_status_0_0<- as.factor(df2$menopause_status_0_0)
df2$alc_stat_0_0_touch <- as.factor(df2$alc_stat_0_0_touch)
df2$smoke_stat_0_0 <- as.factor(df2$smoke_stat_0_0)


# saveRDS(df2, "data/working_file2.rds")

#16.2.26 new version, based on alternative definition of animalp subgroup
saveRDS(df2, "data/working_file3.rds")


################################################################################

df <- readRDS("data/working_file_w_SG2.rds")

df$age_exit <-rowMeans(
	df[,c( "age_exit_first_cancer", "age_exit_first_CVD", "age_exit_first_T2DM", "age_exit_first_mortality")],
	na.rm=T)
df$years_follow <- df$age_exit - df$age_recru
summary(df$years_follow)
sum(df$obesity_cancer)
sum(df$allcvd_inc)
sum(df$t2dm_inc)
sum(df$total_mortality)

df$UPF_intake_q <- ntile(df$UPF_g_day_noalc,4)

df <- droplevels(df)
df$education_cat2 <- as.factor(df$education_cat2)
df$energy_intake_overall <- df$energy_intake_overall/4.184

upf_s_5 <- c( "UPF_beverages_g_day_noalc","UPF_animalp_g_day","UPF_sweets_g_day","UPF_bc_g_day",  "UPF_other_g_day" )

tbl_vars <- c("UPF_g_day_noalc",upf_s_5,"age_recru","sex","energy_intake_overall","score_diet",
														"bmi_m_0_0",  "alc_stat_0_0_touch","smoke_stat_0_0",  "pa_total_mets", "education_cat2" )
formula <- as.formula(paste("~", paste(tbl_vars, collapse = " + "), "| as.factor(UPF_intake_q)"))

rndr <- function(x, ...) {
	if (is.logical(x)) {
		y <- render.default(x, ...)
		y[2]  # Keep the second element for logicals
	} else if (is.numeric(x)) {
		# Ensure Mean (SD) is calculated and rounded properly
		mean_x <- mean(x, na.rm = TRUE)
		sd_x <- sd(x, na.rm = TRUE)
		sprintf("%.1f (%.1f)", mean_x, sd_x)  # Round both mean and SD to 1 decimal
	} else {
		render.default(x, ...)
	}
}




tbl<-table1(formula, data=df, render=rndr)
# Convert to data frame
tbl_df <- as.data.frame(tbl)

write.csv(tbl_df, "results/table1_mf.csv", row.names = FALSE)

df_f <- df[df$sex == "F",]
df_f <- droplevels((df_f))
tbl_vars_f <-c("ever_hrt_0_0", "menopause_status_0_0")

formula_f <- as.formula(paste("~", paste(tbl_vars_f, collapse = " + "), "| as.factor(UPF_intake_q)"))


tbl_f<-table1(formula_f, data=df_f, render=rndr)
tbl_df_f <- as.data.frame(tbl_f)
# Save as CSV
write.csv(tbl_df_f, "results/table1_f.csv", row.names = FALSE)

