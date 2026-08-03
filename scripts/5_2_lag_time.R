rm(list=ls())

library(survival)
library(dplyr)
library(haven)
library(stringr)
library(forestplot)


df<- readRDS("data/working_file_w_SG3.rds")

#define outcomes
outcome_t <- c( "age_exit_first_cancer", "age_exit_first_CVD", "age_exit_first_T2DM", "age_exit_first_mortality")

df$age_recr<- df$age_recr +2
#add lag time, exclude cancers
for (oct in outcome_t) {
	df<- df[df[[oct]]> df$age_recr, ]
	print(nrow(df))
}

saveRDS(df, file="data/working_file_w_SG3_lag.rds")
