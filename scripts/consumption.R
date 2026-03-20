rm(list=ls())

library(survival)
library(dplyr)
library(haven)
library(stringr)
library(forestplot)

setwd("C:/Users/kiliang98/phd/EPIC/alysha/analysis")

source("scripts/X_analysis_functions.R")

# df<- readRDS("data/working_file_w_SG2.rds")

#16.2.26 new version, based on alternative definition of animalp subgroup
df<- readRDS("data/working_file_w_SG3.rds")



upf_s_5 <- c( "UPF_beverages_g_day_noalc","UPF_animalp2_g_day","UPF_sweets_g_day","UPF_bc_g_day",  "UPF_other3_g_day" )



sum(df$UPF_beverages_g_day_noalc > 0)
mean ( df$ UPF_beverages_g_day_noalc)


sum(df$UPF_animalp2_g_day > 0)
mean ( df$ UPF_animalp2_g_day)

sum(df$UPF_animalp_g_day > 0)
mean ( df$ UPF_animalp_g_day)

sum(df$UPF_sweets_g_day > 0)
mean ( df$ UPF_sweets_g_day)

sum(df$UPF_bc_g_day > 0)
mean ( df$ UPF_bc_g_day)

sum(df$UPF_other3_g_day > 0)
mean ( df$ UPF_other3_g_day)


sum(df$UPF_otherv2_g_day > 0)
mean ( df$ UPF_otherv2_g_day)

sum(df$UPF_PBA_g_day > 0)
mean ( df$UPF_PBA_g_day)


