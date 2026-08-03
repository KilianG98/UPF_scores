
rm(list=ls())

library(survival)
library(dplyr)
library(haven)
library(stringr)
library(forestplot)
library(rms)

setwd("C:/Users/kiliang98/phd/EPIC/alysha/analysis/UPF_scores")


df<- readRDS("data/working_file_w_SG3.rds")
plot_df <- read.csv("results/plot_df_ukb_nogate.csv", row.names = 1) 
list_of_plot_dfs <- readRDS("results/list_of_plot_dfs_nogate.rds")

expo_cols <- grepl("rs_q", names(plot_df)) 
expo_df <- plot_df[, expo_cols]

for (i in seq_len(nrow(expo_df))){
	tmp_df <- df
		for (col in names(expo_df)){
			print(col)
			tmp_df <- tmp_df[tmp_df[[col]] == expo_df[[i, col]],]
			print(nrow(tmp_df))
		}
	print(nrow(tmp_df))
}



library(dplyr)

# 1. Convert all columns in your smaller combination dataframe to characters
expo_df_char <- expo_df %>% 
	mutate(across(everything(), as.character))

# 2. Convert the matching columns in 'df' to characters, then join!
matched_df <- df %>%
	mutate(across(all_of(names(expo_df)), as.character)) %>%
	inner_join(expo_df_char, by = names(expo_df))

# 3. Count exactly how many times each combination appeared
final_counts <- matched_df %>%
	count(across(all_of(names(expo_df))))

plot_df$n_exposed <- final_counts$n

for(name in names(list_of_plot_dfs)){
	list_of_plot_dfs[[name]]$n_exposed <- final_counts$n
	
	write.csv(list_of_plot_dfs[[name]], paste0("results/plot_df_ukb_nogate_", name, ".csv" ))
}

