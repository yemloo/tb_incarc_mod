# This script loads necessary packages, data, and model outputs for generating
# all figures, tables, and estimates found in the manuscript.
# Some model output files loaded here are too big to upload to GitHub but are
# available upon request by emailing yiranliu@stanford.edu

library(data.table)
library(ggplot2)
library(ggpubr)
library(ggsci)
library(ggbeeswarm)
library(GGally)
library(gridExtra)
library(ggrepel)
library(tidytext)
library(cowplot)

wd <- '~/Desktop/Stanford/TBprisons_git/'
latam_incarc_data <- fread(paste0(wd, '1-data-and-settings/latam_incarc_data_231203.csv'))
prison_estimates <- fread(paste0(wd, '1-data-and-settings/prison_estimates_latam.csv'))
who_data <- fread(paste0(wd, '1-data-and-settings/who_data_wsplines.csv'))
setorder(who_data, country, year)
setorder(prison_estimates, Country, Year)

wpb.data.interp <- fread(paste0(wd, '1-data-and-settings/wpb_data_region_interp.csv'))
wpp_data <- fread('~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/data/wpp_15pluspop_sixcountries.csv')

analyzed_countries <- c('Argentina','Brazil','Colombia','El Salvador','Mexico','Peru')

incarc_mat_all <- fread(paste0(wd, '3-outputs/incarc_mat_2019_all.csv'))
time_vary_all <- fread(paste0(wd, '3-outputs/time_vary_params_2019.csv'))

pop_main_dt <- readRDS(paste0(wd, '3-outputs/pop_rates.RDS'))
pop_main_dt_summary <- readRDS(paste0(wd, '3-outputs/pop_rates_summary.RDS'))

obs_v_counter_medians <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter_summary.RDS'))
obs_v_counter_sens1_medians <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter_sens1_summary.RDS'))
obs_v_counter_sens2_medians <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter_sens2_summary.RDS'))
obs_v_counter_sens3_medians <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter_sens3_summary.RDS'))
obs_v_counter_sens4_medians <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter_sens4_summary.RDS'))
obs_v_counter_sens5_medians <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter_sens5_summary.RDS'))

obs_v_counter <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter.RDS'))
obs_v_counter_sens1 <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter_sens1.RDS'))
obs_v_counter_sens2 <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter_sens2.RDS'))
obs_v_counter_sens3 <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter_sens3.RDS'))
obs_v_counter_sens4 <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter_sens4.RDS'))
obs_v_counter_sens5 <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter_sens5.RDS'))

sum_incarc_history_medians <- readRDS(paste0(wd, '3-outputs/incarc_hist_TB_obs_v_counter_summary.RDS'))

obs_v_abolition <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_abolition.RDS'))
obs_v_abolition_medians <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_abolition_summary.RDS'))

all_future <- readRDS(paste0(wd, '3-outputs/future_projections.RDS'))
all_future_med <- readRDS(paste0(wd, '3-outputs/future_projections_summary.RDS'))

es_obs_v_noSoE_medians <- readRDS(paste0(wd, '3-outputs/es_obs_v_noSoE_summary.RDS'))
elsal_future <- fread(paste0(wd, '3-outputs/elsalvador_240605_future_combined.csv'))
elsal_future_med <- fread(paste0(wd, '3-outputs/elsalvador_240605_future_combined_summarized.csv'))
elsal_future_pop_med <- fread(paste0(wd, '3-outputs/elsalvador_240605_future_pop_summarized.csv'))

riskfactors_dt <- fread(paste0(wd, '1-data-and-settings/TB_incidence_agegroup_sex_2019.csv'))
riskfactors_dt <- riskfactors_dt[country %in% analyzed_countries]
