library(data.table)
wd <- '~/Desktop/Stanford/TBprisons_git/'
analyzed_countries <- c('Argentina','Brazil','Colombia','El Salvador','Mexico','Peru')

# bind incarc outputs from abolition_sherlock_240605.R
incarc_mat_arg <- readRDS('~/Desktop/Stanford/TBprisons_git/3-outputs/incarc_mat_argentina_240617.RDS')
incarc_mat_arg <- rbindlist(incarc_mat_arg[-which(sapply(incarc_mat_arg, is.null))])
incarc_mat_arg$country <- 'Argentina'

incarc_mat_bra <- readRDS('~/Desktop/Stanford/TBprisons_git/3-outputs/incarc_mat_brazil_240603.RDS')
incarc_mat_bra <- rbindlist(incarc_mat_bra[-which(sapply(incarc_mat_bra, is.null))])
incarc_mat_bra$country <- 'Brazil'

incarc_mat_col <- readRDS('~/Desktop/Stanford/TBprisons_git/3-outputs/incarc_mat_colombia_240612.RDS')
incarc_mat_col <- rbindlist(incarc_mat_col[-which(sapply(incarc_mat_col, is.null))])
incarc_mat_col$country <- 'Colombia'

incarc_mat_es <- readRDS('~/Desktop/Stanford/TBprisons_git/3-outputs/incarc_mat_elsalvador_240605.RDS')
incarc_mat_es <- rbindlist(incarc_mat_es[-which(sapply(incarc_mat_es, is.null))])
incarc_mat_es$country <- 'El Salvador'

incarc_mat_mex <- readRDS('~/Desktop/Stanford/TBprisons_git/3-outputs/incarc_mat_mexico_240611.RDS')
incarc_mat_mex <- rbindlist(incarc_mat_mex[-which(sapply(incarc_mat_mex, is.null))])
incarc_mat_mex$country <- 'Mexico'

incarc_mat_per <- readRDS('~/Desktop/Stanford/TBprisons_git/3-outputs/incarc_mat_peru_240612.RDS')
incarc_mat_per <- rbindlist(incarc_mat_per[-which(sapply(incarc_mat_per, is.null))])
incarc_mat_per$country <- 'Peru'

incarc_mat_all <- rbind(incarc_mat_arg[time == 2019],
                        incarc_mat_bra[time == 2019],
                        incarc_mat_col[time == 2019],
                        incarc_mat_es[time == 2019],
                        incarc_mat_mex[time == 2019],
                        incarc_mat_per[time == 2019])
write.csv(incarc_mat_all, paste0(wd, '3-outputs/incarc_mat_2019_all.csv'), row.names = F)

# bind time-varying params for all countries
time_vary_arg <- fread('~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/params/argentina_240617_params_overtime.csv')
time_vary_arg$country <- 'Argentina'

time_vary_bra <- fread('~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/params/brazil_240603_params_overtime.csv')
time_vary_bra$country <- 'Brazil'

time_vary_col <- fread('~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/params/colombia_240612_params_overtime.csv')
time_vary_col$country <- 'Colombia'

time_vary_es <- fread('~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/params/elsalvador_240605_params_overtime.csv')
time_vary_es$country <- 'El Salvador'

time_vary_mex <- fread('~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/params/mexico_240611_params_overtime.csv')
time_vary_mex$country <- 'Mexico'

time_vary_per <- fread('~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/params/peru_240612_params_overtime.csv')
time_vary_per$country <- 'Peru'

time_vary_all <- rbind(time_vary_arg[t == 2019],
                        time_vary_bra[t == 2019],
                        time_vary_col[t == 2019],
                        time_vary_es[t == 2019],
                        time_vary_mex[t == 2019],
                        time_vary_per[t == 2019])
write.csv(time_vary_all, paste0(wd, '3-outputs/time_vary_params_2019.csv'), row.names = F)

# Process historical counterfactual outputs
all_outputs <- lapply(setdiff(list.files(paste0(wd, '3-outputs'),
                                 pattern = "good_outputs", full.names = T),
                              list.files(paste0(wd, '3-outputs'),
                                         pattern = "sens", full.names = T)), readRDS)

# Population prevalences
all_outputs_pop_main <- list()
for (i in 1:length(all_outputs)){
  all_outputs[[i]]$country <- analyzed_countries[i]
  all_outputs_pop_main[[i]] <- all_outputs[[i]][variable == 'pop' & 
                                                  analysis == 'main analysis' &
                                                  pop %in% c('Prison',
                                                             'Post-Release',
                                                             'Formerly Incarc')]
}
pop_main_dt <- rbindlist(all_outputs_pop_main, use.names = T)

pop_main_dt[, pop_hist := ifelse(pop == 'Prison','Current Incarc.','History of Incarc.')]
pop_main_dt_sum <- pop_main_dt[,.(rate_per100k=sum(rate_per100k)),
                               by=c('pop_hist','time','scenario','country','idx')]
pop_main_dt_summary <- pop_main_dt_sum[,.(rate_per100k=median(rate_per100k),
                                      rate_per100k.hi=quantile(rate_per100k, 0.975),
                                      rate_per100k.lo=quantile(rate_per100k, 0.025)),
                                   by=c('pop_hist','time','scenario','country')]
saveRDS(pop_main_dt, paste0(wd, '3-outputs/pop_rates.RDS'))
saveRDS(pop_main_dt_summary, paste0(wd, '3-outputs/pop_rates_summary.RDS'))

# Main analysis: excess burden attributable to increasing incarc since 1990
all_outputs_main <- list()
for (i in 1:length(all_outputs)){
  all_outputs[[i]]$country <- analyzed_countries[i]
  all_outputs_main[[i]] <- all_outputs[[i]][variable %in% c('D','I') &
                                                  analysis == 'main analysis']
}
all_outputs_main_dt <- rbindlist(all_outputs_main, use.names = T)

obs_v_counter <- merge(all_outputs_main_dt[scenario == 'Leveled Off' & time <= 2023,
                                               c('idx','pop','variable','time','rate_per100k','total','country')],
                        all_outputs_main_dt[scenario == 'No Growth Since 1990' & time <= 2023,
                                               c('idx','pop','variable','time','rate_per100k','total','country')], 
                        by=c('idx','pop','variable','time','country'),
                        suffixes = c('.observed','.counterfct'))

obs_v_counter[,RR.obs.v.counter := total.observed / total.counterfct]
obs_v_counter[,excess.perc := RR.obs.v.counter * 100 - 100]
obs_v_counter[,RD.obs.v.counter := total.observed - total.counterfct]

obs_v_counter_medians <- obs_v_counter[,.(RR.obs.v.counter=median(RR.obs.v.counter),
                                          RR.obs.v.counter.hi=quantile(RR.obs.v.counter, 0.975, na.rm=T),
                                          RR.obs.v.counter.lo=quantile(RR.obs.v.counter, 0.025, na.rm=T),
                                            excess.perc=median(excess.perc),
                                            excess.perc.hi=quantile(excess.perc,0.975, na.rm=T),
                                            excess.perc.lo=quantile(excess.perc,0.025, na.rm=T),
                                            RD.obs.v.counter=median(RD.obs.v.counter),
                                            RD.obs.v.counter.hi=quantile(RD.obs.v.counter, 0.975, na.rm=T),
                                            RD.obs.v.counter.lo=quantile(RD.obs.v.counter, 0.025, na.rm=T),
                                            rate_per100k.observed=median(rate_per100k.observed),
                                            rate_per100k.observed.hi=quantile(rate_per100k.observed, 0.975),
                                            rate_per100k.observed.lo=quantile(rate_per100k.observed, 0.025),
                                            rate_per100k.counterfct=median(rate_per100k.counterfct),
                                            rate_per100k.counterfct.hi=quantile(rate_per100k.counterfct, 0.975),
                                            rate_per100k.counterfct.lo=quantile(rate_per100k.counterfct, 0.025),
                                            total.observed=median(total.observed),
                                            total.counterfct=median(total.counterfct)),
                                         by=c('variable','time','country','pop')]

saveRDS(obs_v_counter, paste0(wd, '3-outputs/total_TB_obs_v_counter.RDS'))
saveRDS(obs_v_counter_medians, paste0(wd, '3-outputs/total_TB_obs_v_counter_summary.RDS'))

# El Salvador: no SoE scenario
es_obs_v_noSoE <- merge(all_outputs_main_dt[scenario == 'Leveled Off' & time >= 2021 & time <= 2025 &
                                              country == 'El Salvador',
                                           c('idx','pop','variable','time','rate_per100k','total')],
                       all_outputs_main_dt[scenario == 'No SoE' & time >= 2021 & time <= 2025 &
                                             country == 'El Salvador',
                                           c('idx','pop','variable','time','rate_per100k','total')], 
                       by=c('idx','pop','variable','time'),
                       suffixes = c('.observed','.noSoE'))

es_obs_v_noSoE[,RR.obs.v.counter := total.observed / total.noSoE]
es_obs_v_noSoE[,excess.perc := RR.obs.v.counter * 100 - 100]
es_obs_v_noSoE[,RD.obs.v.counter := total.observed - total.noSoE]

es_obs_v_noSoE_medians <- es_obs_v_noSoE[,.(RR.obs.v.counter=median(RR.obs.v.counter),
                                          RR.obs.v.counter.hi=quantile(RR.obs.v.counter, 0.975, na.rm=T),
                                          RR.obs.v.counter.lo=quantile(RR.obs.v.counter, 0.025, na.rm=T),
                                          excess.perc=median(excess.perc),
                                          excess.perc.hi=quantile(excess.perc,0.975, na.rm=T),
                                          excess.perc.lo=quantile(excess.perc,0.025, na.rm=T),
                                          RD.obs.v.counter=median(RD.obs.v.counter),
                                          RD.obs.v.counter.hi=quantile(RD.obs.v.counter, 0.975, na.rm=T),
                                          RD.obs.v.counter.lo=quantile(RD.obs.v.counter, 0.025, na.rm=T),
                                          rate_per100k.observed=median(rate_per100k.observed),
                                          rate_per100k.observed.hi=quantile(rate_per100k.observed, 0.975),
                                          rate_per100k.observed.lo=quantile(rate_per100k.observed, 0.025),
                                          rate_per100k.noSoE=median(rate_per100k.noSoE),
                                          rate_per100k.noSoE.hi=quantile(rate_per100k.noSoE, 0.975),
                                          rate_per100k.noSoE.lo=quantile(rate_per100k.noSoE, 0.025),
                                          total.observed=median(total.observed),
                                          total.noSoE=median(total.noSoE)),
                                       by=c('variable','time','pop')]

saveRDS(es_obs_v_noSoE, paste0(wd, '3-outputs/es_obs_v_noSoE.RDS'))
saveRDS(es_obs_v_noSoE_medians, paste0(wd, '3-outputs/es_obs_v_noSoE_summary.RDS'))

# excess cases among ever-exposed (current and past incarc)
sum_history <- obs_v_counter[pop %in% c('Prison','Post-Release','Formerly Incarc'),
                             .(RD.obs.v.counter=sum(RD.obs.v.counter)),
                             by=c('idx','variable','time','country')]
sum_history_medians <- sum_history[,.(RD.obs.v.counter=median(RD.obs.v.counter)),
                                                 by=c('variable','time','country')]
sum_history_medians <- merge(sum_history_medians, 
                     obs_v_counter_medians[pop == 'Combined',
                                                c('time','country','variable','RD.obs.v.counter'),with=F],
                                    by=c('time','country','variable'),
                                    suffixes = c('','.total'))
sum_history_medians[RD.obs.v.counter >= 0, RD.obs.v.counter.adj := pmin(RD.obs.v.counter, RD.obs.v.counter.total)]
sum_history_medians[RD.obs.v.counter < 0, RD.obs.v.counter.adj := RD.obs.v.counter]

sum_history_medians[,perc.RD.history := RD.obs.v.counter / RD.obs.v.counter.total]

saveRDS(sum_history, paste0(wd, '3-outputs/incarc_hist_TB_obs_v_counter.RDS'))
saveRDS(sum_history_medians, paste0(wd, '3-outputs/incarc_hist_TB_obs_v_counter_summary.RDS'))

# proportion of excess burden among recent v distant history
comp_history <- merge(obs_v_counter[variable == 'I' & time == 2019 & pop %in% c('Post-Release','Formerly Incarc'),
                                    c('pop','idx','time','country','variable','RD.obs.v.counter'),with=F],
                      obs_v_counter[variable == 'I' & time == 2019 &  pop == 'Combined',
                                            c('idx','time','country','variable','RD.obs.v.counter'),with=F],
                      by=c('idx','time','country','variable'),
                      suffixes = c('','.total'))
saveRDS(comp_history, paste0(wd, '3-outputs/comp_hist_TB_obs_v_counter.RDS'))

# Sensitivity analyses
# sens 1
all_outputs_sens1 <- lapply(intersect(list.files(paste0(wd, '3-outputs'),
                                         pattern = "good_outputs", full.names = T),
                              list.files(paste0(wd, '3-outputs'),
                                         pattern = "sens1", full.names = T)), readRDS)
for (i in 1:length(all_outputs_sens1)){
  all_outputs_sens1[[i]]$country <- c('Argentina','Brazil','Mexico')[i] # only 3 countries w sens 1
  all_outputs_sens1[[i]] <- all_outputs_sens1[[i]][variable %in% c('D','I') &
                                               analysis == 'no change in beta_pp']
}
all_outputs_sens1_dt <- rbindlist(all_outputs_sens1, use.names = T)
obs_v_counter_sens1 <- merge(all_outputs_sens1_dt[scenario == 'Leveled Off' & time <= 2023,
                                                  c('idx','pop','variable','time','rate_per100k','total','country')],
                             all_outputs_sens1_dt[scenario == 'No Growth Since 1990' & time <= 2023,
                                                  c('idx','pop','variable','time','rate_per100k','total','country')], 
                             by=c('idx','pop','variable','time','country'),
                             suffixes = c('.observed','.counterfct'))

obs_v_counter_sens1[,RR.obs.v.counter := total.observed / total.counterfct]
obs_v_counter_sens1[,excess.perc := RR.obs.v.counter * 100 - 100]
obs_v_counter_sens1[,RD.obs.v.counter := total.observed - total.counterfct]

obs_v_counter_sens1_medians <- obs_v_counter_sens1[,.(RR.obs.v.counter=median(RR.obs.v.counter),
                                                      RR.obs.v.counter.hi=quantile(RR.obs.v.counter, 0.975, na.rm=T),
                                                      RR.obs.v.counter.lo=quantile(RR.obs.v.counter, 0.025, na.rm=T),
                                                      excess.perc=median(excess.perc),
                                                      excess.perc.hi=quantile(excess.perc,0.975, na.rm=T),
                                                      excess.perc.lo=quantile(excess.perc,0.025, na.rm=T),
                                                      RD.obs.v.counter=median(RD.obs.v.counter),
                                                      RD.obs.v.counter.hi=quantile(RD.obs.v.counter, 0.975, na.rm=T),
                                                      RD.obs.v.counter.lo=quantile(RD.obs.v.counter, 0.025, na.rm=T),
                                                      rate_per100k.observed=median(rate_per100k.observed),
                                                      rate_per100k.observed.hi=quantile(rate_per100k.observed, 0.975),
                                                      rate_per100k.observed.lo=quantile(rate_per100k.observed, 0.025),
                                                      rate_per100k.counterfct=median(rate_per100k.counterfct),
                                                      rate_per100k.counterfct.hi=quantile(rate_per100k.counterfct, 0.975),
                                                      rate_per100k.counterfct.lo=quantile(rate_per100k.counterfct, 0.025),
                                                      total.observed=median(total.observed),
                                                      total.counterfct=median(total.counterfct)),
                                                   by=c('variable','time','country','pop')]

saveRDS(obs_v_counter_sens1, paste0(wd, '3-outputs/total_TB_obs_v_counter_sens1.RDS'))
saveRDS(obs_v_counter_sens1_medians, paste0(wd, '3-outputs/total_TB_obs_v_counter_sens1_summary.RDS'))
remove(list = c('all_outputs_sens1', 'all_outputs_sens1_dt', 'obs_v_counter_sens1'))

# sens 2
all_outputs_sens2 <- lapply(intersect(list.files(paste0(wd, '3-outputs'),
                                                 pattern = "good_outputs", full.names = T),
                                      list.files(paste0(wd, '3-outputs'),
                                                 pattern = "sens2", full.names = T)), readRDS)
for (i in 1:length(all_outputs_sens2)){
  all_outputs_sens2[[i]]$country <- analyzed_countries[i] 
  all_outputs_sens2[[i]] <- all_outputs_sens2[[i]][variable %in% c('D','I')]
}
all_outputs_sens2_dt <- rbindlist(all_outputs_sens2, use.names = T)

obs_v_counter_sens2 <- merge(all_outputs_sens2_dt[scenario == 'Leveled Off' & time <= 2023,
                                           c('idx','pop','variable','time','rate_per100k','total','country')],
                             all_outputs_sens2_dt[scenario == 'No Growth Since 1990' & time <= 2023,
                                           c('idx','pop','variable','time','rate_per100k','total','country')], 
                       by=c('idx','pop','variable','time','country'),
                       suffixes = c('.observed','.counterfct'))

obs_v_counter_sens2[,RR.obs.v.counter := total.observed / total.counterfct]
obs_v_counter_sens2[,excess.perc := RR.obs.v.counter * 100 - 100]
obs_v_counter_sens2[,RD.obs.v.counter := total.observed - total.counterfct]

obs_v_counter_sens2_medians <- obs_v_counter_sens2[,.(RR.obs.v.counter=median(RR.obs.v.counter),
                                          RR.obs.v.counter.hi=quantile(RR.obs.v.counter, 0.975, na.rm=T),
                                          RR.obs.v.counter.lo=quantile(RR.obs.v.counter, 0.025, na.rm=T),
                                          excess.perc=median(excess.perc),
                                          excess.perc.hi=quantile(excess.perc,0.975, na.rm=T),
                                          excess.perc.lo=quantile(excess.perc,0.025, na.rm=T),
                                          RD.obs.v.counter=median(RD.obs.v.counter),
                                          RD.obs.v.counter.hi=quantile(RD.obs.v.counter, 0.975, na.rm=T),
                                          RD.obs.v.counter.lo=quantile(RD.obs.v.counter, 0.025, na.rm=T),
                                          rate_per100k.observed=median(rate_per100k.observed),
                                          rate_per100k.observed.hi=quantile(rate_per100k.observed, 0.975),
                                          rate_per100k.observed.lo=quantile(rate_per100k.observed, 0.025),
                                          rate_per100k.counterfct=median(rate_per100k.counterfct),
                                          rate_per100k.counterfct.hi=quantile(rate_per100k.counterfct, 0.975),
                                          rate_per100k.counterfct.lo=quantile(rate_per100k.counterfct, 0.025),
                                          total.observed=median(total.observed),
                                          total.counterfct=median(total.counterfct)),
                                       by=c('variable','time','country','pop')]

saveRDS(obs_v_counter_sens2, paste0(wd, '3-outputs/total_TB_obs_v_counter_sens2.RDS'))
saveRDS(obs_v_counter_sens2_medians, paste0(wd, '3-outputs/total_TB_obs_v_counter_sens2_summary.RDS'))
remove(list = c('all_outputs_sens2', 'all_outputs_sens2_dt', 'obs_v_counter_sens2'))

# sens 3
all_outputs_sens3 <- lapply(intersect(list.files(paste0(wd, '3-outputs'),
                                                 pattern = "good_outputs", full.names = T),
                                      list.files(paste0(wd, '3-outputs'),
                                                 pattern = "sens3", full.names = T)), readRDS)
for (i in 1:length(all_outputs_sens3)){
  all_outputs_sens3[[i]]$country <- analyzed_countries[i] 
  all_outputs_sens3[[i]] <- all_outputs_sens3[[i]][variable %in% c('D','I')]
}
all_outputs_sens3_dt <- rbindlist(all_outputs_sens3, use.names = T)

obs_v_counter_sens3 <- merge(all_outputs_sens3_dt[scenario == 'Leveled Off' & time <= 2023,
                                                  c('idx','pop','variable','time','rate_per100k','total','country')],
                             all_outputs_sens3_dt[scenario == 'No Growth Since 1990' & time <= 2023,
                                                  c('idx','pop','variable','time','rate_per100k','total','country')], 
                             by=c('idx','pop','variable','time','country'),
                             suffixes = c('.observed','.counterfct'))

obs_v_counter_sens3[,RR.obs.v.counter := total.observed / total.counterfct]
obs_v_counter_sens3[,excess.perc := RR.obs.v.counter * 100 - 100]
obs_v_counter_sens3[,RD.obs.v.counter := total.observed - total.counterfct]

obs_v_counter_sens3_medians <- obs_v_counter_sens3[,.(RR.obs.v.counter=median(RR.obs.v.counter),
                                                      RR.obs.v.counter.hi=quantile(RR.obs.v.counter, 0.975, na.rm=T),
                                                      RR.obs.v.counter.lo=quantile(RR.obs.v.counter, 0.025, na.rm=T),
                                                      excess.perc=median(excess.perc),
                                                      excess.perc.hi=quantile(excess.perc,0.975, na.rm=T),
                                                      excess.perc.lo=quantile(excess.perc,0.025, na.rm=T),
                                                      RD.obs.v.counter=median(RD.obs.v.counter),
                                                      RD.obs.v.counter.hi=quantile(RD.obs.v.counter, 0.975, na.rm=T),
                                                      RD.obs.v.counter.lo=quantile(RD.obs.v.counter, 0.025, na.rm=T),
                                                      rate_per100k.observed=median(rate_per100k.observed),
                                                      rate_per100k.observed.hi=quantile(rate_per100k.observed, 0.975),
                                                      rate_per100k.observed.lo=quantile(rate_per100k.observed, 0.025),
                                                      rate_per100k.counterfct=median(rate_per100k.counterfct),
                                                      rate_per100k.counterfct.hi=quantile(rate_per100k.counterfct, 0.975),
                                                      rate_per100k.counterfct.lo=quantile(rate_per100k.counterfct, 0.025),
                                                      total.observed=median(total.observed),
                                                      total.counterfct=median(total.counterfct)),
                                                   by=c('variable','time','country','pop')]

saveRDS(obs_v_counter_sens3, paste0(wd, '3-outputs/total_TB_obs_v_counter_sens3.RDS'))
saveRDS(obs_v_counter_sens3_medians, paste0(wd, '3-outputs/total_TB_obs_v_counter_sens3_summary.RDS'))
remove(list = c('all_outputs_sens3', 'all_outputs_sens3_dt', 'obs_v_counter_sens3'))

# sens 4
all_outputs_sens4 <- lapply(intersect(list.files(paste0(wd, '3-outputs'),
                                                 pattern = "good_outputs", full.names = T),
                                      list.files(paste0(wd, '3-outputs'),
                                                 pattern = "sens4", full.names = T)), readRDS)
for (i in 1:length(all_outputs_sens4)){
  all_outputs_sens4[[i]]$country <- analyzed_countries[i] 
  all_outputs_sens4[[i]] <- all_outputs_sens4[[i]][variable %in% c('D','I')]
}
all_outputs_sens4_dt <- rbindlist(all_outputs_sens4, use.names = T)

obs_v_counter_sens4 <- merge(all_outputs_sens4_dt[scenario == 'Leveled Off' & time <= 2023,
                                                  c('idx','pop','variable','time','rate_per100k','total','country')],
                             all_outputs_sens4_dt[scenario == 'No Growth Since 1990' & time <= 2023,
                                                  c('idx','pop','variable','time','rate_per100k','total','country')], 
                             by=c('idx','pop','variable','time','country'),
                             suffixes = c('.observed','.counterfct'))

obs_v_counter_sens4[,RR.obs.v.counter := total.observed / total.counterfct]
obs_v_counter_sens4[,excess.perc := RR.obs.v.counter * 100 - 100]
obs_v_counter_sens4[,RD.obs.v.counter := total.observed - total.counterfct]

obs_v_counter_sens4_medians <- obs_v_counter_sens4[,.(RR.obs.v.counter=median(RR.obs.v.counter),
                                                      RR.obs.v.counter.hi=quantile(RR.obs.v.counter, 0.975, na.rm=T),
                                                      RR.obs.v.counter.lo=quantile(RR.obs.v.counter, 0.025, na.rm=T),
                                                      excess.perc=median(excess.perc),
                                                      excess.perc.hi=quantile(excess.perc,0.975, na.rm=T),
                                                      excess.perc.lo=quantile(excess.perc,0.025, na.rm=T),
                                                      RD.obs.v.counter=median(RD.obs.v.counter),
                                                      RD.obs.v.counter.hi=quantile(RD.obs.v.counter, 0.975, na.rm=T),
                                                      RD.obs.v.counter.lo=quantile(RD.obs.v.counter, 0.025, na.rm=T),
                                                      rate_per100k.observed=median(rate_per100k.observed),
                                                      rate_per100k.observed.hi=quantile(rate_per100k.observed, 0.975),
                                                      rate_per100k.observed.lo=quantile(rate_per100k.observed, 0.025),
                                                      rate_per100k.counterfct=median(rate_per100k.counterfct),
                                                      rate_per100k.counterfct.hi=quantile(rate_per100k.counterfct, 0.975),
                                                      rate_per100k.counterfct.lo=quantile(rate_per100k.counterfct, 0.025),
                                                      total.observed=median(total.observed),
                                                      total.counterfct=median(total.counterfct)),
                                                   by=c('variable','time','country','pop')]

saveRDS(obs_v_counter_sens4, paste0(wd, '3-outputs/total_TB_obs_v_counter_sens4.RDS'))
saveRDS(obs_v_counter_sens4_medians, paste0(wd, '3-outputs/total_TB_obs_v_counter_sens4_summary.RDS'))
remove(list = c('all_outputs_sens4', 'all_outputs_sens4_dt', 'obs_v_counter_sens4'))

# sens 5
all_outputs_sens5 <- lapply(intersect(list.files(paste0(wd, '3-outputs'),
                                                 pattern = "good_outputs", full.names = T),
                                      list.files(paste0(wd, '3-outputs'),
                                                 pattern = "sens5", full.names = T)), readRDS)
for (i in 1:length(all_outputs_sens5)){
  all_outputs_sens5[[i]]$country <- analyzed_countries[i] 
  all_outputs_sens5[[i]] <- all_outputs_sens5[[i]][variable %in% c('D','I')]
}
all_outputs_sens5_dt <- rbindlist(all_outputs_sens5, use.names = T)

obs_v_counter_sens5 <- merge(all_outputs_sens5_dt[scenario == 'Leveled Off' & time <= 2023,
                                                  c('idx','pop','variable','time','rate_per100k','total','country')],
                             all_outputs_sens5_dt[scenario == 'No Growth Since 1990' & time <= 2023,
                                                  c('idx','pop','variable','time','rate_per100k','total','country')], 
                             by=c('idx','pop','variable','time','country'),
                             suffixes = c('.observed','.counterfct'))

obs_v_counter_sens5[,RR.obs.v.counter := total.observed / total.counterfct]
obs_v_counter_sens5[,excess.perc := RR.obs.v.counter * 100 - 100]
obs_v_counter_sens5[,RD.obs.v.counter := total.observed - total.counterfct]

obs_v_counter_sens5_medians <- obs_v_counter_sens5[,.(RR.obs.v.counter=median(RR.obs.v.counter),
                                                      RR.obs.v.counter.hi=quantile(RR.obs.v.counter, 0.975, na.rm=T),
                                                      RR.obs.v.counter.lo=quantile(RR.obs.v.counter, 0.025, na.rm=T),
                                                      excess.perc=median(excess.perc),
                                                      excess.perc.hi=quantile(excess.perc,0.975, na.rm=T),
                                                      excess.perc.lo=quantile(excess.perc,0.025, na.rm=T),
                                                      RD.obs.v.counter=median(RD.obs.v.counter),
                                                      RD.obs.v.counter.hi=quantile(RD.obs.v.counter, 0.975, na.rm=T),
                                                      RD.obs.v.counter.lo=quantile(RD.obs.v.counter, 0.025, na.rm=T),
                                                      rate_per100k.observed=median(rate_per100k.observed),
                                                      rate_per100k.observed.hi=quantile(rate_per100k.observed, 0.975),
                                                      rate_per100k.observed.lo=quantile(rate_per100k.observed, 0.025),
                                                      rate_per100k.counterfct=median(rate_per100k.counterfct),
                                                      rate_per100k.counterfct.hi=quantile(rate_per100k.counterfct, 0.975),
                                                      rate_per100k.counterfct.lo=quantile(rate_per100k.counterfct, 0.025),
                                                      total.observed=median(total.observed),
                                                      total.counterfct=median(total.counterfct)),
                                                   by=c('variable','time','country','pop')]

saveRDS(obs_v_counter_sens5, paste0(wd, '3-outputs/total_TB_obs_v_counter_sens5.RDS'))
saveRDS(obs_v_counter_sens5_medians, paste0(wd, '3-outputs/total_TB_obs_v_counter_sens5_summary.RDS'))
remove(list = c('all_outputs_sens5', 'all_outputs_sens5_dt', 'obs_v_counter_sens5'))

# Abolition counterfactual for tPAF
abol_arg <- readRDS(paste0(wd, '3-outputs/abolition_argentina_240617.RDS'))
abol_bra <- readRDS(paste0(wd, '3-outputs/abolition_brazil_240603.RDS'))
abol_col <- readRDS(paste0(wd, '3-outputs/abolition_colombia_240612.RDS'))
abol_es <- readRDS(paste0(wd, '3-outputs/abolition_elsalvador_240605.RDS'))
abol_mex <- readRDS(paste0(wd, '3-outputs/abolition_mexico_240611.RDS'))
abol_per <- readRDS(paste0(wd, '3-outputs/abolition_peru_240612.RDS'))

abol_arg <- rbindlist(abol_arg[-which(sapply(abol_arg, is.null))])
abol_bra <- rbindlist(abol_bra[-which(sapply(abol_bra, is.null))])
abol_col <- rbindlist(abol_col[-which(sapply(abol_col, is.null))])
abol_es <- rbindlist(abol_es[-which(sapply(abol_es, is.null))])
abol_mex <- rbindlist(abol_mex[-which(sapply(abol_mex, is.null))])
abol_per <- rbindlist(abol_per[-which(sapply(abol_per, is.null))])

abol_arg$country <- 'Argentina'
abol_bra$country <- 'Brazil'
abol_col$country <- 'Colombia'
abol_es$country <- 'El Salvador'
abol_mex$country <- 'Mexico'
abol_per$country <- 'Peru'

abol_all <- rbind(abol_arg, abol_bra, abol_col, abol_es, abol_mex, abol_per)

# double check because ode solver threw warnings for a few countries (issues w integration due to 0 incarc...)
# remove idx where abolition appeared to not be successfully achieved
ggplot(abol_all[variable == 'pop' & pop == 'Prison']) + 
  geom_line(aes(x=time, y=rate_per100k, group=idx)) + facet_wrap(~country)

# only an issue for argentina, brazil, el salvador, peru
arg_exclude <- unique(abol_arg[variable == 'pop' & pop == 'Prison' & time >= 2015 & rate_per100k >= 1, idx])
bra_exclude <- unique(abol_bra[variable == 'pop' & pop == 'Prison' & time >= 2015 & rate_per100k >= 1, idx])
es_exclude <- unique(abol_es[variable == 'pop' & pop == 'Prison' & time >= 2015 & rate_per100k >= 1, idx])
per_exclude <- unique(abol_per[variable == 'pop' & pop == 'Prison' & time >= 2015 & rate_per100k >= 1, idx])

abol_all <- rbind(abol_arg[!idx %in% arg_exclude], 
                  abol_bra[!idx %in% bra_exclude], 
                  abol_col, 
                  abol_es[!idx %in% es_exclude], 
                  abol_mex, 
                  abol_per[!idx %in% per_exclude])

obs_v_abolition <- merge(rbind(all_outputs_main_dt[scenario == 'Leveled Off' & variable == 'I' & time <= 2020,
                                           c('idx','pop','variable','time','rate_per100k','total','country')]),
                         abol_all[scenario == 'Abolition' & variable == 'I' & time <= 2020,
                                           c('idx','pop','variable','time','rate_per100k','total','country')], 
                       by=c('idx','pop','variable','time','country'),
                       suffixes = c('.observed','.abolition'))

obs_v_abolition[,RR.obs.v.abolition := total.observed / total.abolition]
obs_v_abolition[,excess.perc := RR.obs.v.abolition * 100 - 100]
obs_v_abolition[,RD.obs.v.abolition := total.observed - total.abolition]
obs_v_abolition[,tPAF := (total.observed - total.abolition)/total.observed]

obs_v_abolition_medians <- obs_v_abolition[,.(RR.obs.v.abolition=median(RR.obs.v.abolition),
                                          excess.perc=median(excess.perc),
                                          excess.perc.hi=quantile(excess.perc,0.975, na.rm=T),
                                          excess.perc.lo=quantile(excess.perc,0.025, na.rm=T),
                                          tPAF=median(tPAF),
                                          tPAF.hi=quantile(tPAF, 0.975, na.rm=T),
                                          tPAF.lo=quantile(tPAF, 0.025, na.rm=T),
                                          RD.obs.v.abolition=median(RD.obs.v.abolition),
                                          RD.obs.v.abolition.hi=quantile(RD.obs.v.abolition, 0.975, na.rm=T),
                                          RD.obs.v.abolition.lo=quantile(RD.obs.v.abolition, 0.025, na.rm=T),
                                          rate_per100k.observed=median(rate_per100k.observed),
                                          rate_per100k.observed.hi=quantile(rate_per100k.observed, 0.975),
                                          rate_per100k.observed.lo=quantile(rate_per100k.observed, 0.025),
                                          rate_per100k.abolition=median(rate_per100k.abolition),
                                          rate_per100k.abolition.hi=quantile(rate_per100k.abolition, 0.975),
                                          rate_per100k.abolition.lo=quantile(rate_per100k.abolition, 0.025),
                                          total.observed=median(total.observed),
                                          total.abolition=median(total.abolition)),
                                       by=c('variable','time','country','pop')]

saveRDS(obs_v_abolition, paste0(wd, '3-outputs/total_TB_obs_v_abolition.RDS'))
saveRDS(obs_v_abolition_medians, paste0(wd, '3-outputs/total_TB_obs_v_abolition_summary.RDS'))

# Future projections
all_future <- lapply(setdiff(list.files(paste0(wd, '3-outputs'),
                             pattern = "future.*\\.RDS", full.names = T),
                             list.files(paste0(wd, '3-outputs'),
                                        pattern = "projections.*\\.RDS", full.names = T)), readRDS)
all_future <- rbindlist(all_future, fill = T)

elsal_future <- all_future[country == 'El Salvador']
all_future <- all_future[country != 'El Salvador']

all_future[perc_change_r == 0 & perc_change_iE == 0, scenario := 'Stable']
all_future[round(perc_change_r)==33 & perc_change_iE == 0, scenario := 'Reduce duration 25%'] # increasing release rate by 33% is the same as reducing duration by 25%
all_future[perc_change_r == 0 & perc_change_iE == -25, scenario := 'Reduce entries 25%']
all_future[round(perc_change_r)==33 & perc_change_iE == -25, scenario := 'Both 25%']
all_future[perc_change_r == 100 & perc_change_iE == 0, scenario := 'Reduce duration 50%'] # increasing release rate by 100% is the same as reducing duration by 50%
all_future[perc_change_r == 0 & perc_change_iE == -50, scenario := 'Reduce entries 50%']
all_future[perc_change_r == 100 & perc_change_iE == -50, scenario := 'Both 50%']
table(all_future$scenario, all_future$country)

rates_stable <- all_future[variable == 'I' & pop == 'Combined' & scenario == 'Stable']
all_future <- merge(all_future, 
                    rates_stable[,c('time','pop','variable','idx','rate_per100k','country')],
                    by=c('time','pop','variable','idx','country'),
                    suffixes = c('','.stable'),
                    all.x = T)
all_future[,perc_change_from_stable_rate_per100k := (rate_per100k - rate_per100k.stable) / rate_per100k.stable * 100]

all_future_med <- all_future[,.(rate_per100k=median(rate_per100k),
                                rate_per100k.hi=quantile(rate_per100k, 0.975),
                                rate_per100k.lo=quantile(rate_per100k, 0.025),
                                perc_change_from_stable_rate_per100k=median(perc_change_from_stable_rate_per100k),
                                perc_change_from_stable_rate_per100k.hi=quantile(perc_change_from_stable_rate_per100k, 0.975, na.rm=T),
                                perc_change_from_stable_rate_per100k.lo=quantile(perc_change_from_stable_rate_per100k, 0.025, na.rm=T),
                                perc_change_r=median(perc_change_r),
                                perc_change_iE=median(perc_change_iE)),
                             by=c('pop','variable','time','scenario',
                                     'country')]
saveRDS(all_future, paste0(wd, '3-outputs/future_projections.RDS'))
saveRDS(all_future_med, paste0(wd, '3-outputs/future_projections_summary.RDS'))

elsal_res_2021 <- elsal_future[scenario == 'Continue state of emergency' & variable == 'I' & pop == 'Combined' & time == 2021]

elsal_future_comp <- merge(elsal_future[variable %in% c('I') & pop %in% c('Combined') & time >= 2010],
                         elsal_res_2021[variable %in% c('I') & pop %in% c('Combined'),
                                  c('pop','variable','rate_per100k','idx')],
                         by=c('pop','variable','idx'),
                         suffixes=c('','_2021'), allow.cartesian = T)
elsal_future_comp[, perc_change_from_2021_rate_per100k := (rate_per100k - rate_per100k_2021)/ rate_per100k_2021 * 100]

elsal_future_comp_med <- elsal_future_comp[,.(perc_change_from_2021_rate_per100k=median(perc_change_from_2021_rate_per100k),
                                          rate_per100k=median(rate_per100k),
                                          perc_change_from_2021_rate_per100k_lo=quantile(perc_change_from_2021_rate_per100k, 0.025),
                                          rate_per100k_lo=quantile(rate_per100k, 0.025),
                                          perc_change_from_2021_rate_per100k_hi=quantile(perc_change_from_2021_rate_per100k, 0.975),
                                          rate_per100k_hi=quantile(rate_per100k, 0.975)),
                                       by=c('pop','variable','time','scenario')]
elsal_future_prisonpop_med <- elsal_future[pop == 'Prison' & variable == 'pop',
                                       .(rate_per100k=median(rate_per100k),
                                         rate_per100k_lo=quantile(rate_per100k, 0.025),
                                         rate_per100k_hi=quantile(rate_per100k, 0.975)),
                                       by=c('scenario','time')]

write.csv(elsal_future_comp, paste0(wd, '3-outputs/elsalvador_240605_future_combined.csv'),
          row.names = F)
write.csv(elsal_future_comp_med, paste0(wd, '3-outputs/elsalvador_240605_future_combined_summarized.csv'),
          row.names = F)
write.csv(elsal_future_prisonpop_med, paste0(wd, '3-outputs/elsalvador_240605_future_pop_summarized.csv'),
          row.names = F)
