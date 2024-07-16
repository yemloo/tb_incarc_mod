library(data.table)
library(ggplot2)
library(ggpubr)
library(ggsci)
library(ggbeeswarm)
library(GGally)
library(gridExtra)

country_p <- 'Colombia'
today <- '240612'
wd <- '~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/' # local
wd_save <- '~/Desktop/Stanford/TBprisons_git/'

# read in calibration targets
latam_incarc_data <- fread(paste0(wd, 'data/latam_incarc_data_231203.csv'))
prison_estimates <- fread(paste0(wd, 'data/prison_estimates_latam.csv'))
who_data <- fread(paste0(wd, 'data/who_data_wsplines.csv'))
setorder(who_data, country, year)
setorder(prison_estimates, Country, Year)
prison_incid_samp <- as.matrix(fread(paste0(wd, 'params/uncertainty_analysis/colombia_prison_incid_231211.csv')))
who_incid_samp <- as.matrix(fread(paste0(wd, 'params/uncertainty_analysis/colombia_whoincidfactor_231211.csv')))
all_samps <- as.matrix(fread(paste0(wd, 'params/uncertainty_analysis/colombia_samps_240612.csv')))

# read in and bind the final param sets (with calibration errors)
incarc_mod_fits <- lapply(list.files(paste0(wd, 'results/', gsub(' ', '', tolower(country_p)), '_', today, ''),
                                     pattern = "incarc_mod_fit.*\\.RDS", full.names = T), readRDS)
idx_list <- gsub('colombia_240612_incarc_mod_fit_|.RDS', '', 
                 list.files(paste0(wd, 'results/', gsub(' ', '', tolower(country_p)), '_', today, ''),
                       pattern = "incarc_mod_fit.*\\.RDS", full.names = F))
incarc_mod_fits_dt <- data.table()
for (i in 1:length(incarc_mod_fits)) { 
  tmp <- data.table(time=c(incarc_mod_fits[[i]]$admissions.years, 
                           incarc_mod_fits[[i]]$incarc_prev_yrs,
                           incarc_mod_fits[[i]]$recid_yr),
                    value=c(incarc_mod_fits[[i]]$admissions,
                            incarc_mod_fits[[i]]$incarc_prev,
                            incarc_mod_fits[[i]]$recid),
                    variable=c(rep('admissions',length(incarc_mod_fits[[i]]$admissions)),
                               rep('incarc_prev',length(incarc_mod_fits[[i]]$incarc_prev)),
                               rep('recid',length(incarc_mod_fits[[i]]$recid))))
  tmp$idx <- idx_list[i]
  incarc_mod_fits_dt <- rbind(incarc_mod_fits_dt, tmp)
}

# read in and bind the final param sets (with calibration errors)
all_params <- lapply(list.files(paste0(wd, 'results/', gsub(" ", "", tolower(country_p)), '_', today, ''),
                                pattern = "params.*.RDS", full.names = T), readRDS)
saveRDS(all_params, file=paste0(wd_save, '5-params/all_params_',tolower(country_p), '_', today, '.RDS'))

exclude_from_dt <- c('pop_grow','pop_grow_func','mort_genpop_func')

all_params_dt <- data.table()
for (i in 1:length(all_params)) { all_params_dt <- rbind(all_params_dt, 
                                                         as.list(all_params[[i]]$main_params[setdiff(names(all_params[[i]]$main_params),exclude_from_dt)]))}
write.csv(all_params_dt, file=paste0(wd_save, '5-params/all_params_',tolower(country_p), '_', today, '.csv'),
          row.names = F)

incarc_mod_fits_dt$idx <- as.integer(incarc_mod_fits_dt$idx)
incarc_mod_fits_dt <- merge(incarc_mod_fits_dt, all_params_dt[,c('idx','incarc_optim_error','incarc_optim_e1',
                                                                 'incarc_optim_e2','incarc_optim_e3',
                                                                 'incarc_optim_e4','incarc_optim_e5'),
                                                              with=F],
                            by='idx')
write.csv(incarc_mod_fits_dt, file=paste0(wd_save, '3-outputs/incarc_mod_fits_',tolower(country_p), '_', today, '.csv'),
          row.names = F)

# investigate error distribution
quantile(all_params_dt$incarc_optim_error, c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
plot(density(all_params_dt$incarc_optim_error))

quantile(all_params_dt$tb_optim_error, c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
plot(density(all_params_dt$tb_optim_error))

# visualize joint distributions of errors & params
ggpairs(all_params_dt[incarc_optim_error <= quantile(all_params_dt$incarc_optim_error, 0.95),
                      c('incarc_optim_e1','incarc_optim_e2','incarc_optim_e3',
                        'incarc_optim_e4','incarc_optim_e5','incarc_optim_error')],
        upper = list(continuous=wrap('density', alpha=0.5)),
        lower = list(continuous = wrap("points", alpha=0.5, size=0.5))) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggpairs(all_params_dt[incarc_optim_error <= quantile(all_params_dt$incarc_optim_error, 0.95),
                      c('recid_factor','admissions.factor','change.r.factor1','change.r.factor2',
                        'iR', 'iE:iR','k1','k2','covidf','r','incarc_optim_error')],
        upper = list(continuous=wrap('density', alpha=0.5)),
        lower = list(continuous = wrap("points", alpha=0.5, size=0.5))) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggpairs(all_params_dt[tb_optim_error<=quantile(all_params_dt$tb_optim_error, 0.95),
                      c('tb_optim_e1','tb_optim_e2','tb_optim_e3','tb_optim_e4',
                        'tb_optim_error')],
        upper = list(continuous=wrap('density', alpha=0.5)),
        lower = list(continuous = wrap("points", alpha=0.5, size=0.5))) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggpairs(all_params_dt[tb_optim_error <= quantile(all_params_dt$tb_optim_error, 0.95),
                      c('beta_pp','beta_cc','c1','c4','v','z','d4','d1','v1',
                        'covid.d.factor','tb_optim_error')],
        # upper = list(continuous=wrap('density', alpha=0.5)),
        lower = list(continuous = wrap("points", alpha=0.5, size=0.5))) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave(paste0(wd, 'figs/', gsub(" ", "", tolower(country_p)), '_', today, '_calib_TBparams.pdf'), height = 14, width = 14)

# read in and bind model outputs
all_outputs <- lapply(list.files(paste0(wd, 'results/model_outputs_', gsub(" ", "", tolower(country_p)), '_', today),
                                 pattern = ".*.csv", full.names = T), fread)
all_outputs_dt <- rbindlist(all_outputs)

remove(all_outputs)
# saveRDS(all_outputs_dt, file=paste0(wd_save, '3-outputs/all_outputs_',gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))

# check and set incarceration-related error threshold
# incarc errors:
# e1: mean incarc prevalence
# e2: incarc prevalence at baseline
# e3: final incarc prevalence
# e4: recidivism
# e5: admissions rate

incarc_optim_error_thresh <- quantile(all_params_dt$incarc_optim_error, 0.95)
incarc_optim_e1_thresh <- 0.01
incarc_optim_e2_thresh <- Inf
incarc_optim_e3_thresh <- 0.001
incarc_optim_e4_thresh <- Inf
incarc_optim_e5_thresh <- 0.06

ggplot(incarc_mod_fits_dt[variable == 'incarc_prev' & incarc_optim_e1 <= incarc_optim_e1_thresh]) + 
  geom_line(aes(x=time, y=value, group=idx, color=incarc_optim_e1), linewidth=0.2) +
  geom_point(data=latam_incarc_data[Country == country_p & !is.na(`Adjusted Incarceration Prevalence`)],
             aes(x=Year, y=`Adjusted Incarceration Prevalence`)) + 
  theme_bw() + scale_color_gradient(low = 'grey', high = 'red')
ggplot(incarc_mod_fits_dt[variable == 'incarc_prev' & incarc_optim_e1 <= incarc_optim_e1_thresh &
                            incarc_optim_e3 <= incarc_optim_e3_thresh]) + 
  geom_line(aes(x=time, y=value, group=idx, color=incarc_optim_e3), linewidth=0.2) +
  geom_point(data=latam_incarc_data[Country == country_p & !is.na(`Adjusted Incarceration Prevalence`)],
             aes(x=Year, y=`Adjusted Incarceration Prevalence`)) + 
  theme_bw() + scale_color_gradient(low = 'grey', high = 'red')
ggplot(incarc_mod_fits_dt[variable == 'recid' & incarc_optim_e4 <= incarc_optim_e4_thresh]) + 
  geom_line(aes(x=time, y=value, group=idx, color=incarc_optim_e4), linewidth=0.2) +
  geom_point(data=latam_incarc_data[Country == country_p & !is.na(`Recidivism Percentage`)], aes(x=Year, y=`Recidivism Percentage`)) +
  theme_bw() + scale_color_gradient(low = 'grey', high = 'red')
ggplot(incarc_mod_fits_dt[variable == 'admissions' & incarc_optim_e5 <= incarc_optim_e5_thresh]) + 
  geom_line(aes(x=time, y=value, group=idx, color=incarc_optim_e5), linewidth=0.2) +
  geom_point(data=latam_incarc_data[Country == country_p & !is.na(`Admissions Rate`)], aes(x=Year, y=`Admissions Rate`)) +
  theme_bw() + scale_color_gradient(low = 'grey', high = 'red')
# ggplot(incarc_mod_fits_dt[variable == 'recid']) + geom_density(aes(x=value)) +
#   geom_vline(data=latam_incarc_data[Country == country_p & !is.na(`Recidivism Percentage`)],
#              aes(xintercept=`Recidivism Percentage`), linetype='dotted')

incarc_e_hi_idx <- unique(all_params_dt[incarc_optim_error > incarc_optim_error_thresh |
                                                  incarc_optim_e1 > incarc_optim_e1_thresh |
                                                  incarc_optim_e2 > incarc_optim_e2_thresh |
                                                  incarc_optim_e3 > incarc_optim_e3_thresh |
                                                  incarc_optim_e4 > incarc_optim_e4_thresh |
                                                  incarc_optim_e5 > incarc_optim_e5_thresh,
                                                idx])

# check and set TB-related error threshold
# TB errors:
# e1: prison incidence
# e2: total incidence
# e3: prison notifications
# e4: total notifications
# not calibrating to TB deaths but will visually check model fit to TB deaths as validation
who_tmp <- who_data[country == country_p, .(year, c_newinc_per100k, e_inc_100k, e_mort_100k)]
setnames(who_tmp, c('year','c_newinc_per100k','e_inc_100k','e_mort_100k'), c('time', 'D', 'I', 'muI'))
who_tmp <- melt(who_tmp, id.vars = 'time', value.name = 'rate_per100k')
who_tmp$pop <- 'Combined'
who_tmp$source <- 'Data'

prison_tmp <- prison_estimates[Country == country_p, .(Year, NR_Obs_New)]
setnames(prison_tmp, c('Year','NR_Obs_New'), c('time','D'))
prison_tmp[!is.na(D) & time <= 2019, I := apply(prison_incid_samp, 1, median)/100000]
prison_tmp <- melt(prison_tmp, id.vars = 'time', value.name = 'rate_per100k')
prison_tmp$rate_per100k <- prison_tmp$rate_per100k*100000
prison_tmp$pop <- 'Prison'
prison_tmp$source <- 'Data'

all_outputs_dt_tb_main <- all_outputs_dt[pop %in% c('Prison','Combined') & 
                                           variable %in% c('I','D','muI') &
                                           !idx %in% incarc_e_hi_idx & 
                                           scenario == 'Leveled Off' & 
                                           analysis == 'main analysis'] 
all_outputs_dt_tb_main <- merge(all_outputs_dt_tb_main, all_params_dt, by='idx')
compare_with_data <- rbind(all_outputs_dt_tb_main,
                           who_tmp,
                           prison_tmp,
                           fill=TRUE)
compare_with_data$variable <- factor(compare_with_data$variable, 
                                     levels = c('I','D','muI'),
                                     labels = c('Cases','Notifications','Deaths'))
medians <- compare_with_data[time >= 1990 & time <= 2025 & is.na(source),
                             .(medianrate_per100k = median(rate_per100k),
                               lo=quantile(rate_per100k, 0.025),
                               hi=quantile(rate_per100k, 0.975)), by=c('time','variable',
                                                                                'pop')]
# test diff thresholds
tb_optim_error_thresh <- quantile(all_params_dt$tb_optim_error, 0.95)
tb_optim_e1_thresh <- Inf
tb_optim_e2_thresh <- Inf
tb_optim_e3_thresh <- 0.4
tb_optim_e4_thresh <- 0.11

ggplot() +
  geom_line(data = compare_with_data[time >= 1990 & time <= 2025 & is.na(source) &
                                       tb_optim_error <= tb_optim_error_thresh &
                                       tb_optim_e1 <= tb_optim_e1_thresh & 
                                       tb_optim_e2 <= tb_optim_e2_thresh &
                                     tb_optim_e3 <= tb_optim_e3_thresh & # prison notifs
                                     tb_optim_e4 <= tb_optim_e4_thresh # combined notifs
  ],
  aes(x=time, y=rate_per100k, group=idx, color=tb_optim_e1), linewidth=0.5) +
  geom_point(data = compare_with_data[time >= 1990 & source == 'Data' &
                                      (pop != 'Prison' | variable == 'Notifications')],
           aes(x=time, y=rate_per100k), size = 0.8) +
  geom_point(data=compare_with_data[pop == 'Prison' & source == 'Data' & variable == 'Cases'],
           aes(x=time, y=rate_per100k), size = 0.8) +
  facet_grid(pop~variable, scales = 'free') + theme_bw() +
  scale_color_gradient(low = 'grey', high = 'red') +
  labs(x='Year', y='Rate per 100k') + ggtitle(country_p) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

tb_e_hi_idx_main <- all_params_dt[tb_optim_error > tb_optim_error_thresh |
                                     tb_optim_e1 > tb_optim_e1_thresh |
                                     tb_optim_e2 > tb_optim_e2_thresh |
                                     tb_optim_e3 > tb_optim_e3_thresh |
                                     tb_optim_e4 > tb_optim_e4_thresh, idx]

# save good idx
all_outputs_main_good_dt <- all_outputs_dt[analysis == 'main analysis' & !idx %in% c(incarc_e_hi_idx, tb_e_hi_idx_main)]
good_idx <- unique(all_outputs_main_good_dt$idx)
length(good_idx)

# save good outputs
saveRDS(all_outputs_main_good_dt, paste0(wd_save, '3-outputs/good_outputs_',gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))
saveRDS(good_idx, paste0(wd_save, '3-outputs/good_idx_',gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))
remove(all_outputs_main_good_dt)

# apply same thresholds for sensitivity analyses
# params_sens1_dt <- data.table() # no sens1 for colombia: no change in beta_pp in main analysis
exclude <- c()
# for (i in 1:length(all_params)) {
#   tmp <- as.data.table(as.list(all_params[[i]]$sens1_params))
#   if (length(tmp)==0){ # some didn't finish calibrating before timeout
#     # print(all_params[[i]]$main_params[['idx']])
#     exclude <- unique(c(exclude, all_params[[i]]$main_params[['idx']]))
#     next
#   }
#   tmp$idx <- all_params[[i]]$main_params[['idx']]
#   params_sens1_dt <- rbind(params_sens1_dt, tmp)
# }

params_sens2_dt <- data.table()
for (i in 1:length(all_params)) {
  tmp <- as.data.table(as.list(all_params[[i]]$sens2_params))
  if (length(tmp)==0){ # some didn't finish calibrating before timeout
    # print(all_params[[i]]$main_params[['idx']])
    exclude <- unique(c(exclude, all_params[[i]]$main_params[['idx']]))
    next
  }
  tmp$idx <- all_params[[i]]$main_params[['idx']]
  params_sens2_dt <- rbind(params_sens2_dt, tmp)
}

tb_e_hi_idx_sens2 <- params_sens2_dt[tb_optim_error > tb_optim_error_thresh |
                                       tb_optim_e1 > tb_optim_e1_thresh |
                                       tb_optim_e2 > tb_optim_e2_thresh |
                                       tb_optim_e3 > tb_optim_e3_thresh |
                                       tb_optim_e4 > tb_optim_e4_thresh, idx]

exclude <- c()
params_sens3_dt <- data.table()
for (i in 1:length(all_params)) {
  tmp <- as.data.table(as.list(all_params[[i]]$sens3_params))
  if (length(tmp)==0){ # some didn't finish calibrating before timeout
    # print(all_params[[i]]$main_params[['idx']])
    exclude <- unique(c(exclude, all_params[[i]]$main_params[['idx']]))
    next
  }
  tmp$idx <- all_params[[i]]$main_params[['idx']]
  params_sens3_dt <- rbind(params_sens3_dt, tmp)
}

tb_e_hi_idx_sens3 <- params_sens3_dt[tb_optim_error > tb_optim_error_thresh |
                                       tb_optim_e1 > tb_optim_e1_thresh |
                                       tb_optim_e2 > tb_optim_e2_thresh |
                                       tb_optim_e3 > tb_optim_e3_thresh |
                                       tb_optim_e4 > tb_optim_e4_thresh, idx]

exclude <- c()
params_sens4_dt <- data.table()
for (i in 1:length(all_params)) {
  tmp <- as.data.table(as.list(all_params[[i]]$sens4_params))
  if (length(tmp)==0){ # some didn't finish calibrating before timeout
    # print(all_params[[i]]$main_params[['idx']])
    exclude <- unique(c(exclude, all_params[[i]]$main_params[['idx']]))
    next
  }
  tmp$idx <- all_params[[i]]$main_params[['idx']]
  params_sens4_dt <- rbind(params_sens4_dt, tmp)
}

tb_e_hi_idx_sens4 <- params_sens4_dt[tb_optim_error > tb_optim_error_thresh |
                                       tb_optim_e1 > tb_optim_e1_thresh |
                                       tb_optim_e2 > tb_optim_e2_thresh |
                                       tb_optim_e3 > tb_optim_e3_thresh |
                                       tb_optim_e4 > tb_optim_e4_thresh, idx]

exclude <- c()
params_sens5_dt <- data.table()
for (i in 1:length(all_params)) {
  tmp <- as.data.table(as.list(all_params[[i]]$sens5_params))
  if (length(tmp)==0){ # some didn't finish calibrating before timeout
    # print(all_params[[i]]$main_params[['idx']])
    exclude <- unique(c(exclude, all_params[[i]]$main_params[['idx']]))
    next
  }
  tmp$idx <- all_params[[i]]$main_params[['idx']]
  params_sens5_dt <- rbind(params_sens5_dt, tmp)
}

tb_e_hi_idx_sens5 <- params_sens5_dt[tb_optim_error > tb_optim_error_thresh |
                                       tb_optim_e1 > tb_optim_e1_thresh |
                                       tb_optim_e2 > tb_optim_e2_thresh |
                                       tb_optim_e3 > tb_optim_e3_thresh |
                                       tb_optim_e4 > tb_optim_e4_thresh, idx]

# save good idx
saveRDS(all_outputs_dt[analysis == 'assortative mixing' & !idx %in% c(incarc_e_hi_idx, tb_e_hi_idx_sens2)], 
        paste0(wd_save, '3-outputs/good_outputs_',gsub(" ", "", tolower(country_p)), '_', today, '_sens2_assort.RDS'))
saveRDS(all_outputs_dt[analysis == 'c1w=d1w=0' & !idx %in% c(incarc_e_hi_idx, tb_e_hi_idx_sens3)], 
        paste0(wd_save, '3-outputs/good_outputs_',gsub(" ", "", tolower(country_p)), '_', today, '_sens3_c1wd1w0.RDS'))
saveRDS(all_outputs_dt[analysis == 'prop_E=1/3' & !idx %in% c(incarc_e_hi_idx, tb_e_hi_idx_sens4)], 
        paste0(wd_save, '3-outputs/good_outputs_',gsub(" ", "", tolower(country_p)), '_', today, '_sens4_prop_E.RDS'))
saveRDS(all_outputs_dt[analysis == 'beta_pc=0' & !idx %in% c(incarc_e_hi_idx, tb_e_hi_idx_sens5)], 
        paste0(wd_save, '3-outputs/good_outputs_',gsub(" ", "", tolower(country_p)), '_', today, '_sens5_beta_pc.RDS'))

