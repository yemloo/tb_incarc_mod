library(data.table)
library(ggplot2)
library(ggpubr)
library(ggsci)
library(gridExtra)
library(EnvStats)

wd <- '~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/' # local
wd_save <- '~/Desktop/Stanford/TBprisons_git/'

# read in calibration targets
latam_incarc_data <- fread(paste0(wd, 'data/latam_incarc_data_231203.csv'))
prison_estimates <- fread(paste0(wd, 'data/prison_estimates_latam.csv'))
who_data <- fread(paste0(wd, 'data/who_data_wsplines.csv'))
setorder(who_data, country, year)
setorder(prison_estimates, Country, Year)

pop_main_dt_summary <- readRDS(paste0(wd_save, '3-outputs/pop_rates_summary.RDS'))

###################################
####### PLOT CALIB FITS ###########
###################################
all_countries <- c('Argentina','Brazil','Colombia','El Salvador','Mexico','Peru')
today_list <- c('240617','240603','240612','240605','240611','240612')

for (i in 1:length(all_countries)){
  today <- today_list[i]
  country_p <- all_countries[i]
  
  incarc_mod_fits_dt <- fread(paste0(wd_save, '3-outputs/incarc_mod_fits_',gsub(" ", "", tolower(country_p)), '_', today, '.csv'))
  good_idx <- readRDS(paste0(wd_save, '3-outputs/good_idx_',gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))
  
  if (country_p == 'El Salvador'){
    prison_incid_samp <- as.matrix(fread(paste0(wd, 'params/uncertainty_analysis/elsal_prison_incid_231211.csv')))
    who_incid_samp <- as.matrix(fread(paste0(wd, 'params/uncertainty_analysis/elsal_whoincidfactor_231211.csv')))
    all_samps <- as.matrix(fread(paste0(wd, 'params/uncertainty_analysis/elsal_samps_', today, '.csv')))
  } else {
    prison_incid_samp <- as.matrix(fread(paste0(wd, 'params/uncertainty_analysis/', gsub(" ", "", tolower(country_p)), '_prison_incid_231211.csv')))
    who_incid_samp <- as.matrix(fread(paste0(wd, 'params/uncertainty_analysis/', gsub(" ", "", tolower(country_p)), '_whoincidfactor_231211.csv')))
    all_samps <- as.matrix(fread(paste0(wd, 'params/uncertainty_analysis/', gsub(" ", "", tolower(country_p)), '_samps_', today, '.csv')))
  }
 
  all_outputs_main_good_dt <- readRDS(paste0(wd_save, '3-outputs/good_outputs_',gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))
  all_outputs_main_good_dt$variable <- factor(all_outputs_main_good_dt$variable, 
                                              levels = c('I','D'),
                                              labels = c('Incidence','Notifications'))
  
  incarc_mod_fits_summary <- incarc_mod_fits_dt[idx %in% good_idx,
                                                .(med=median(value),
                                                  lo=quantile(value, 0.025),
                                                  hi=quantile(value, 0.975)),
                                                by=c('time','variable')]
  
  ggplot(incarc_mod_fits_summary[variable == 'incarc_prev']) +
    geom_ribbon(aes(x=time, ymin=lo, ymax=hi), fill=alpha('navyblue', 0.3)) +
    geom_line(aes(x=time, y=med), color='navyblue') +
    geom_point(data=latam_incarc_data[Country == country_p & !is.na(`Adjusted Incarceration Prevalence`)],
               aes(x=Year, y=`Adjusted Incarceration Prevalence`), size=0.3) +
    theme_bw() + labs(x='Year', y='Incarc prev per 100k') +
    ggtitle(country_p)
  ggsave(paste0(wd_save, '6-figures/incarc_prev_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
         height=2.5, width=3)
  
  # compare w tb model pop fit
  ggplot(incarc_mod_fits_summary[variable == 'incarc_prev']) +
    geom_ribbon(aes(x=time+0.5, ymin=lo, ymax=hi), fill=alpha('navyblue', 0.3)) +
    geom_line(aes(x=time+0.5, y=med), color='navyblue') +
    geom_ribbon(data=pop_main_dt_summary[country == country_p & pop_hist == 'Current Incarc.' & scenario == 'Leveled Off' &
                                           time %in% (incarc_mod_fits_summary[variable == 'incarc_prev', time]+0.5)], 
                aes(x=time, ymin=rate_per100k.lo, ymax=rate_per100k.hi), fill=alpha('darkred', 0.3)) +
    geom_line(data=pop_main_dt_summary[country == country_p & pop_hist == 'Current Incarc.' & scenario == 'Leveled Off' &
                                         time %in% (incarc_mod_fits_summary[variable == 'incarc_prev', time]+0.5)], 
              aes(x=time, y=rate_per100k), color='darkred') +
    geom_point(data=latam_incarc_data[Country == country_p & !is.na(`Adjusted Incarceration Prevalence`)],
               aes(x=Year, y=`Adjusted Incarceration Prevalence`), size=0.3) +
    theme_bw() + labs(x='Year', y='Incarc prev per 100k') +
    ggtitle(country_p)
  ggsave(paste0(wd_save, '6-figures/incarc_prev_fit_compare', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
         height=2.5, width=3)
  
  
  if (country_p == 'Colombia'){
    ggplot(incarc_mod_fits_summary[variable == 'admissions']) +
      geom_ribbon(aes(x=time, ymin=lo, ymax=hi), fill=alpha('navyblue', 0.3)) +
      geom_line(aes(x=time, y=med), color='navyblue') +
      geom_point(data=latam_incarc_data[Country == country_p & !is.na(`Admissions Rate`) & Year <= 2021], 
                 aes(x=Year, y=`Admissions Rate`), size=0.3) +
      geom_errorbar(data=latam_incarc_data[Country == country_p & !is.na(`Admissions Rate`) & Year <= 2021],
                    aes(x=Year, ymin=`Admissions Rate`*qtri(0.025, min=0.75, max=1.25, mode=1),
                        ymax=`Admissions Rate`*qtri(0.975, min=0.75, max=1.25, mode=1)),
                    linewidth=0.2) +
      theme_bw() + labs(x='Year', y='Admissions per 100k') +
      ggtitle(country_p)
    ggsave(paste0(wd_save, '6-figures/incarc_admissions_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
           height=2.5, width=3)
  } else {
    ggplot(incarc_mod_fits_summary[variable == 'admissions']) +
      geom_ribbon(aes(x=time, ymin=lo, ymax=hi), fill=alpha('navyblue', 0.3)) +
      geom_line(aes(x=time, y=med), color='navyblue') +
      geom_point(data=latam_incarc_data[Country == country_p & !is.na(`Admissions Rate`)], aes(x=Year, y=`Admissions Rate`), size=0.3) +
      geom_errorbar(data=latam_incarc_data[Country == country_p & !is.na(`Admissions Rate`)],
                    aes(x=Year, ymin=`Admissions Rate`*qtri(0.025, min=0.75, max=1.25, mode=1),
                        ymax=`Admissions Rate`*qtri(0.975, min=0.75, max=1.25, mode=1)),
                    linewidth=0.2) +
      theme_bw() + labs(x='Year', y='Admissions per 100k') +
      ggtitle(country_p)
    ggsave(paste0(wd_save, '6-figures/incarc_admissions_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
           height=2.5, width=3)
  }
  
  if (country_p == 'Mexico'){
    latam_incarc_data[Country==country_p & !is.na(`Readmissions percent`),
                      recid_odds := `Readmissions percent` / (1-`Readmissions percent`)]
  } else {
    latam_incarc_data[Country==country_p & !is.na(`Recidivism Percentage`),
                      recid_odds := `Recidivism Percentage` / (1-`Recidivism Percentage`)]
  }
 
  if (country_p == 'Brazil'){
    ggplot(incarc_mod_fits_dt[variable == 'recid' &
                                time %in% latam_incarc_data[Country == country_p & !is.na(recid_odds), Year]]) +
      geom_density(aes(x=value, y = after_stat(count)/sum(after_stat(count))), fill=alpha('navyblue', 0.3)) +
      # geom_point(aes(x=time, y=med), color='navyblue') +
      geom_vline(data=latam_incarc_data[Country == country_p & !is.na(recid_odds)], aes(xintercept=recid_odds/(1+recid_odds))) +
      geom_vline(data=latam_incarc_data[Country == country_p & !is.na(recid_odds)],
                 aes(xintercept=recid_odds*quantile(all_samps[,'recid_factor'], 0.025)/
                       (1+recid_odds*quantile(all_samps[,'recid_factor'], 0.025))),
                 linetype='dotted') +
      geom_vline(data=latam_incarc_data[Country == country_p & !is.na(recid_odds)],
                 aes(xintercept=recid_odds*quantile(all_samps[,'recid_factor'], 0.975)/
                       (1+recid_odds*quantile(all_samps[,'recid_factor'], 0.975))),
                 linetype='dotted') +
      theme_bw() + labs(x='Recidivism (proportion)', y='Density') +
      ggtitle(country_p)
    ggsave(paste0(wd_save, '6-figures/incarc_recid_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
           height=2.5, width=3)
  } else {
    ggplot(incarc_mod_fits_summary[variable == 'recid' &
                                     time %in% latam_incarc_data[Country == country_p & !is.na(recid_odds), Year]]) +
      geom_ribbon(aes(x=time, ymin=lo, ymax=hi), fill=alpha('navyblue', 0.3)) +
      geom_line(aes(x=time, y=med), color='navyblue') +
      geom_point(data=latam_incarc_data[Country == country_p & !is.na(recid_odds)], aes(x=Year, y=recid_odds/(1+recid_odds)), size=0.3) +
      geom_errorbar(data=latam_incarc_data[Country == country_p & !is.na(recid_odds)],
                    aes(x=Year, ymin=recid_odds*quantile(all_samps[,'recid_factor'], 0.025)/
                          (1+recid_odds*quantile(all_samps[,'recid_factor'], 0.025)),
                        ymax=recid_odds*quantile(all_samps[,'recid_factor'], 0.975)/
                          (1+recid_odds*quantile(all_samps[,'recid_factor'], 0.975))),
                    linewidth=0.2) +
      theme_bw() + labs(x='Year', y='Recidivism (proportion)') +
      ggtitle(country_p)
    ggsave(paste0(wd_save, '6-figures/incarc_recid_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
           height=2.5, width=3)
  }
  
  # TB
  medians <- all_outputs_main_good_dt[time >= 1990 & time <= 2025 &
                                        pop %in% c('Prison','Combined') &
                                        scenario == 'Leveled Off',
                                      .(medianrate_per100k = median(rate_per100k),
                                        lo=quantile(rate_per100k, 0.025),
                                        hi=quantile(rate_per100k, 0.975)), by=c('time','variable',
                                                                                'pop')]
  if (country_p %in% c('Brazil','Colombia')){
    ggplot() +
      geom_ribbon(data = medians[variable != 'Deaths' & time <= 2022],
                  aes(x=time, ymin=lo, ymax=hi), fill=alpha('navyblue', 0.3)) +
      geom_line(data = medians[variable != 'Deaths' & time <= 2022],
                aes(x=time, y=medianrate_per100k), color='navyblue') +
      geom_errorbar(data=data.table(pop='Combined',
                                    variable='Incidence',
                                    time=seq(2000,2022),
                                    lo=who_data[country == country_p & !is.na(e_inc_100k), c_newinc_per100k]*apply(who_incid_samp, 1, function(x) quantile(x, 0.025)),
                                    hi=who_data[country == country_p & !is.na(e_inc_100k), c_newinc_per100k]*apply(who_incid_samp, 1, function(x) quantile(x, 0.975))),
                    aes(x=time, ymin=lo, ymax=hi), linewidth=0.2) +
      geom_point(data=data.table(pop='Combined',
                                 variable='Incidence',
                                 time=seq(2000,2022),
                                 med=who_data[country == country_p & !is.na(e_inc_100k), e_inc_100k]),
                 aes(x=time, y=med), size=0.3) +
      geom_point(data=data.table(pop='Combined',
                                 variable='Notifications',
                                 time=who_data[country == country_p & year>=1990, year],
                                 med=who_data[country == country_p & year>=1990, c_newinc_per100k]),
                 aes(x=time, y=med), size=0.3) +
      geom_errorbar(data=data.table(pop='Prison',
                                    variable='Incidence',
                                    time=prison_estimates[Country == country_p & !is.na(NR_Obs_New) & Year <= 2019, Year],
                                    lo=apply(prison_incid_samp, 1, function(x) quantile(x, 0.025)),
                                    hi=apply(prison_incid_samp, 1, function(x) quantile(x, 0.975))),
                    aes(x=time, ymin=lo, ymax=hi), linewidth=0.2) +
      geom_point(data=data.table(pop='Prison',
                                 variable='Incidence',
                                 time=prison_estimates[Country == country_p & !is.na(NR_Obs_New) & Year <= 2019, Year],
                                 med=apply(prison_incid_samp, 1, median)),
                 aes(x=time, y=med), size=0.3) +
      geom_point(data=data.table(pop='Prison',
                                 variable='Notifications',
                                 time=prison_estimates[Country == country_p & !is.na(NR_Obs_New), Year],
                                 med=prison_estimates[Country == country_p & !is.na(NR_Obs_New), NR_Obs_New]*100000),
                 aes(x=time, y=med), size=0.3) +
      facet_grid(pop~variable, scales = 'free') + theme_bw() +
      labs(x='Year', y='Rate per 100k') + ggtitle(country_p) +
      theme(axis.text.x=element_text(angle=45, hjust=1))
    ggsave(paste0(wd_save, '6-figures/tb_calib_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
           height=4, width=5)
  } else {
    ggplot() +
      geom_ribbon(data = medians[variable != 'Deaths' & time <= 2022],
                  aes(x=time, ymin=lo, ymax=hi), fill=alpha('navyblue', 0.3)) +
      geom_line(data = medians[variable != 'Deaths' & time <= 2022],
                aes(x=time, y=medianrate_per100k), color='navyblue') +
      geom_errorbar(data=data.table(pop='Combined',
                                    variable='Incidence',
                                    time=seq(2000,2022),
                                    lo=who_data[country == country_p & !is.na(e_inc_100k), c_newinc_per100k]*apply(who_incid_samp, 1, function(x) quantile(x, 0.025)),
                                    hi=who_data[country == country_p & !is.na(e_inc_100k), c_newinc_per100k]*apply(who_incid_samp, 1, function(x) quantile(x, 0.975))),
                    aes(x=time, ymin=lo, ymax=hi), linewidth=0.2) +
      geom_point(data=data.table(pop='Combined',
                                 variable='Incidence',
                                 time=seq(2000,2022),
                                 med=who_data[country == country_p & !is.na(e_inc_100k), e_inc_100k]),
                 aes(x=time, y=med), size=0.3) +
      geom_point(data=data.table(pop='Combined',
                                 variable='Notifications',
                                 time=who_data[country == country_p & year>=1990, year],
                                 med=who_data[country == country_p & year>=1990, c_newinc_per100k]),
                 aes(x=time, y=med), size=0.3) +
      geom_errorbar(data=data.table(pop='Prison',
                                    variable='Incidence',
                                    time=prison_estimates[Country == country_p & !is.na(NR_Obs_New), Year],
                                    lo=apply(prison_incid_samp, 1, function(x) quantile(x, 0.025)),
                                    hi=apply(prison_incid_samp, 1, function(x) quantile(x, 0.975))),
                    aes(x=time, ymin=lo, ymax=hi), linewidth=0.2) +
      geom_point(data=data.table(pop='Prison',
                                 variable='Incidence',
                                 time=prison_estimates[Country == country_p & !is.na(NR_Obs_New), Year],
                                 med=apply(prison_incid_samp, 1, median)),
                 aes(x=time, y=med), size=0.3) +
      geom_point(data=data.table(pop='Prison',
                                 variable='Notifications',
                                 time=prison_estimates[Country == country_p & !is.na(NR_Obs_New), Year],
                                 med=prison_estimates[Country == country_p & !is.na(NR_Obs_New), NR_Obs_New]*100000),
                 aes(x=time, y=med), size=0.3) +
      facet_grid(pop~variable, scales = 'free') + theme_bw() +
      labs(x='Year', y='Rate per 100k') + ggtitle(country_p) +
      theme(axis.text.x=element_text(angle=45, hjust=1))
    ggsave(paste0(wd_save, '6-figures/tb_calib_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
           height=4, width=5)
  }
}
