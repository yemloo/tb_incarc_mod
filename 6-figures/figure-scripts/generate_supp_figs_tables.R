# Code for supplementary tables and figures not generated in other scripts

##### TABLE S7  relative values for beta, prog rate, diag rate in prisons vs outside #####
ratios_2019 <- time_vary_all[,.(beta_ratio=median(beta_pp/beta_cc), 
                                c_ratio=median(c1/c4), 
                                d_ratio=median(d1/d4)),
                             by=c('country')]

quantile(ratios_2019$beta_ratio)
quantile(ratios_2019$c_ratio)
quantile(ratios_2019$d_ratio)

##### TABLE S8 change in entry & rel rates #####
all_params_dt_all <- readRDS(paste0(wd, '5-params/all_params_all_countries_240708.RDS'))
all_pop_sizes <- fread(paste0(wd, '3-outputs/all_pop_sizes.csv')) # from mod_validation_240710.R

# entry rates were different for recent & distant history strata
# calculate weighted percent change based on their respective pop prevalence
pop_cast <- all_pop_sizes[time==2019]

# merge w entry rates...
pop_cast_merged <- merge(all_params_dt_all[,c('country','idx','iE_perc_change','iR_perc_change')],
                         pop_cast, by=c('country','idx'))
pop_cast_merged[,weighted_perc_change := iE_perc_change*(N3+N4)/(N2+N3+N4) +
                  iR_perc_change*N2/(N2+N3+N4)]
pop_cast_merged[,.(med=round(median(weighted_perc_change)),
                   lo=round(quantile(weighted_perc_change, 0.025)),
                   hi=round(quantile(weighted_perc_change, 0.975))),
                by='country']

# also do this for the years where entry rates peaked for each country
table(all_params_dt_all$iE_perc_change_max_yr, all_params_dt_all$country)

# get the size of the recently released pop for each country in that year
pop_cast_max <- dcast(pop_main_dt[scenario == 'Leveled Off' &
                                    ((country == 'Argentina' & time == 2016) |
                                       (country == 'Brazil' & time == 2019) |
                                       (country == 'Colombia' & time == 2009) |
                                       (country == 'El Salvador' & time == 2015) |
                                       (country == 'Mexico' & time == 2002) |
                                       (country == 'Peru' & time == 1999))],
                      country+idx+time~pop, value.var = 'rate_per100k')
pop_cast_merged_max <- merge(all_params_dt_all[,c('country','idx','iE_perc_change_max','iR_perc_change_max')],
                             pop_cast_max, by=c('country','idx'))
pop_cast_merged_max[,weighted_perc_change_max := iE_perc_change_max*(100000-`Post-Release`)/100000 +
                      iR_perc_change_max*`Post-Release`/100000]
pop_cast_merged_max[,.(med=round(median(weighted_perc_change_max)),
                       lo=round(quantile(weighted_perc_change_max, 0.025)),
                       hi=round(quantile(weighted_perc_change_max, 0.975))),
                    by='country']

# summarize change in release rates
all_params_dt_all[,.(med=round(median(r_perc_change)),
                     lo=round(quantile(r_perc_change, 0.025)),
                     hi=round(quantile(r_perc_change, 0.975))),
                  by='country']

# change in duration of incarceration
all_params_dt_all[,.(med=100/(100+median(r_perc_change)),
                     lo=100/(100+quantile(r_perc_change, 0.025)),
                     hi=100/(100+quantile(r_perc_change, 0.975))),
                  by='country']

# max change in release rates
all_params_dt_all[,.(med=round(median(r_perc_change_max)),
                     lo=round(quantile(r_perc_change_max, 0.025)),
                     hi=round(quantile(r_perc_change_max, 0.975))),
                  by='country']

##### TABLE S11 #####
# proportion of excess cases among distant v recent history
comp_history[,.(perc.RD.history = round(median(RD.obs.v.counter / RD.obs.v.counter.total)*100),
                perc.RD.history.lo = round(quantile(RD.obs.v.counter / RD.obs.v.counter.total, 0.025)*100),
                perc.RD.history.hi = round(quantile(RD.obs.v.counter / RD.obs.v.counter.total, 0.975)*100)),
             by=c('pop','country')]
comp_history_both <- comp_history[,.(RD.obs.v.counter = sum(RD.obs.v.counter),
                                     RD.obs.v.counter.total=RD.obs.v.counter.total),
                                  by=c('idx','country')]
comp_history_both[,.(perc.RD.history = round(median(RD.obs.v.counter / RD.obs.v.counter.total)*100),
                     perc.RD.history.lo = round(quantile(RD.obs.v.counter / RD.obs.v.counter.total, 0.025)*100),
                     perc.RD.history.hi = round(quantile(RD.obs.v.counter / RD.obs.v.counter.total, 0.975)*100)),
                  by=c('country')]

##### TABLE S12 stratum-specific incidence rates #####
for (c in unique(obs_v_counter_medians$country)){
  print(c)
  tmp <- obs_v_counter_medians[variable == 'I' & time == 2019 & country == c]
  tmp$pop <- factor(tmp$pop, levels = c('Combined','Prison','Post-Release','Formerly Incarc','Never Incarc'))
  setorder(tmp, pop)
  print(round(tmp[,c('rate_per100k.observed', 'rate_per100k.observed.lo','rate_per100k.observed.hi'),with=F]))
}

##### TABLE S13 incarc prev in 2034 under future scenarios
for (c in unique(all_future_med$country)){
  print(c)
  tmp <- all_future_med[variable == 'pop' & time == 2034 & country == c]
  tmp$scenario <- factor(tmp$scenario, levels = c('Stable',
                                                  'Continual Growth',
                                                  'Reduce entries 25%',
                                                  'Reduce entries 50%',
                                                  'Reduce duration 25%',
                                                  'Reduce duration 50%',
                                                  'Both 25%',
                                                  'Both 50%'))
  setorder(tmp, scenario)
  print(round(tmp[,c('rate_per100k', 'rate_per100k.lo','rate_per100k.hi'),with=F]))
}

##### TABLE S15 incarc prev in 2034 in el salvador under future scenarios #####
elsal_future_pop_med[time == 2034]

##### FIGURE S4 param priors and posteriors ######
all_params_dt_all <- readRDS(paste0(wd_save, '5-params/all_params_all_countries_240708.RDS'))
start_params_all <- readRDS(paste0(wd_save, '5-params/uncertainty_analysis/start_params_all_countries_240708.RDS'))

toplot <- all_params_dt_all[,c('iR','iE:iR','r','beta_pp','beta_cc','c1','c4','d1','d4','country'),with=F]
toplot$iE <- as.numeric(toplot$`iE:iR`) * as.numeric(toplot$iR)

toplot_melt <- melt(toplot[,!c('iE:iR'),with=F],
                    id.vars='country',
                    measure.vars=setdiff(colnames(toplot), c('iE:iR','country')),
                    variable.name = 'param')

# priors
start_params_all_plot <- start_params_all[,c('iR','iE:iR','r','beta_pp','beta_cc:beta_pp','c1','c4:c1','d','d1:d4','country'),with=F]
start_params_all_plot$iE <- as.numeric(start_params_all_plot$`iE:iR`) * as.numeric(start_params_all_plot$iR)
start_params_all_plot$beta_cc <- as.numeric(start_params_all_plot$`beta_cc:beta_pp`) * as.numeric(start_params_all_plot$beta_pp)
start_params_all_plot$c4 <- as.numeric(start_params_all_plot$`c4:c1`) * as.numeric(start_params_all_plot$c1)
start_params_all_plot$d1 <- as.numeric(start_params_all_plot$`d1:d4`) * as.numeric(start_params_all_plot$d)
setnames(start_params_all_plot, 'd','d4')
start_params_all_plot[is.na(d1), d1 := d4] # didn't calibrate d1 for brazil, mexico, peru

start_params_all_plot_melt <- melt(start_params_all_plot[,!c('iE:iR',
                                                             'beta_cc:beta_pp',
                                                             'd1:d4'),with=F],
                                   id.vars='country',
                                   measure.vars=setdiff(colnames(toplot), c('iE:iR','country')),
                                   variable.name = 'param')

table(toplot_melt$param)
table(start_params_all_plot_melt$param)

start_params_txt <- start_params_all_plot_melt[,.(mean=mean(value),
                                                  median=median(value),
                                                  lo=quantile(value, 0.025),
                                                  hi=quantile(value, 0.975),
                                                  ymax=max(density(value)$y)),
                                               by=c('country','param')]
start_params_txt[mean > 1, txt:=paste0('Prior: ', round(mean, 1), ' (', # adjust rounding depending on magnitude of param
                                       round(lo, 1), ',', round(hi, 1), ')')]
start_params_txt[mean <= 1, txt:=paste0('Prior: ', round(mean, 3), ' (', 
                                        round(lo, 2), ',', round(hi, 2), ')')]
start_params_txt[mean <= 0.01, txt:=paste0('Prior: ', round(mean, 4), ' (', 
                                           round(lo, 4), ',', round(hi, 3), ')')]

post_params_txt <- toplot_melt[,.(mean=mean(as.numeric(value)),
                                  median=median(as.numeric(value)),
                                  lo=quantile(as.numeric(value), 0.025),
                                  hi=quantile(as.numeric(value), 0.975),
                                  ymax=max(density(as.numeric(value))$y)),
                               by=c('country','param')]
post_params_txt[mean > 1, txt:=paste0('Posterior: ', round(mean, 1), ' (', # adjust rounding depending on magnitude of param
                                      round(lo, 1), ',', round(hi, 1), ')')]
post_params_txt[mean <= 1, txt:=paste0('Posterior: ', round(mean, 3), ' (', 
                                       round(lo, 2), ',', round(hi, 2), ')')]
post_params_txt[mean <= 0.01, txt:=paste0('Posterior: ', round(mean, 4), ' (', 
                                          round(lo, 4), ',', round(hi, 3), ')')]

all_txt <- merge(start_params_txt, post_params_txt, by=c('country','param'),
                 suffixes = c('.prior','.post'))
all_txt[,xmin := pmin(lo.prior, lo.post)] # set where to print text
all_txt[,ymax := pmax(ymax.prior, ymax.post)*1.35] 

# adjust names to correspond to manuscript param notation
toplot_melt$param <- factor(toplot_melt$param, 
                            levels=c('iR','iE','r',
                                     'beta_pp','beta_cc',
                                     'c1','c4','d1','d4'),
                            labels=c('q_r','q_d,q_n','r',
                                     'beta_pp','beta_cc',
                                     'tau_p','tau_d,tau_n',
                                     'delta_d,delta_n','delta_p'))
start_params_all_plot_melt$param <- factor(start_params_all_plot_melt$param, 
                                           levels=c('iR','iE','r',
                                                    'beta_pp','beta_cc',
                                                    'c1','c4','d1','d4'),
                                           labels=c('q_r','q_d,q_n','r',
                                                    'beta_pp','beta_cc',
                                                    'tau_p','tau_d,tau_n',
                                                    'delta_d,delta_n','delta_p'))
all_txt$param <- factor(all_txt$param, 
                        levels=c('iR','iE','r',
                                 'beta_pp','beta_cc',
                                 'c1','c4','d1','d4'),
                        labels=c('q_r','q_d,q_n','r',
                                 'beta_pp','beta_cc',
                                 'tau_p','tau_d,tau_n',
                                 'delta_d,delta_n','delta_p'))
all_txt[country == 'Argentina' & param == 'q_d,q_n',
        ymax:=26000*1.35] # fix this: density changes after two distributions are jointly plotted

ggplot() +
  geom_density(data=toplot_melt,
               aes(x=as.numeric(value),
                   fill='posterior'),
               fill=alpha('darkred', 0.4)) +
  geom_density(data=start_params_all_plot_melt,
               aes(x=as.numeric(value),
                   fill='prior'),
               fill=alpha('darkblue', 0.3)) +
  geom_text(data=all_txt,
            aes(x=xmin, y=ymax*0.82,
                label=txt.post), size=3, hjust='inward',
            color=alpha('darkred', 0.5)) + 
  geom_text(data=all_txt,
            aes(x=xmin, y=ymax,
                label=txt.prior), size=3, hjust='inward', vjust='inward',
            color=alpha('darkblue', 0.5)) + 
  facet_wrap(country~param, scales='free', nrow=6) + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text = element_text(size=8)) +
  scale_x_continuous(expand=c(0,0)) +
  labs(x='value',y='')
ggsave(paste0(wd_save, '6-figures/figureS4_param_prior_post.pdf'), height=12, width=18)

##### FIGURE S9 sensitivity analyses #####
sens1 <- obs_v_counter_sens1_medians[time == 2019 & pop == 'Combined' & variable == 'I']
sens1$analysis <- 'No change in beta_pp'

sens2 <- obs_v_counter_sens2_medians[time == 2019 & pop == 'Combined' & variable == 'I']
sens2$analysis <- 'Assortative mixing'

sens3 <- obs_v_counter_sens3_medians[time == 2019 & pop == 'Combined' & variable == 'I']
sens3$analysis <- 'Equal tau_r, delta_r'

sens4 <- obs_v_counter_sens4_medians[time == 2019 & pop == 'Combined' & variable == 'I']
sens4$analysis <- '1:2 ratio of E:L'

sens5 <- obs_v_counter_sens5_medians[time == 2019 & pop == 'Combined' & variable == 'I']
sens5$analysis <- 'Beta_pc=beta_cp=0'

main <- obs_v_counter_medians[time == 2019 & pop == 'Combined' & variable == 'I']
main$analysis <- 'Main analysis'

all_sens <- rbind(main, sens1, sens2, sens3, sens4, sens5, fill=T)
all_sens$analysis <- factor(all_sens$analysis, levels=c('Main analysis',
                                                        '1:2 ratio of E:L',
                                                        'Equal tau_r, delta_r',
                                                        'No change in beta_pp',
                                                        'Assortative mixing',
                                                        'Beta_pc=beta_cp=0'))

ggplot(all_sens) +
  geom_point(aes(x=analysis, y=RR.obs.v.counter, color=analysis)) +
  geom_errorbar(aes(x=analysis, ymin=RR.obs.v.counter.lo, 
                    ymax=RR.obs.v.counter.hi, color=analysis),
                width=0.3) +
  facet_wrap(~country, scales='free_x', nrow=1) + theme_bw() +
  labs(x='', y='Incidence rate ratio for observed vs. counterfactual in 2019') + coord_flip() +
  theme(legend.position = 'none',
        axis.text.x=element_text(angle=45, hjust=1)) +
  scale_x_discrete(limits = rev(levels(all_sens$analysis)),
                   breaks = rev(levels(all_sens$analysis))) 
ggsave(paste0(wd, '6-figures/figureS9_main.v.sens.RR.pdf'), height=2, width=12)

##### FIGURE S10 stratum-specific incidence rates #####
ggplot(obs_v_counter[variable == 'I' & time == 2019], aes(x=pop, y=rate_per100k.observed, fill=pop)) +
  geom_boxplot(outlier.shape = NA, size=0.3) + facet_wrap(~country) +
  scale_x_discrete(limits=c('Combined','Never Incarc','Formerly Incarc', 'Post-Release','Prison'),
                   labels=c('Total','Never Incarc.','Distant Incarc. History','Recent Incarc. History','Prison')) +
  theme_bw() + theme(axis.text.x=element_text(angle=45, hjust=1),
                     legend.position = 'none') +
  labs(x='',y='TB incidence per 100 000 person-years (log10 scale)') +
  scale_fill_lancet(alpha=0.8) + scale_y_log10(limits=c(10,NA))
ggsave('~/Desktop/Stanford/TBprisons_git/6-figures/figureS10_incid2019.perpop.pdf', height=5, width=7)

##### FIGURE S11 excess burden due to recent state of emergency in El Salvador #####
# incarc prev
ggplot() + 
  geom_line(data=pop_main_dt_summary[pop_hist == 'Current Incarc.' & 
                                       country == 'El Salvador'&
                                       scenario == 'No SoE' &
                                       time >= 2021 & time <= 2025], aes(x=time, y=rate_per100k)) +
  geom_line(data=pop_main_dt_summary[pop_hist == 'Current Incarc.' & 
                                       country == 'El Salvador'&
                                       scenario == 'Leveled Off' &
                                       time >= 2021 & time <= 2025], aes(x=time, y=rate_per100k)) +
  theme_classic() + ylim (0, NA)

# ratio of observed to counterfactual (no SoE) incidence 
round(es_obs_v_noSoE_medians[time == 2024 & pop == 'Combined' & variable == 'I', 
                             c(RR.obs.v.counter, RR.obs.v.counter.lo, RR.obs.v.counter.hi)], 1)
# excess cases
round(es_obs_v_noSoE_medians[time == 2024 & pop == 'Combined' & variable == 'I', 
                             c(RD.obs.v.counter, RD.obs.v.counter.lo, RD.obs.v.counter.hi)] *
        wpp_data[country == 'El Salvador' & year == 2024, pop.15plus] / 100000)

ggplot(es_obs_v_noSoE_medians[pop == 'Combined' & variable == 'I']) +
  geom_ribbon(aes(x=time, ymin=rate_per100k.observed.lo, ymax=rate_per100k.observed.hi),
              fill=alpha(get_palette('aaas', 2)[2],0.2)) +
  geom_line(aes(x=time, y=rate_per100k.observed, color='State of emergency')) +
  geom_ribbon(aes(x=time, ymin=rate_per100k.noSoE.lo, ymax=rate_per100k.noSoE.hi),
              fill=alpha(get_palette('aaas', 2)[1],0.2)) +
  geom_line(aes(x=time, y=rate_per100k.noSoE, color='No state of emergency')) +
  theme_classic() + ylim (0, NA) +
  scale_color_aaas() +
  labs(x='Year', y='Pop TB incidence per 100 000', color='Incarceration scenario')
ggsave(paste0(wd, '6-figures/figureS11_elsal_obs_v_noSoE.pdf'), height=2.5, width=4.5)

