riskfactors_dt_use <- riskfactors_dt[sex == 'a' & age_group %in% c('all','15plus','18plus')]
riskfactors_dt_use[risk_factor == 'dia', age_group := '15plus'] # compare w 15plus, will be an overestimate

riskfactors_dt_use <- rbind(riskfactors_dt_use, data.table(risk_factor=rep(c('incarc'), length=length(analyzed_countries)),
                                                     age_group=rep(c('15plus'), length=length(analyzed_countries)),
                                                     country=obs_v_abolition_medians[time == 2019 & pop == 'Combined' & variable == 'I', country],
                                                     paf=obs_v_abolition_medians[time == 2019 & pop == 'Combined' & variable == 'I', tPAF],
                                                     paf.lo=obs_v_abolition_medians[time == 2019 & pop == 'Combined' & variable == 'I', tPAF.lo],
                                                     paf.hi=obs_v_abolition_medians[time == 2019 & pop == 'Combined' & variable == 'I', tPAF.hi]),
                        fill=T)

riskfactors_dt_use <- merge(riskfactors_dt_use[risk_factor != 'all'], 
                                          riskfactors_dt_use[risk_factor == 'all'],
                                          all.x=T, by=c('age_group','country'),
                                          suffixes = c('','.total'))
riskfactors_dt_use[risk_factor != 'incarc',paf := best / best.total]
riskfactors_dt_use[risk_factor != 'incarc',paf.lo := lo / best.total]
riskfactors_dt_use[risk_factor != 'incarc',paf.hi := hi / best.total]

riskfactors_dt_use[risk_factor == 'incarc', best := paf*best.total]
riskfactors_dt_use[risk_factor == 'incarc', lo := paf.lo*best.total]
riskfactors_dt_use[risk_factor == 'incarc', hi := paf.hi*best.total]

riskfactors_dt_use$risk_factor <- factor(riskfactors_dt_use$risk_factor,
                                                       levels = rev(c('und','alc','hiv','smk','dia','incarc')),
                                                       labels = rev(c('Undernourishment',
                                                                      'Alcohol',
                                                                      'HIV',
                                                                      'Smoking',
                                                                      'Diabetes',
                                                                      'Incarceration')))

sum(riskfactors_dt_use[risk_factor == 'Incarceration', best]) / sum(riskfactors_dt[sex == 'a' & age_group %in% c('15plus') & risk_factor == 'all', best])
sum(riskfactors_dt_use[risk_factor == 'Incarceration', lo]) / sum(riskfactors_dt[sex == 'a' & age_group %in% c('15plus') & risk_factor == 'all', best])
sum(riskfactors_dt_use[risk_factor == 'Incarceration', hi]) / sum(riskfactors_dt[sex == 'a' & age_group %in% c('15plus') & risk_factor == 'all', best])

ggplot(riskfactors_dt_use) + 
  geom_bar(aes(x=reorder_within(risk_factor, by=paf, within=country), 
               y=paf*100, fill=risk_factor), stat='identity') +
  geom_bar(data=data.table(risk_factor=c(rep('Incarceration',6),'Undernourishment',
                                         'Alcohol','HIV','Smoking','Diabetes'),
                           country=c(analyzed_countries, rep('Argentina', 5)),
                           paf=c(2.0,11.5,6.8,44.1,1.2,8.9,rep(0,5))),
           aes(x=reorder_within(risk_factor, by=paf, within=country), y=paf),
           fill='#5E1313',
           stat='identity') +
  geom_errorbar(aes(x=reorder_within(risk_factor, by=paf, within=country),
                    ymin=paf.lo*100, ymax=paf.hi*100), width=0.2) +
  theme_classic() +
  scale_x_reordered() +
  scale_fill_manual(values=c('Alcohol'='grey', 'Smoking'='grey', 'Undernourishment'='grey',
                             'HIV'='grey','Diabetes'='grey','Incarceration'='#ad4c4c')) +
  coord_flip() +
  facet_wrap(~country, scales='free') +
  labs(x='',
       y='Percent of incident cases') +
  theme(legend.position = 'none')
ggsave(paste0(wd, '6-figures/figure3_wcrudepaf_240705.pdf'), height=3.5, width=8) # note: this doesn't include the legend at the bottom, which we add in PPT
