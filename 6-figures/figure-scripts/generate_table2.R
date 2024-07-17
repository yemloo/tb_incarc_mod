# note: this script also includes code for table s9 (estimates in 2022 instead of 2019)

# RR for obs v counterfactual in 2019
rr_dt <- obs_v_counter_medians[time == 2019 & pop == 'Combined' & variable == 'I',
                      c('country',
                        'RR.obs.v.counter', 
                        'RR.obs.v.counter.hi', 
                        'RR.obs.v.counter.lo'), 
                      with=F]
for (i in 1:nrow(rr_dt)){
  current <- as.vector(rr_dt[i])
  print(paste0(current[['country']], ': ', 
               round(current[['RR.obs.v.counter']], 2), ' (',
               round(current[['RR.obs.v.counter.lo']], 2), ', ',
               round(current[['RR.obs.v.counter.hi']], 2), ')'))
}

# RR for obs v counterfactual in 2022
rr_dt_2022 <- obs_v_counter_medians[time == 2022 & pop == 'Combined' & variable == 'I',
                               c('country',
                                 'RR.obs.v.counter', 
                                 'RR.obs.v.counter.hi', 
                                 'RR.obs.v.counter.lo'), 
                               with=F]
for (i in 1:nrow(rr_dt_2022)){
  current <- as.vector(rr_dt_2022[i])
  print(paste0(current[['country']], ': ', 
               round(current[['RR.obs.v.counter']], 2), ' (',
               round(current[['RR.obs.v.counter.lo']], 2), ', ',
               round(current[['RR.obs.v.counter.hi']], 2), ')'))
}

# excess cases per 100k in 2019
excess_dt <- obs_v_counter_medians[time == 2019 & pop == 'Combined' & variable == 'I',
                               c('country',
                                 'RD.obs.v.counter', 
                                 'RD.obs.v.counter.hi', 
                                 'RD.obs.v.counter.lo'), 
                               with=F]
for (i in 1:nrow(excess_dt)){
  current <- as.vector(excess_dt[i])
  print(paste0(current[['country']], ': ', 
               round(current[['RD.obs.v.counter']], 1), ' (',
               round(current[['RD.obs.v.counter.lo']], 1), ', ',
               round(current[['RD.obs.v.counter.hi']], 1), ')'))
}

# excess cases per 100k in 2022
excess_dt_2022 <- obs_v_counter_medians[time == 2022 & pop == 'Combined' & variable == 'I',
                                   c('country',
                                     'RD.obs.v.counter', 
                                     'RD.obs.v.counter.hi', 
                                     'RD.obs.v.counter.lo'), 
                                   with=F]
for (i in 1:nrow(excess_dt_2022)){
  current <- as.vector(excess_dt_2022[i])
  print(paste0(current[['country']], ': ', 
               round(current[['RD.obs.v.counter']], 1), ' (',
               round(current[['RD.obs.v.counter.lo']], 1), ', ',
               round(current[['RD.obs.v.counter.hi']], 1), ')'))
}

# excess cases (absolute) in 2019
excess_dt_abs <- merge(obs_v_counter[pop == 'Combined' & variable == 'I',
                      c('idx','country','time',
                        'RD.obs.v.counter','rate_per100k.counterfct'), 
                      with=F],
                      latam_incarc_data[,c('Country','Population Size 15+','Year')],
                      by.x=c('country','time'), by.y=c('Country','Year'))
excess_dt_abs[,excess.cases:=RD.obs.v.counter*`Population Size 15+`/100000]

excess_dt_med <- excess_dt_abs[,.(excess.cases=round(median(excess.cases)),
                                                excess.cases.lo=round(quantile(excess.cases, 0.025)),
                                                excess.cases.hi=round(quantile(excess.cases, 0.975))),
                                             by=c('country','time')]
excess_dt_med[time==2019]

# excess cases (absolute) in 2022
excess_dt_abs_2022 <- merge(obs_v_counter[time == 2022 & pop == 'Combined' & variable == 'I',
                                     c('idx','country',
                                       'RD.obs.v.counter'), 
                                     with=F],
                       latam_incarc_data[Year == 2022,c('Country','Population Size 15+')],
                       by.x=c('country'), by.y=c('Country'))
excess_dt_abs_2022[,excess.cases:=RD.obs.v.counter*`Population Size 15+`/100000]

excess_dt_abs_2022[,.(excess.cases=round(median(excess.cases)),
                 excess.cases.lo=round(quantile(excess.cases, 0.025)),
                 excess.cases.hi=round(quantile(excess.cases, 0.975))),
              by=c('country')]

# tPAF
paf_dt <- obs_v_abolition_medians[time == 2019 & variable == 'I' & pop == 'Combined',
                                  c('country','tPAF','tPAF.lo','tPAF.hi'),
                                  with=F]
for (i in 1:nrow(paf_dt)){
  current <- as.vector(paf_dt[i])
  print(paste0(current[['country']], ': ', 
               round(current[['tPAF']]*100, 1), ' (',
               round(current[['tPAF.lo']]*100, 1), ', ',
               round(current[['tPAF.hi']]*100, 1), ')'))
}
