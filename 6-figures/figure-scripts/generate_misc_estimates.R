# increase in crude PAF (% of notifs occurring in prisons) between 2014 and 2019
include_countries <- intersect(prison_estimates[Country %in% c(unique(wpb.data.interp$Country), analyzed_countries) & Year == 2014 & !is.na(notifs_prison_final), Country],
                               prison_estimates[Country %in% c(unique(wpb.data.interp$Country), analyzed_countries) & Year == 2019 & !is.na(notifs_prison_final), Country])
sum(prison_estimates[Country %in% include_countries & Year == 2014, notifs_prison_final] / 
      sum(who_data[country %in% include_countries & year == 2014, c_newinc]))
sum(prison_estimates[Country %in% include_countries & Year == 2019, notifs_prison_final] / 
      sum(who_data[country %in% include_countries & year == 2019, c_newinc]))

# how much has incarc pop changed since 1990
setorder(wpb.data.interp, Country, Year)
sum(wpb.data.interp[, .SD[1], by='Country']$`Incarcerated Population`,# note these are from differing years - mostly 1989-1992, but two 1998s
    as.integer(latam_incarc_data[Country %in% analyzed_countries & !is.na(`Incarcerated Population`), # argentina is from 1992
                                 .SD[1], by='Country']$`Incarcerated Population`))
sum(wpb.data.interp[, .SD[.N], by='Country']$`Incarcerated Population`, # most 2018-2021, but one 2014
    as.integer(latam_incarc_data[Country %in% analyzed_countries & Year == 2020, `Incarcerated Population`]))

# included countries account for __% of region's prison pop?
sum(as.integer(latam_incarc_data[Country %in% analyzed_countries & Year == 2018, `Incarcerated Population`])) /
  sum(wpb.data.interp[Year == 2018, `Incarcerated Population`], as.integer(latam_incarc_data[Country %in% analyzed_countries & Year == 2018, `Incarcerated Population`]))

# included countries account for __% of region's tb cases?
sum(who_data[country %in% analyzed_countries & year == 2018, c_newinc]) /
  sum(who_data[country %in% c(unique(wpb.data.interp$Country), analyzed_countries) & year == 2018, c_newinc])

# included countries account for __% of region's tb cases in prisons?
sum(prison_estimates[Country %in% analyzed_countries & Year == 2018, notifs_prison_final]) /
  sum(prison_estimates[Country %in% c(unique(wpb.data.interp$Country), analyzed_countries) & Year == 2018, notifs_prison_final])

# pop w incarc history
pop_hist_dt <- pop_main_dt[time == 2019 & scenario == 'Leveled Off' & pop_hist == 'History of Incarc.',
                           .(rate_per100k=sum(rate_per100k)),
                           by=c('country','idx')]
pop_hist_dt_popsize <- merge(pop_hist_dt,
                             latam_incarc_data[Year == 2019, .(Country, `Population Size 15+`)],
                             by.x=c('country'),
                             by.y=c('Country'))
pop_hist_dt_popsize[,pop:=rate_per100k*`Population Size 15+`/100000]

set.seed(2024)
pop_hist_dist <- pop_hist_dt_popsize[, .(pop=sample(pop, 5000, replace=T)), by='country']
pop_hist_dist$n <- rep(seq(1,5000), 6)
round(quantile(pop_hist_dist[,.(total.pop=sum(pop)),
                             by='n']$total.pop, c(0.025, 0.5, 0.975)))

# size of ever-exposed pop vs pop currently in prison
(sum(as.numeric(latam_incarc_data[Country %in% analyzed_countries & 
                                    Year == 2019, 
                                  `Incarcerated Population`])) + 
    median(pop_hist_dist[,.(total.pop=sum(pop)),by='n']$total.pop)) / 
  sum(as.numeric(latam_incarc_data[Country %in% analyzed_countries & 
                                     Year == 2019, 
                                   `Incarcerated Population`]))

# total excess cases across countries - generate thru sampling
all_cases <- merge(obs_v_counter[pop == 'Combined' &
                                   variable == 'I' &
                                   time == 2019],
                   latam_incarc_data[,c('Country','Population Size 15+','Year')],
                   by.x=c('country','time'), by.y=c('Country','Year'))
all_cases[,total.cases.ctft:=rate_per100k.counterfct*`Population Size 15+`/100000]
all_cases[,excess.cases:=RD.obs.v.counter*`Population Size 15+`/100000]

set.seed(2024)
excess_dt_abs_dist <- all_cases[time == 2019, .(excess.cases=sample(excess.cases, 5000, replace=T),
                                                total.cases.ctft=sample(total.cases.ctft, 5000, replace=T)), by='country']
excess_dt_abs_dist$n <- rep(seq(1,5000), 6)
round(quantile(excess_dt_abs_dist[,.(total.excess.cases=sum(excess.cases)), # total excess
                                  by='n']$total.excess.cases, c(0.025, 0.5, 0.975)))
round(quantile(excess_dt_abs_dist[,.(perc.excess.cases=sum(excess.cases)/sum(total.cases.ctft)), # perc excess
                                  by='n']$perc.excess.cases, c(0.025, 0.5, 0.975))*100, 1)

# how much do excess incident cases in prisons exceed excess cases *diagnosed* in prison
temp <- merge(obs_v_counter[pop == 'Prison' & variable == 'I' & time == 2019, c('idx','country','RD.obs.v.counter'), with=F],
              obs_v_counter[pop == 'Prison' & variable == 'D' & time == 2019, c('idx','country','RD.obs.v.counter'), with=F],
              by=c('idx','country'), suffixes = c('.I','.D'))
temp[,.(perc_diagnosed=median(RD.obs.v.counter.D/RD.obs.v.counter.I),
        perc_diagnosed_lo=quantile(RD.obs.v.counter.D/RD.obs.v.counter.I, 0.025),
        perc_diagnosed_hi=quantile(RD.obs.v.counter.D/RD.obs.v.counter.I, 0.975)),
     by=c('country')]
temp_sum <- temp[,.(RD.obs.v.counter.D=median(RD.obs.v.counter.D),
                    RD.obs.v.counter.D.lo=quantile(RD.obs.v.counter.D, 0.025),
                    RD.obs.v.counter.D.hi=quantile(RD.obs.v.counter.D, 0.975),
                    RD.obs.v.counter.I=median(RD.obs.v.counter.I),
                    RD.obs.v.counter.I.lo=quantile(RD.obs.v.counter.I, 0.025),
                    RD.obs.v.counter.I.hi=quantile(RD.obs.v.counter.I, 0.975)),
                 by=c('country')]
temp_sum[,ratio := RD.obs.v.counter.I/RD.obs.v.counter.D]
temp_sum[,c('country','ratio')]
median(temp_sum$ratio)
# temp_sum[,.(ratio = sum(RD.obs.v.counter.I)/sum(RD.obs.v.counter.D))]

# pop tb incidence in 2034 under future scenarios
all_future_2034[scenario == 'Both 50%', .(med=median(perc_change_from_stable_rate_per100k),
                                          lo=quantile(perc_change_from_stable_rate_per100k, 0.025),
                                          hi=quantile(perc_change_from_stable_rate_per100k, 0.975)), by='country']
elsal_future_2034[, .(med=median(perc_change_from_2021_rate_per100k),
                      lo=quantile(perc_change_from_2021_rate_per100k, 0.025),
                      hi=quantile(perc_change_from_2021_rate_per100k, 0.975)), by='scenario']

# Appendix future projections: % changes in entry & release rates under continue trends future scenario
all_pop_sizes <- fread(paste0(wd, '3-outputs/all_pop_sizes.csv'))

pop_cast_merged <- merge(all_future[scenario == 'Continual Growth' & variable == 'pop' & time == 2034,
                                    c('country','idx','perc_change_iE','perc_change_iR')],
                         all_pop_sizes[time==2023], by=c('country','idx'))
pop_cast_merged[,weighted_perc_change := perc_change_iE*(N3+N4)/(N2+N3+N4)+
                  perc_change_iR*(N2)/(N2+N3+N4)]
pop_cast_merged[,.(med=round(median(weighted_perc_change)),
                   lo=round(quantile(weighted_perc_change, 0.025)),
                   hi=round(quantile(weighted_perc_change, 0.975))),
                by='country']
all_future[scenario == 'Continual Growth' & variable == 'pop' & time == 2034,
           .(med_r=round(median(perc_change_r))),
           by='country']
