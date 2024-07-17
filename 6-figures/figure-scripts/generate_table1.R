# recidivism in 2019 (within-prison prev of incarc history)
incarc_mat_all[,.(recid=round(median(S/(S+P)*100)),
                  lo=round(quantile(S/(S+P), 0.025)*100),
                  hi=round(quantile(S/(S+P), 0.975)*100)),
               by='country']

# avg duration of incarc in 2019
time_vary_all[,.(dur=round(median(1/(r+mu1)),1),
                 lo=round(quantile(1/(r+mu1), 0.025),1),
                 hi=round(quantile(1/(r+mu1), 0.975),1)),
              by='country']

# community prev of incarc history
pop_main_dt_summary[time == 2019 & scenario == 'Leveled Off' & pop_hist == 'History of Incarc.']

# perc increase in incarc prev from 1990 to 2019
pop_main_dt_comp <- merge(pop_main_dt[time == 2019 & scenario == 'Leveled Off' & pop == 'Prison',c('country','idx','rate_per100k'),with=F],
                          pop_main_dt[time == 1990 & scenario == 'Leveled Off' & pop == 'Prison',c('country','idx','rate_per100k'),with=F],
                          by=c('country','idx'), suffixes = c('.final','.base'))
pop_main_dt_comp[,perc.change := (rate_per100k.final - rate_per100k.base)/rate_per100k.base * 100]
pop_main_dt_comp[,.(perc.change=median(perc.change),
                    perc.change.lo=quantile(perc.change, 0.025),
                    perc.change.hi=quantile(perc.change, 0.975)),
                 by='country']

# increasing incarc driven by entry or duration - see generate_supp_figs_tables.R (table S8)