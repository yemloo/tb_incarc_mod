library(data.table)
library(ggplot2)
library(ggpubr)
library(lme4)
library(ggsci)
library(clubSandwich)
library(MuMIn)
wd <- '~/Desktop/Stanford/TBprisons_git/'
all_params_dt <- readRDS(paste0(wd, '3-outputs/all_params_all_countries_240708.RDS'))
obs_v_counter <- readRDS(paste0(wd, '3-outputs/total_TB_obs_v_counter.RDS'))
obs_v_counter <- obs_v_counter[variable == 'I' & pop == 'Combined']

all_countries_mm_merged <- merge(all_params_dt, obs_v_counter, by=c('country','idx'))

# changes in beta and diagnosis rate outside prison (change periods are based on country-specific settings)
all_countries_mm_merged[country == 'Argentina', perc.change.beta.out := (z*(2010-2002) + -1*z*(2020-2013)) / beta_cc * 100]
all_countries_mm_merged[country == 'Argentina', perc.change.diag.out := v*(2000-1992) / d4 * 100]

all_countries_mm_merged[country == 'Brazil', perc.change.beta.out := z*(2015-1990) / beta_cc * 100]
all_countries_mm_merged[country == 'Brazil', perc.change.diag.out := v*(2015-1990) / d4 * 100]

all_countries_mm_merged[country == 'Colombia', perc.change.beta.out := z*(2020-1990) / beta_cc * 100]
all_countries_mm_merged[country == 'Colombia', perc.change.diag.out := v*(2020-1990) / d4 * 100]

all_countries_mm_merged[country == 'El Salvador', perc.change.beta.out := z*(2000-1990) / beta_cc * 100]
all_countries_mm_merged[country == 'El Salvador', perc.change.diag.out := v*(2000-1990) / d4 * 100]

all_countries_mm_merged[country == 'Mexico', perc.change.beta.out := (z*(1996-1994) + z*`z2:z`*(2002-1996) + z*`z3:z`*(2020-2006)) / beta_cc * 100]
all_countries_mm_merged[country == 'Mexico', perc.change.diag.out := v*(1998-1995) / d4 * 100]

all_countries_mm_merged[country == 'Peru', perc.change.beta.out := (z*(2000-1990) + 0.5*z*(2020-2000)) / beta_cc * 100]
all_countries_mm_merged[country == 'Peru', perc.change.diag.out := v*(2020-1990) / d4 * 100]

# changes in beta and diagnosis rate in prison
all_countries_mm_merged[country == 'Argentina', perc.change.beta.prison := (p*(2020-2015)) / beta_pp * 100]
all_countries_mm_merged[country == 'Argentina', perc.change.diag.prison := 0]

all_countries_mm_merged[country == 'Brazil', perc.change.beta.prison := p*(2018-2015) / beta_pp * 100]
all_countries_mm_merged[country == 'Brazil', perc.change.diag.prison := 0]

all_countries_mm_merged[country == 'Colombia', perc.change.beta.prison := 0]
all_countries_mm_merged[country == 'Colombia', perc.change.diag.prison := 0]

all_countries_mm_merged[country == 'El Salvador', perc.change.diag.prison := (v1*(perc.change.diag.out/100)*d4 + (d1 + v1*(perc.change.diag.out/100)*d4)*(spike.d1.factor-1)) / d1 * 100] # in El Sal, d1 changes as ratio of change in d4 (v1 ratio is named "v1"), and then spikes in last few years
all_countries_mm_merged[country == 'El Salvador', perc.change.beta.prison := (exp(p*(2017-2005))-1) / beta_pp * 100]

all_countries_mm_merged[country == 'Mexico', perc.change.beta.prison := p*(2020-2016) / beta_pp * 100]
all_countries_mm_merged[country == 'Mexico', perc.change.diag.prison := v1.ratio*((perc.change.diag.out/100)*d4) / d1 * 100]

all_countries_mm_merged[country == 'Peru', perc.change.beta.prison := 0]
all_countries_mm_merged[country == 'Peru', perc.change.diag.prison := 0]

# add ratios
all_countries_mm_merged[,`diag.rate.prison/outside` := d1/d4]
all_countries_mm_merged[,`prog.rate.prison/outside` := c1/c4]
all_countries_mm_merged[,`beta.prison/outside` := beta_pp/beta_cc]
all_countries_mm_merged[,`entry.rate.rel/nev.incarc` := 1/`iE:iR`]
all_countries_mm_merged[,iE := iR*`iE:iR`]

setnames(all_countries_mm_merged, c('beta_cc','c4','d4','b','alpha','sc','d1w','c1w','iE'),
         c('beta.out','prog.rate.out','diag.rate.out','E.to.L.rate','reinfect.RR',
           'selfcure.rate','diag.rel.weight','prog.rel.weight',
           'entry.rate.nev.incarc'))

# identify predictors
predictor_cols <- c('beta.out','beta.prison/outside','prog.rate.out','prog.rate.prison/outside','perc.change.beta.out',
                    'diag.rate.out','perc.change.diag.out', 'beta_pc',
                    # 'diag.rate.prison/outside','perc.change.beta.prison','perc.change.diag.prison', # don't include these bc they are not applicable for all six countries
                    'mu.incarc.irr','mu.postrel.irr','muI','E.to.L.rate','reinfect.RR','selfcure.rate','diag.rel.weight','prog.rel.weight', # prespecified
                    'entry.rate.nev.incarc','entry.rate.rel/nev.incarc','r','iE_perc_change','r_perc_change')

all_countries_mm_merged2019_melt <- melt(all_countries_mm_merged[time == 2019, c(predictor_cols, 'country','RR.obs.v.counter'), with=F], 
                                         id.vars = c('country','RR.obs.v.counter'),
                                         variable.name = 'param')
all_countries_mm_merged2019_melt$param <- factor(all_countries_mm_merged2019_melt$param,
                                                 levels = c('entry.rate.nev.incarc','entry.rate.rel/nev.incarc','iE_perc_change','r','r_perc_change',
                                                            'beta.out','beta.prison/outside','beta_pc','perc.change.beta.out',
                                                            'prog.rate.out','prog.rate.prison/outside',
                                                            'diag.rate.out','perc.change.diag.out',
                                                            'mu.incarc.irr','mu.postrel.irr','muI','E.to.L.rate','reinfect.RR','selfcure.rate','diag.rel.weight','prog.rel.weight'
                                                 ))

all_countries_standardized <- all_countries_mm_merged[time == 2019,c(predictor_cols,
                                                                     'RR.obs.v.counter','country'),with=F]

# add country-level fixed effects: notifs in gen pop, notifs in prison, incarc prev
all_countries_standardized <- merge(all_countries_standardized, 
                                    who_data[year == 2019 & country %in% analyzed_countries, c('c_newinc_per100k',
                                                                                               'country'), with=F],
                                    by='country')
all_countries_standardized <- merge(all_countries_standardized, 
                                    prison_estimates[Year == 2019 & Country %in% analyzed_countries, 
                                                     c('NR_Obs_New',
                                                       'Country'), with=F],
                                    by.x='country', by.y='Country')
all_countries_standardized <- merge(all_countries_standardized, 
                                    latam_incarc_data[Year == 2019 & Country %in% analyzed_countries, 
                                                     c('Country','Adjusted Incarceration Prevalence'), with=F],
                                    by.x='country', by.y='Country')
predictor_cols <- c(predictor_cols, c('c_newinc_per100k','NR_Obs_New',
                                      'Adjusted Incarceration Prevalence'))

# standardize all variables
all_countries_standardized[, (predictor_cols) := lapply(.SD, scale), .SDcols=(predictor_cols)]

metamod <- lmer(RR.obs.v.counter ~ beta.out + perc.change.beta.out +
               `beta.prison/outside` + 
               prog.rate.out + `prog.rate.prison/outside` + 
               diag.rate.out + 
               perc.change.diag.out +
               `mu.incarc.irr` + `mu.postrel.irr` +
               muI + 
               E.to.L.rate +
               reinfect.RR +
               selfcure.rate +
               beta_pc +
               diag.rel.weight +
               prog.rel.weight +
              `entry.rate.rel/nev.incarc` + iE_perc_change + r_perc_change + 
               r + entry.rate.nev.incarc +
               NR_Obs_New + # country-level fixed effects
               c_newinc_per100k + # country-level fixed effects
               `Adjusted Incarceration Prevalence` + # country-level fixed effects
               (beta.out + perc.change.beta.out + r + diag.rate.out + prog.rate.out || country), # added based on AIC
             data=all_countries_standardized)
AIC(metamod)
summary(metamod)
ranef(metamod)

saveRDS(metamod, file='~/Desktop/Stanford/TBprisons_git/3-outputs/metamodel.RDS')

coefs <- data.table(data.frame('coef'=fixef(metamod)), keep.rownames = 'param')
ci <- data.table(data.frame(confint(metamod, parm=names(fixef(metamod)), level = 0.95, method='Wald')), keep.rownames = 'param')
colnames(ci) <- c('param','ci.lower','ci.upper')
coefs <- merge(coefs, ci, by='param')
setorder(coefs, -coef)
coefs$param <- factor(coefs$param, levels = coefs$param)
ggplot(coefs[param != '(Intercept)']) + geom_point(aes(x=param, y=coef)) +
  geom_segment(aes(x=param, xend=param, y=ci.lower, yend=ci.upper)) +
  theme_bw() + geom_hline(yintercept = 0, linetype = 'dotted') +
  coord_flip() + labs(y='std coef')

cat(paste0(coefs$param, '\n'))
cat(paste0(formatC(coefs$coef, digits=2, format='fg', drop0trailing = FALSE),
           ' (',formatC(coefs$ci.lower, digits=2, format='fg', drop0trailing = FALSE),
           ', ',
           formatC(coefs$ci.upper, digits=2, format='fg', drop0trailing = FALSE),')\n'))
