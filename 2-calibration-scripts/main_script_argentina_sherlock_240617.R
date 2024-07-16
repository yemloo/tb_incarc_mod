#-------------------------------------------
# Author: Yiran Liu
# Date Modified: 06.17.24
# Country: Argentina
# Description: This script runs calibration for main & sensitivity analyses for
# each country, given a single set of sampled calibration targets, fixed
# parameters, and starting values for calibrated parameters. 
# Inputs:
# - incarceration data
# - distributions for calibration targets & fixed params
# - distributions for starting values of calibrated params
# - model & calibration functions
# - country-specific settings for time-varying params
# Outputs:
# - an RDS object with parameters from each of main & two sensitivity
#   analyses
# - a data frame with model results from observed & counterfactual scenarios for
#   each of the main & sensitivity analyses
#-------------------------------------------
########################################
######## load packages and data ########
########################################
library(deSolve)
library(data.table)

args <- commandArgs(trailingOnly = T)
i <- as.integer(args[1]) # retrieve iteration index from bash script call
# today <- format(Sys.time(), "%y%m%d")
today <- '240617'
country_p <- 'Argentina'

# ### LOCAL STUFF ####
# wd <- '~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/' # local
# wd_params <- '~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/params/uncertainty_analysis/' # local
# source('~/Desktop/Stanford/TBprisons_git/0-model-functions/tb_incarc_models_240528_newformulation.R') # script with model functions # local
# source('~/Desktop/Stanford/TBprisons_git/0-model-functions/calc_error_wrappers.R') # wrapper functions to calculate errors during optimization # local
# source(paste0('~/Desktop/Stanford/TBprisons_git/1-data-and-settings/', gsub(' ', '', tolower(country_p)), '_main_settings_', today,'.R')) # script with country settings for time-varying params # local

wd <- '~/TBprisons/' # Sherlock
wd_params <- paste0(wd, 'params/') # Sherlock
source(paste0(wd, 'scripts/tb_incarc_models_240528_newformulation.R')) # script with model functions
source(paste0(wd, 'scripts/calc_error_wrappers.R')) # wrapper functions to calculate errors during optimization

# read in country-specific settings
source(paste0(wd, 'scripts/', gsub(' ', '', tolower(country_p)), '_main_settings_', today,'.R')) # script with country settings for time-varying params

# read in calibration targets
latam_incarc_data <- fread(paste0(wd, 'data/latam_incarc_data_231203.csv'))
prison_estimates <- fread(paste0(wd, 'data/prison_estimates_latam.csv'))
who_data <- fread(paste0(wd, 'data/who_data_wsplines.csv'))
setorder(who_data, country, year)
setorder(prison_estimates, Country, Year)

# read in distributions for targets and fixed params
all_samps <- fread(paste0(wd_params, 'argentina_samps_240617.csv'))
who_incid_factor_samp <- as.matrix(fread(paste0(wd_params,'argentina_whoincidfactor_231211.csv')))
prison_incid_samp <- as.matrix(fread(paste0(wd_params,'argentina_prison_incid_231211.csv')))
wpp_data <- fread(paste0(wd, 'data/wpp_15pluspop_sixcountries.csv'))

# read in distributions for starting values of calibrated params
all_samps_calib_start <- read.csv(paste0(wd_params, 'argentina_samps_calib_start_240617.csv'), header=T, check.names = FALSE)

####### calibration targets ####### 
incarc_prev=latam_incarc_data[Country==country_p & !is.na(`Adjusted Incarceration Prevalence`),
                              `Adjusted Incarceration Prevalence`]
incarc_prev_yrs=latam_incarc_data[Country==country_p & !is.na(`Adjusted Incarceration Prevalence`),
                                  Year]
fit.end.idx=c(0,3) # fit to 2019 and 2022 equally

admissions=latam_incarc_data[Country==country_p & !is.na(`Admissions Rate`),
                             `Admissions Rate`] # admissions rate per 100k
admissions.years=latam_incarc_data[Country==country_p & !is.na(`Admissions Rate`), Year]
admissions_factor=all_samps[i,]$admissions_factor

who_incid_factor=who_incid_factor_samp[,i]

prison_incid <- prison_incid_samp[,i]
prison_incid_yr <- sort(prison_estimates[Country == country_p & 
                                           !is.na(NR_Obs_New),
                                         Year])

recid_odds=latam_incarc_data[Country==country_p & !is.na(`Recidivism Percentage`),
                             `Recidivism Percentage`] /
  (1-latam_incarc_data[Country==country_p & !is.na(`Recidivism Percentage`),
                       `Recidivism Percentage`]) * 
  all_samps[i,]$recid_factor # adjust the odds by an uncertainty factor
recid_prct <- recid_odds / (1+recid_odds) # convert back to probability
recid_yr=latam_incarc_data[Country==country_p & !is.na(`Recidivism Percentage`),
                           Year]

####### pre-specified parameters (fixed during calibration) ####### 
mort_genpop_func <- approxfun(x = wpp_data[country == country_p & !is.na(life.expect.age15), year], 
                              y=1/wpp_data[country == country_p & !is.na(life.expect.age15), life.expect.age15], 
                              rule = 2, method = 'linear')
mortality_incarc_irr=all_samps[i,]$mortality_incarc_irr
mortality_postrel_irr=all_samps[i,]$mortality_postrel_irr

pop_grow_func <- approxfun(x = wpp_data[year >= 1990 & country == country_p, year],
                           y = wpp_data[year >= 1990 & country == country_p, pop.growth.rate.percent],
                           rule = 2, method = 'linear')
pop_grow <- TRUE

change.r.factor1=all_samps[i,]$r_change_factor1
change.r.factor2=all_samps[i,]$r_change_factor2
SoEf_iR=NA # elsal only
SoE_r=NA # elsal only

beta_pc=all_samps[i,]$beta_pc 
b=all_samps[i,]$b
alpha=all_samps[i,]$alphas
muI=all_samps[i,]$muI
sc=all_samps[i,]$sc
d1w=all_samps[i,]$d1w
c1w=all_samps[i,]$c1w
covid.change.beta.factor=all_samps[i,]$covid.beta.factor
spike.d1.factor=NA # elsal only

prop_E=1/7 # proportion of latent individuals born into model with EARLY latent (vs. late latent) infection

# the below two are Mexico-only:
c2.perc.change=NA
v1.ratio=NA

baseline.dprison=NA
baseline.dcomm=NA

assrt.fctr=1
e=0.000594
gamma=0.01

a=1/7

####### parameters to calibrate ####### 
incarc_param_names <- c('iR','iE:iR','r','k1','k2','k3','covidf')
incarc_params_lower_bounds <- c(0.01, 0.0005, 0.2, 0.00001, 0.00005, 0.00001, 0.6)
incarc_params_upper_bounds <- c(0.2, 0.05, 0.5, 0.00005, 0.0005, 0.0005, 0.9)
incarc_params_start=as.numeric(all_samps_calib_start[i,incarc_param_names]) # may need to adjust columns

tb_param_names <- c('beta_pp',
                    'beta_cc:beta_pp',
                    'c1',
                    'c4:c1',
                    'v',
                    'z',
                    'd',
                    'd1:d4',
                    'covid.d.factor',
                    'p')
tb_params_lower_bounds <- c(15, # beta_pp
                            0.2, # beta_cc:beta_pp
                            0.05, # c1
                            0.3, # c2:c1
                            0.02, # v (change in diagnosis rate outside prison)
                            -1.25, # z (change in beta outside prison period 1),
                            0.5, # baseline diagnosis rate outside prison)
                            0.4, # ratio of d1:d4
                            0.6, # relative diagnosis rate during COVID (d_covid = d*param)
                            0.1) # p (change in beta prison)
tb_params_upper_bounds <- c(40, # beta_pp
                            0.6, # beta_cc:beta_pp
                            0.16, # c1
                            0.8, # c2:c1
                            0.25, # v (change in diagnosis rate outside prison)
                            -0.4, #  z (change in beta outside prison period 1)
                            1.3, # baseline diagnosis rate
                            1, # ratio of d1:d4
                            0.95, # relative diagnosis rate during COVID
                            1) # p (change in beta prison)

tb_params_start=as.numeric(all_samps_calib_start[i,tb_param_names])


####### calibration settings ####### 
incarc_pgtol=0.000001
tb_parscale_factor=2
d_thresh=3
upper_betap_thresh=Inf

##################################################
################  CALIBRATION  ###################
##################################################

#----------------------------------------#
#   First calibrate incarceration params 
#----------------------------------------#
unvarying_incarc_params <- c(mortality_incarc_irr = mortality_incarc_irr,
                             mortality_postrel_irr = mortality_postrel_irr, 
                             mort_genpop_func = mort_genpop_func,
                             a=a,
                             pop_grow=TRUE,
                             pop_grow_func=pop_grow_func,
                             SoEf_iR=SoEf_iR, # elsal only
                             SoE_r=SoE_r)

incarc_param_means <- apply(matrix(data = c(incarc_params_lower_bounds, incarc_params_upper_bounds), ncol = 2), 1, mean)
if (is.null(incarc_params_start)){
  incarc_params_start <- incarc_param_means
}

xstart <- c(P=0, S=0, R=0, N=100000, E=0, Ishadow=0, Ireshadow=0, Eshadow=0)
quasi_2000 <- 500

# adjust times in all.settings based on quasi_2000 for optimization
all.settings.incarc.optim <- all.settings
all.settings.incarc.optim[grep('start|end',names(all.settings.incarc.optim),value=T)] <- 
  as.numeric(all.settings.incarc.optim[grep('start|end',names(all.settings.incarc.optim),value=T)])-2000+quasi_2000

# smallest_error <- Inf # uncomment for troubleshooting
print('Calibrating incarc params! :)')
incarc_optim_res <- optim(par=incarc_params_start, fn=optim_for_iE_iR_perct_grow, 
                          method='L-BFGS-B', country_p=country_p,
                          xstart=xstart,
                          lower=incarc_params_lower_bounds, upper=incarc_params_upper_bounds,
                          control=list(trace=T, parscale=abs(incarc_param_means), maxit=30,
                                       pgtol=incarc_pgtol, factr=1),
                          timeunit=c(0,seq(quasi_2000-15+0.5, quasi_2000+30+0.5)),
                          quasi_2000=quasi_2000,
                          incarc_param_names=incarc_param_names,
                          incarc_prev_known=incarc_prev,
                          fit.end.idx=fit.end.idx, 
                          incarc.prev.times=incarc_prev_yrs-2000+quasi_2000+0.5, # evaluate halfway through year
                          admissions_known=admissions*admissions_factor,
                          admissions.times = admissions.years-2000+quasi_2000+0.5,
                          recid_percent_known = recid_prct, recid.time=recid_yr-2000+quasi_2000+0.5,
                          unvarying_incarc_params = c(unvarying_incarc_params,
                                                      all.settings.incarc.optim))

names(incarc_optim_res$par) <- incarc_param_names
incarc_functions <- get_incarc_functions(incarc_params=incarc_optim_res$par, # named vector, should include iR, iE, r, k1 (at minimum); may include k2, k3, covidf, SoEf
                                         unvarying_incarc_params=c(unvarying_incarc_params,
                                                                   all.settings))

if (country_p == 'El Salvador'){
  new_funcs <- get_incarc_functions_elsal(iE_func=incarc_functions$iE_func,
                                          iR_func=incarc_functions$iR_func,
                                          r_func=incarc_functions$r_func,
                                          SoEf=incarc_optim_res$par[['SoEf']],
                                          SoEf_iR=unvarying_incarc_params[['SoEf_iR']],
                                          SoE_r=unvarying_incarc_params[['SoE_r']],
                                          SoEf2=incarc_optim_res$par[['SoEf2']],
                                          unvarying_incarc_params=c(unvarying_incarc_params,
                                                                    all.settings))
  incarc_functions_noSoE <- incarc_functions
  incarc_functions <- new_funcs
}

incarc_params <- c(incarc_optim_res$par, unvarying_incarc_params)

# record individual errors from best set
output <- ode(
  func=prison.model.with.growth,
  y=xstart,
  times=c(0,seq(1985.5, 2040.5, by=1)),
  parms=incarc_params,
  iR_func=incarc_functions$iR_func,
  iE_func=incarc_functions$iE_func,
  r_func=incarc_functions$r_func,
)

output_dt <- data.table(output)
output_dt[, Ntotal := P+S+R+E+N]

incarc_mod_fit <- list() # save model fits

if (length(admissions.years)>0){
  admissions_obs <- (output_dt[time %in% c(admissions.years+1+0.5), Ishadow] - output_dt[time %in% (admissions.years+0.5), Ishadow]) /
    output_dt[time %in% c(admissions.years+0.5), Ntotal] * 100000 # at present
  
  incarc_mod_fit$admissions.years=admissions.years
  incarc_mod_fit$admissions=admissions_obs
}

incarc_prev_obs <- output_dt[time %in% (incarc_prev_yrs + 0.5), (S + P)/Ntotal*100000] # population incarc per 100K - halfway through year
incarc_mod_fit$incarc_prev_yrs=incarc_prev_yrs
incarc_mod_fit$incarc_prev=incarc_prev_obs

if (country_p == "Mexico"){ # in Mexico, "recid.percent" is the percent of all *admissions* that are re-admissions
  all_admissions_obs <- output_dt[time %in% c(recid_yr+1+0.5), Ishadow] - output_dt[time %in% (recid_yr+0.5), Ishadow] # at present
  readmissions_obs <- output_dt[time %in% c(recid_yr+1+0.5), Ireshadow] - output_dt[time %in% (recid_yr+0.5), Ireshadow] # at present
  percent_readmissions <- readmissions_obs/all_admissions_obs
  
  incarc_mod_fit$recid_yr=recid_yr
  incarc_mod_fit$recid=percent_readmissions
  
} else {
  recid_percent_obs <- output_dt[time %in% (recid_yr+0.5), S] / (output_dt[time %in% (recid_yr+0.5), S] + output_dt[time %in% (recid_yr+0.5), P]) # percentage with prior incarc (at present)
  
  incarc_mod_fit$recid_yr=recid_yr
  incarc_mod_fit$recid=recid_percent_obs
}

saveRDS(incarc_mod_fit, paste0(wd, 'results/', gsub(' ', '', tolower(country_p)), '_', today, '/', 
                          gsub(' ', '', tolower(country_p)), '_', today, '_incarc_mod_fit_', i, '.RDS'))

# Uncomment below if you need to check fit during troubleshooting
# ggplot(output_dt[time>=1990], aes(x=time, y=(S+P)/Ntotal * 100000)) + geom_line() +
#   geom_point(data=latam_incarc_data[Country == country_p],
#              aes(x=Year, y=`Adjusted Incarceration Prevalence`))
# ggplot(output_dt[time>=1990], aes(x=time, y=Ntotal)) + geom_line()
# ggplot(data.table(time=admissions.years, admissions=admissions_obs,
#                   admissions.known=admissions*admissions_factor)) +
#   geom_line(aes(x=time, y=admissions)) +
#   geom_point(aes(x=time, y=admissions.known))
# ggplot(output_dt[time >= 1990]) + geom_line(aes(x=time, y=S/(S+P))) +
#   geom_point(data=data.table(time=recid_yr, recid=recid_prct),
#              aes(x=time, y=recid))
# ggplot() + geom_line(data=data.table(time=recid_yr, recid=percent_readmissions),
#                                      aes(x=time, y=recid)) +
#   geom_point(data=data.table(time=recid_yr, recid=recid_prct),
#              aes(x=time, y=recid))

# incarc errors
incarc_optim_res$e1 <- mean(((incarc_prev_obs - incarc_prev)/incarc_prev)^2)
incarc_optim_res$e2 <- ((incarc_prev_obs[1] - incarc_prev[1])/incarc_prev[1])^2
incarc_optim_res$e3 <- mean(((incarc_prev_obs[length(incarc_prev)-fit.end.idx] - 
                                incarc_prev[length(incarc_prev)-fit.end.idx])/incarc_prev[length(incarc_prev)-fit.end.idx])^2)
if (country_p == "Mexico"){
  incarc_optim_res$e4 = mean(((percent_readmissions - recid_prct)/recid_prct)^2)
} else {
  incarc_optim_res$e4 = mean(((recid_percent_obs - recid_prct)/recid_prct)^2)
}
if (length(admissions.years)>0){
  incarc_optim_res$e5 = mean(((admissions_obs - (admissions*admissions_factor))/
                   (admissions*admissions_factor))^2)
} else {
  incarc_optim_res$e5 = NA
}


#----------------------------------------#
#   Next calibrate TB parameters
#----------------------------------------#
unvarying_params <- c(b=b, e=e,
                      alpha = alpha, gamma=gamma, 
                      assrt.fctr=assrt.fctr, # first w/ assrt.fctr set to 1
                      muI=muI, sc=sc, beta_pc=beta_pc,
                      d1w=d1w, c1w=c1w,
                      prop_E = prop_E,
                      spike.d1.factor=spike.d1.factor, # elsal only
                      # c2.perc.change=c2.perc.change, # mexico only
                      v1.ratio=v1.ratio, # mexico only
                      incarc_params)
  
if (!is.na(baseline.dprison)){
  unvarying_params[['d1']] <- baseline.dprison
}

if (!is.na(baseline.dcomm)){
  unvarying_params[['d3']] <- baseline.dcomm
  unvarying_params[['d4']] <- baseline.dcomm
  
  unvarying_params[['d2']] <- (unvarying_params[['d1']]*unvarying_params[['d1w']]) + (unvarying_params[['d4']]*(1-unvarying_params[['d1w']]))
}

tb_params_mean <- apply(matrix(data = c(tb_params_lower_bounds, tb_params_upper_bounds), ncol = 2), 1, mean)
if (is.null(tb_params_start)){
  tb_params_start <- tb_params_mean
}

quasi_2000 <- quasi_2000_tb

# adjust times in all.settings based on quasi_2000 for optimization
all.settings.tb.optim <- all.settings
all.settings.tb.optim[grep('start|end',names(all.settings.tb.optim),value=T)] <- 
  as.numeric(all.settings.tb.optim[grep('start|end',names(all.settings.tb.optim),value=T)])-2000+quasi_2000

times <- c(0,seq(quasi_2000-15, quasi_2000+25))
# last_plotted_error <- Inf # Uncomment when troubleshooting
# smallest_error <- Inf # Uncomment when troubleshooting
print('Calibrating TB params! :)')
tb_optim_res <- optim(tb_params_start, calc_diffs, method='L-BFGS-B', 
                      lower=tb_params_lower_bounds, upper=tb_params_upper_bounds,
                      control=list(trace=T, parscale=abs(tb_params_mean)*tb_parscale_factor, maxit=30,
                                   pgtol=0.0001, factr=1),
                      param_names=tb_param_names, 
                      iR_func=incarc_functions$iR_func,
                      iE_func=incarc_functions$iE_func,
                      r_func=incarc_functions$r_func,
                      upper_betap_thresh=upper_betap_thresh,
                      d_thresh=d_thresh,
                      times_to_use=times, quasi_2000=quasi_2000,
                      country_p=country_p, 
                      who_incid_factor=who_incid_factor,
                      prison_incid=prison_incid, prison_incid_yr=prison_incid_yr,
                      k=1, unvarying_params=c(unvarying_params,
                                              all.settings.tb.optim))

# save best-fitting parameters
names(tb_optim_res$par) <- tb_param_names
all_params <- c(beta_pp=tb_optim_res$par[['beta_pp']], 
                beta_cc=tb_optim_res$par[['beta_pp']]*tb_optim_res$par[['beta_cc:beta_pp']], 
                c1=tb_optim_res$par[['c1']],
                c2=tb_optim_res$par[['c1']]*tb_optim_res$par[['c4:c1']],
                c3=tb_optim_res$par[['c1']]*tb_optim_res$par[['c4:c1']],
                c4=tb_optim_res$par[['c1']]*tb_optim_res$par[['c4:c1']],
                unvarying_params, incarc_optim_res$par)
# all_params[['c2']] <- (all_params[['c1']]*unvarying_params[['c1w']]) + (all_params[['c4']]*(1-unvarying_params[['c1w']]))
if ('d' %in% tb_param_names){
  all_params[['d1']]=tb_optim_res$par[['d']]
  all_params[['d2']]=tb_optim_res$par[['d']]
  all_params[['d3']]=tb_optim_res$par[['d']]
  all_params[['d4']]=tb_optim_res$par[['d']]
}
if ('v' %in% tb_param_names){
  all_params[['v']] <- tb_optim_res$par[['v']]
}
if ('j' %in% tb_param_names){
  all_params[['j']] <- tb_optim_res$par[['j']]
}
if ('p' %in% tb_param_names){
  all_params[['p']] <- tb_optim_res$par[['p']]
}
if ('z' %in% tb_param_names){
  all_params[['z']] <- tb_optim_res$par[['z']]
}
if ('d1:d4' %in% tb_param_names){
  all_params[['d1']] <- tb_optim_res$par[['d']]*tb_optim_res$par[['d1:d4']]
  # all_params[['d2']] <- (all_params[['d1']]*unvarying_params[['d1w']]) + (all_params[['d4']]*(1-unvarying_params[['d1w']]))
}
if ('v1ratio' %in% tb_param_names | !is.na(v1.ratio)){
  total.change.d4 <- all_params[['v']]*(all.settings$change.d.end1-all.settings$change.d.start1) # calculate total change in diagnosis rate outside prison
  if (all.settings$change.d.start2 != Inf & all.settings$change.d.end2 != Inf){
    total.change.d4 <- total.change.d4 + all_params[['v']]*all.settings$change.d.2.factor*(all.settings$change.d.end2-all.settings$change.d.start2)
  }
  max_possible_v1 <- total.change.d4 / (all.settings$change.d1.end1 - all.settings$change.d1.start1)
  if ('v1ratio' %in% tb_param_names){
    all_params[['v1']] <- max_possible_v1*tb_optim_res$par[['v1ratio']]
  } else if (!is.na(v1.ratio)) {
    all_params[['v1']] <- max_possible_v1*v1.ratio
  }
}
if (country_p == 'Mexico'){ # for Mexico, change rates in beta in periods 2 and 3 are calibrated
  all_params[['j']] <- all_params[['c1']]*all_params[['c4:c1']]*c2.perc.change # for Mexico, j is not calibrated, it is sampled from a distribution as yearly percent change from baseline
  if ('z2:z' %in% tb_param_names){
    all_params[['z2:z']] <- tb_optim_res$par[['z2:z']]
    all.settings[['change.beta.2.factor']] <- tb_optim_res$par[['z2:z']]
  }
  if ('z3:z' %in% tb_param_names){
    all_params[['z3:z']] <- tb_optim_res$par[['z3:z']]
    all.settings[['change.beta.3.factor']] <- tb_optim_res$par[['z3:z']]
  }
} 
if ('covid.d.factor' %in% tb_param_names){
  all_params[['covid.d.factor']]=tb_optim_res$par[['covid.d.factor']]
  # covid.change.d.factor=tb_optim_res$par[['covid.d.factor']]
}

#---------------------------------------------------#
#   Now run w calibrated params to get outputs 
#---------------------------------------------------#
horiz.start=1990
horiz.end=2036
time.increment=0.5
timeunit <- c(0,seq(horiz.start-5,horiz.end+1, by=time.increment))
xstart <- c(S1=0.005*0.95*100000, E1=0, L1=0, I1=0.005*0.05*100000, R1=0,
            S2=0.005*0.95*100000, E2=0, L2=0, I2=0.005*0.05*100000, R2=0,
            S3=0, E3=0, L3=0, I3=0, R3=0,
            S4=0.99*0.95*100000, E4=0, L4=0, I4=0.99*0.05*100000, R4=0,
            I1shadow=0, I2shadow=0, I3shadow=0, I4shadow=0,
            D1shadow=0, D2shadow=0, D3shadow=0, D4shadow=0,
            muI1shadow=0, muI2shadow=0, muI3shadow=0, muI4shadow=0)

######## Mass incarc & leveling off ######## 
output_stable <- ode(
  func=tb_model_for_calibration,
  y=xstart,
  times=timeunit,
  parms=c(all_params, all.settings),
  iR_func=incarc_functions$iR_func,
  iE_func=incarc_functions$iE_func,
  r_func=incarc_functions$r_func,
  quasi_2000=2000
)

output_dt_stable <- data.table(output_stable)
output_dt_stable[,`:=`(N1=S1+E1+L1+I1+R1,
                       N2=S2+E2+L2+I2+R2,
                       N3=S3+E3+L3+I3+R3,
                       N4=S4+E4+L4+I4+R4)]

rates_stable <- get_annualized_rates(output_dt_stable, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)

prison_TB_incid_obs <- rates_stable[pop == 'Prison' & variable == 'I' & time %in% (prison_incid_yr), rate_per100k]
prison_TB_notif_yrs <- prison_estimates[Country == country_p & !is.na(NR_Obs_New) &
                                          Year >= 2000 & Year <= 2022, Year]
prison_TB_notif_obs <- rates_stable[pop == 'Prison' & variable == 'D' & 
                                  time %in% (prison_TB_notif_yrs), 
                                rate_per100k]

comb_TB_incid_yrs <- seq(2000, 2022)
comb_TB_notif_yrs <- who_data[country == country_p & year >= 1990 & year <= 2022 &
                                !is.na(c_newinc_per100k), year]

if (country_p == 'Mexico'){
  comb_TB_incid_yrs <- setdiff(comb_TB_incid_yrs, c(2004))
  # comb_TB_notif_yrs <- setdiff(comb_TB_notif_yrs, c(1995, 2004)) # remove these two years which seem like outliers and are messing up the fit
}
comb_TB_incid_obs <- rates_stable[pop == 'Combined' & variable == 'I' & 
                                time %in% comb_TB_incid_yrs, rate_per100k]
comb_TB_notif_obs <- rates_stable[pop == 'Combined' & variable == 'D' &
                                time %in% comb_TB_notif_yrs, rate_per100k]

# prison incidence
tb_optim_error1 <- mean(abs((prison_TB_incid_obs - prison_incid)/prison_incid))

# combined incidence
comm_TB_incid_exp <- who_data[country == country_p & year %in% comb_TB_incid_yrs, c_newinc_per100k]*who_incid_factor
tb_optim_error2 <- mean(abs((comb_TB_incid_obs - comm_TB_incid_exp)/
                              comm_TB_incid_exp))

# prison notifications
tb_optim_error3 <- mean(abs((prison_TB_notif_obs - prison_estimates[Country == country_p & 
                                                                      Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)/
                              (prison_estimates[Country == country_p & 
                                                  Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)))

# combined notifications
tb_optim_error4 <- mean(abs((comb_TB_notif_obs - who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k])/
                              who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k]))

rates_stable <- rbind(rates_stable, data.table(pop='Prison', variable='pop',
                                               rate_per100k=output_dt_stable[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                               total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                      data.table(pop='Post-Release', variable='pop',
                                 rate_per100k=output_dt_stable[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                 total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                      data.table(pop='Formerly Incarc', variable='pop',
                                 rate_per100k=output_dt_stable[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                 total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)
print('stable...done')
rates_stable$scenario <- 'Leveled Off'

##### save all parameters & calibration targets #####
optim_res <- list(idx=i,
                  incarc_functions=incarc_functions,
                  main_params=c(all_params,
                                idx=i,
                                change.r.factor1=change.r.factor1,
                                change.r.factor2=change.r.factor2,
                                change.beta.2.factor=all.settings[['change.beta.2.factor']],
                                change.beta.3.factor=all.settings[['change.beta.3.factor']],
                                covid.change.beta.factor=covid.change.beta.factor,
                                recid_factor=all_samps[i,]$recid_factor,
                                mu.incarc.irr=mortality_incarc_irr,
                                mu.postrel.irr=mortality_postrel_irr,
                                admissions.factor=admissions_factor,
                                who_incid_factor=mean(who_incid_factor),
                                prison_incid_factor=mean(prison_incid / (prison_estimates[Country == country_p & 
                                                                                            !is.na(NR_Obs_New) &
                                                                                            Year %in% prison_incid_yr,
                                                                                          NR_Obs_New]*100000)),
                                c2.perc.change=c2.perc.change,
                                v1.ratio=v1.ratio,
                                spike.d1.factor=spike.d1.factor,
                                incarc_optim_error=incarc_optim_res$value,
                                incarc_optim_e1=incarc_optim_res$e1,
                                incarc_optim_e2=incarc_optim_res$e2,
                                incarc_optim_e3=incarc_optim_res$e3,
                                incarc_optim_e4=incarc_optim_res$e4,
                                incarc_optim_e5=incarc_optim_res$e5,
                                tb_optim_error=tb_optim_res$value,
                                tb_optim_e1=tb_optim_error1,
                                tb_optim_e2=tb_optim_error2,
                                tb_optim_e3=tb_optim_error3,
                                tb_optim_e4=tb_optim_error4))
saveRDS(optim_res, paste0(wd, 'results/', gsub(' ', '', tolower(country_p)), '_', today, '/', 
                          gsub(' ', '', tolower(country_p)), '_', today, '_params_', i, '.RDS'))

######## Counterfactual ########
get_iR_cf <- function(x) incarc_optim_res$par[['iR']]
get_iE_cf <- function(x) incarc_optim_res$par[['iR']]*incarc_optim_res$par[['iE:iR']]
get_r_cf <- function(x) incarc_optim_res$par[['r']]
# remove changes in beta_pp if any
all.settings.cf <- all.settings
all.settings.cf[c('change.betap.start1',
                  'change.betap.end1',
                  'change.betap.start2',
                  'change.betap.end2',
                  'change.betap.2.factor',
                  'change.betap.exp')] <- list(Inf,Inf,Inf,Inf,1,FALSE)

output_counterfactual <- ode(
  func=tb_model_for_calibration,
  y=xstart,
  times=timeunit,
  parms=c(all_params, all.settings.cf),
  iR_func=get_iR_cf,
  iE_func=get_iE_cf,
  r_func=get_r_cf,
  quasi_2000=2000
)

output_dt_counterfactual <- data.table(output_counterfactual)
output_dt_counterfactual[,`:=`(N1=S1+E1+L1+I1+R1,
                               N2=S2+E2+L2+I2+R2,
                               N3=S3+E3+L3+I3+R3,
                               N4=S4+E4+L4+I4+R4)]

# plot to make sure incarc prev remains relatively steady under counterfactual
# ggplot() +
#   geom_line(data=output_dt_counterfactual[time>=1990 & time<=2020], aes(x=time, y=N1/(N1+N2+N3+N4)*100000)) +
#   geom_line(data=output_dt_stable[time>=1990 & time<=2020], aes(x=time, y=N1/(N1+N2+N3+N4)*100000))

rates_counterfactual <- get_annualized_rates(output_dt_counterfactual, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)
rates_counterfactual <- rbind(rates_counterfactual, data.table(pop='Prison', variable='pop',
                                                               rate_per100k=output_dt_counterfactual[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                                               total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                              data.table(pop='Post-Release', variable='pop',
                                         rate_per100k=output_dt_counterfactual[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                         total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                              data.table(pop='Formerly Incarc', variable='pop',
                                         rate_per100k=output_dt_counterfactual[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                         total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)

print('counterfactual...done')
rates_counterfactual$scenario <- 'No Growth Since 1990'

rates_all <- rbind(rates_stable, rates_counterfactual)
rates_all$idx <- i

##################################################
########## SENSITIVITY ANALYSES ##################
##################################################
tb_params_start <- tb_optim_res$par

##### 1. No increase in beta_pp #####
if (all.settings[['change.betap.start1']] != Inf){
  all.settings.sens1 <- all.settings
  all.settings.sens1[c('change.betap.start1',
                       'change.betap.end1',
                       'change.betap.start2',
                       'change.betap.end2',
                       'change.betap.2.factor',
                       'change.betap.exp')] <- list(Inf,Inf,Inf,Inf,1,FALSE)
  all.settings.tb.optim.sens1 <- all.settings.sens1
  all.settings.tb.optim.sens1[grep('start|end',names(all.settings.tb.optim.sens1),value=T)] <-
    as.numeric(all.settings.tb.optim.sens1[grep('start|end',names(all.settings.tb.optim.sens1),value=T)])-2000+quasi_2000

  # first re-calculate the error
  new_error <- calc_diffs(tb_params_start,
                          param_names=tb_param_names,
                          iR_func=incarc_functions$iR_func, iE_func=incarc_functions$iE_func, r_func=incarc_functions$r_func,
                          upper_betap_thresh=upper_betap_thresh,
                          d_thresh=d_thresh,
                          times_to_use=times, quasi_2000=quasi_2000,
                          country_p=country_p,
                          who_incid_factor=who_incid_factor,
                          prison_incid=prison_incid, prison_incid_yr=prison_incid_yr,
                          k=1, unvarying_params=c(unvarying_params,
                                                  all.settings.tb.optim.sens1))
  if (new_error <= 1.1*tb_optim_res$value){
    print('Similar error; NOT recalibrating for sensitivity analysis 1: no increase in beta_pp')
    all_params_sens1 <- all_params
    tb_optim_res_sens1 <- list()
    tb_optim_res_sens1[['par']] <- tb_optim_res$par
    tb_optim_res_sens1[['value']] <- new_error
  } else {
    # last_plotted_error <- Inf # Uncomment when troubleshooting
    # smallest_error <- Inf # Uncomment when troubleshooting
    print('Calibrating for sensitivity analysis 1: no increase in beta_pp')
    params_error_thresh <- NULL
    error_reached <- NULL
    tb_optim_res_sens1 <- NULL
    tryCatch({tb_optim_res_sens1 <- optim(tb_params_start, calc_diffs, method='L-BFGS-B',
                                  lower=tb_params_lower_bounds, upper=tb_params_upper_bounds,
                                  control=list(trace=T, parscale=abs(tb_params_mean)*tb_parscale_factor, maxit=3,
                                               pgtol=0.0001, factr=1),
                                  param_names=tb_param_names,
                                iR_func=incarc_functions$iR_func, iE_func=incarc_functions$iE_func, r_func=incarc_functions$r_func,
                                upper_betap_thresh=upper_betap_thresh,
                                  d_thresh=d_thresh,
                                  times_to_use=times, quasi_2000=quasi_2000,
                                  country_p=country_p,
                                  who_incid_factor=who_incid_factor,
                                  prison_incid=prison_incid, prison_incid_yr=prison_incid_yr,
                                  k=1, unvarying_params=c(unvarying_params,
                                                          all.settings.tb.optim.sens1),
                                error_thresh=tb_optim_res$value*1.1) # can stop the calibration as soon as it is within range of calibration error from main analysis
    }, error = function(e) {
      # Print the error message
      print(paste("Stopping calibration after reaching error threshold with e =", error_reached))
      tb_optim_res_sens1 <<- list(par=params_error_thresh,
                                  value=error_reached)
    })

    # save best-fitting parameters
    names(tb_optim_res_sens1$par) <- tb_param_names
    all_params_sens1 <- c(beta_pp=tb_optim_res_sens1$par[['beta_pp']],
                          beta_cc=tb_optim_res_sens1$par[['beta_pp']]*tb_optim_res_sens1$par[['beta_cc:beta_pp']],
                          c1=tb_optim_res_sens1$par[['c1']],
                          c2=tb_optim_res_sens1$par[['c1']]*tb_optim_res_sens1$par[['c4:c1']],
                          c3=tb_optim_res_sens1$par[['c1']]*tb_optim_res_sens1$par[['c4:c1']],
                          c4=tb_optim_res_sens1$par[['c1']]*tb_optim_res_sens1$par[['c4:c1']],
                          unvarying_params, incarc_optim_res$par)
    # all_params_sens1[['c2']] <- (all_params_sens1[['c1']]*unvarying_params[['c1w']]) + (all_params_sens1[['c4']]*(1-unvarying_params[['c1w']]))
    if ('d' %in% tb_param_names){
      all_params_sens1[['d1']]=tb_optim_res_sens1$par[['d']]
      all_params_sens1[['d2']]=tb_optim_res_sens1$par[['d']]
      all_params_sens1[['d3']]=tb_optim_res_sens1$par[['d']]
      all_params_sens1[['d4']]=tb_optim_res_sens1$par[['d']]
    }
    if ('v' %in% tb_param_names){
      all_params_sens1[['v']] <- tb_optim_res_sens1$par[['v']]
    }
    if ('j' %in% tb_param_names){
      all_params_sens1[['j']] <- tb_optim_res_sens1$par[['j']]
    }
    if ('p' %in% tb_param_names){
      all_params_sens1[['p']] <- tb_optim_res_sens1$par[['p']]
    }
    if ('z' %in% tb_param_names){
      all_params_sens1[['z']] <- tb_optim_res_sens1$par[['z']]
    }
    if ('d1:d4' %in% tb_param_names){
      all_params_sens1[['d1']]=tb_optim_res_sens1$par[['d']]*tb_optim_res_sens1$par[['d1:d4']]
      # all_params_sens1[['d2']] <- (all_params_sens1[['d1']]*unvarying_params[['d1w']]) + (all_params_sens1[['d4']]*(1-unvarying_params[['d1w']]))
    }
    if ('v1ratio' %in% tb_param_names | !is.na(v1.ratio)){
      total.change.d4 <- all_params_sens1[['v']]*(all.settings$change.d.end1-all.settings$change.d.start1) # calculate total change in diagnosis rate outside prison
      if (all.settings$change.d.start2 != Inf & all.settings$change.d.end2 != Inf){
        total.change.d4 <- total.change.d4 + all_params_sens1[['v']]*all.settings$change.d.2.factor*(all.settings$change.d.end2-all.settings$change.d.start2)
      }
      max_possible_v1 <- total.change.d4 / (all.settings$change.d1.end1 - all.settings$change.d1.start1)
      if ('v1ratio' %in% tb_param_names){
        all_params_sens1[['v1']] <- max_possible_v1*tb_optim_res_sens1$par[['v1ratio']]
      } else if (!is.na(v1.ratio)) {
        all_params_sens1[['v1']] <- max_possible_v1*v1.ratio
      }
    }
    
    if (country_p == 'Mexico'){ # for Mexico, change rates in beta in periods 2 and 3 are calibrated
      # all_params_sens1[['j']] <- all_params_sens1[['c1']]*all_params_sens1[['c4:c1']]*c2.perc.change # for Mexico, j is not calibrated, it is sampled from a distribution as yearly percent change from baseline
      if ('z2:z' %in% tb_param_names){
        all_params_sens1[['z2:z']] <- tb_optim_res_sens1$par[['z2:z']]
        all.settings.sens1[['change.beta.2.factor']] <- tb_optim_res_sens1$par[['z2:z']]
      }
      if ('z3:z' %in% tb_param_names){
        all_params_sens1[['z3:z']] <- tb_optim_res_sens1$par[['z3:z']]
        all.settings.sens1[['change.beta.3.factor']] <- tb_optim_res_sens1$par[['z3:z']]
      }
    }
    if ('covid.d.factor' %in% tb_param_names){
      all_params_sens1[['covid.d.factor']]=tb_optim_res_sens1$par[['covid.d.factor']]
      # covid.change.d.factor=tb_optim_res_sens1$par[['covid.d.factor']]
    }
  }

  #---------------------------------------------------#
  #   Now run w calibrated params to get outputs
  #---------------------------------------------------#
  ######## Mass incarc & leveling off ########
  output_stable_sens1 <- ode(
    func=tb_model_for_calibration,
    y=xstart,
    times=timeunit,
    parms=c(all_params_sens1, all.settings.sens1),
    iR_func=incarc_functions$iR_func,
    iE_func=incarc_functions$iE_func,
    r_func=incarc_functions$r_func,
    quasi_2000=2000
  )

  output_dt_stable_sens1 <- data.table(output_stable_sens1)
  output_dt_stable_sens1[,`:=`(N1=S1+E1+L1+I1+R1,
                         N2=S2+E2+L2+I2+R2,
                         N3=S3+E3+L3+I3+R3,
                         N4=S4+E4+L4+I4+R4)]

  rates_stable_sens1 <- get_annualized_rates(output_dt_stable_sens1, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)

  prison_TB_incid_obs <- rates_stable_sens1[pop == 'Prison' & variable == 'I' & time %in% (prison_incid_yr), rate_per100k]

  prison_TB_notif_obs <- rates_stable_sens1[pop == 'Prison' & variable == 'D' &
                                        time %in% (prison_TB_notif_yrs),
                                      rate_per100k]

  comb_TB_incid_obs <- rates_stable_sens1[pop == 'Combined' & variable == 'I' &
                                      time %in% comb_TB_incid_yrs, rate_per100k]
  comb_TB_notif_obs <- rates_stable_sens1[pop == 'Combined' & variable == 'D' &
                                      time %in% comb_TB_notif_yrs, rate_per100k]

  # prison incidence
  tb_optim_error1 <- mean(abs((prison_TB_incid_obs - prison_incid)/prison_incid))

  # combined incidence
  tb_optim_error2 <- mean(abs((comb_TB_incid_obs - comm_TB_incid_exp)/
                                comm_TB_incid_exp))

  # prison notifications
  tb_optim_error3 <- mean(abs((prison_TB_notif_obs - prison_estimates[Country == country_p &
                                                                        Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)/
                                (prison_estimates[Country == country_p &
                                                    Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)))

  # combined notifications
  tb_optim_error4 <- mean(abs((comb_TB_notif_obs - who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k])/
                                who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k]))

  rates_stable_sens1 <- rbind(rates_stable_sens1, data.table(pop='Prison', variable='pop',
                                                 rate_per100k=output_dt_stable_sens1[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                                 total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                        data.table(pop='Post-Release', variable='pop',
                                   rate_per100k=output_dt_stable_sens1[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                   total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                        data.table(pop='Formerly Incarc', variable='pop',
                                   rate_per100k=output_dt_stable_sens1[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                   total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)
  print('sens 1: stable...done')
  rates_stable_sens1$scenario <- 'Leveled Off'

  ##### save all parameters & calibration targets #####
  optim_res[['sens1_params']] <- c(tb_optim_res_sens1$par,
                                   tb_optim_error=tb_optim_res_sens1$value,
                                   tb_optim_e1=tb_optim_error1,
                                   tb_optim_e2=tb_optim_error2,
                                   tb_optim_e3=tb_optim_error3,
                                   tb_optim_e4=tb_optim_error4)
  saveRDS(optim_res, paste0(wd, 'results/', gsub(' ', '', tolower(country_p)), '_', today, '/',
                            gsub(' ', '', tolower(country_p)), '_', today, '_params_', i, '.RDS'))


  ######## Counterfactual ########
  output_counterfactual_sens1 <- ode(
    func=tb_model_for_calibration,
    y=xstart,
    times=timeunit,
    parms=c(all_params_sens1, all.settings.sens1), # same as cf
    iR_func=get_iR_cf,
    iE_func=get_iE_cf,
    r_func=get_r_cf,
    quasi_2000=2000
  )

  output_dt_counterfactual_sens1 <- data.table(output_counterfactual_sens1)
  output_dt_counterfactual_sens1[,`:=`(N1=S1+E1+L1+I1+R1,
                                 N2=S2+E2+L2+I2+R2,
                                 N3=S3+E3+L3+I3+R3,
                                 N4=S4+E4+L4+I4+R4)]

  rates_counterfactual_sens1 <- get_annualized_rates(output_dt_counterfactual_sens1, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)
  rates_counterfactual_sens1 <- rbind(rates_counterfactual_sens1, data.table(pop='Prison', variable='pop',
                                                                 rate_per100k=output_dt_counterfactual_sens1[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                                                 total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                                data.table(pop='Post-Release', variable='pop',
                                           rate_per100k=output_dt_counterfactual_sens1[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                           total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                                data.table(pop='Formerly Incarc', variable='pop',
                                           rate_per100k=output_dt_counterfactual_sens1[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                           total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)

  print('sens 1: counterfactual...done')
  rates_counterfactual_sens1$scenario <- 'No Growth Since 1990'

  rates_all_sens1 <- rbind(rates_stable_sens1, rates_counterfactual_sens1)
  rates_all_sens1$idx <- i
  # write.csv(rates_all_sens1, paste0(wd, 'results/model_outputs_', gsub(' ', '', tolower(country_p)), '_', today, '/',
  #                             gsub(' ', '', tolower(country_p)), '_', today, '_sens1_outputs_', i, '.csv'), row.names=F)

}

##### 2. Assortative mixing #####
new_assrt.fctr <- 3
unvarying_params[['assrt.fctr']] <- new_assrt.fctr
new_error <- calc_diffs(tb_params_start,
                        iR_func=incarc_functions$iR_func, iE_func=incarc_functions$iE_func, r_func=incarc_functions$r_func, 
                        param_names=tb_param_names, 
                        upper_betap_thresh=upper_betap_thresh,
                        d_thresh=d_thresh,
                        times_to_use=times, quasi_2000=quasi_2000,
                        country_p=country_p, 
                        who_incid_factor=who_incid_factor,
                        prison_incid=prison_incid, prison_incid_yr=prison_incid_yr,
                        # c2.perc.change=c2.perc.change, # Mexico-only
                        # v1.ratio=v1.ratio, # Mexico-only
                        k=1, unvarying_params=c(unvarying_params,
                                                all.settings.tb.optim))
if (new_error <= 1.1*tb_optim_res$value){
  print('Similar error; NOT re-calibrating for sensitivity analysis 2: assortative mixing')
  all_params_sens2 <- all_params
  all_params_sens2[['assrt.fctr']] <- new_assrt.fctr
  tb_optim_res_sens2 <- list()
  tb_optim_res_sens2[['par']] <- tb_optim_res$par
  tb_optim_res_sens2[['value']] <- new_error
} else {
  # last_plotted_error <- Inf
  # smallest_error <- Inf
  print('Sensitivity analysis 2: assortative mixing')
  params_error_thresh <- NULL
  error_reached <- NULL
  tb_optim_res_sens2 <- NULL
  tryCatch({tb_optim_res_sens2 <- optim(tb_params_start, calc_diffs, method='L-BFGS-B', 
                                        lower=tb_params_lower_bounds, upper=tb_params_upper_bounds,
                                        control=list(trace=T, parscale=abs(tb_params_mean)*tb_parscale_factor, maxit=3,
                                                     pgtol=0.0001, factr=1),
                                        iR_func=incarc_functions$iR_func, iE_func=incarc_functions$iE_func, r_func=incarc_functions$r_func,
                                        param_names=tb_param_names, 
                                        upper_betap_thresh=upper_betap_thresh,
                                        d_thresh=d_thresh,
                                        times_to_use=times, quasi_2000=quasi_2000,
                                        country_p=country_p, 
                                        who_incid_factor=who_incid_factor,
                                        # muTB_factor=muTB_factor,
                                        prison_incid=prison_incid, prison_incid_yr=prison_incid_yr,
                                        # c2.perc.change=c2.perc.change, # Mexico-only
                                        # v1.ratio=v1.ratio, # Mexico-only
                                        k=1, unvarying_params=c(unvarying_params,
                                                                all.settings.tb.optim),
                                        error_thresh=tb_optim_res$value*1.1) # can stop the calibration as soon as it is within range of calibration error from main analysis
  }, error = function(e) {
    # Print the error message
    print(paste("Stopping calibration after reaching error threshold with e =", error_reached))
    tb_optim_res_sens2 <<- list(par=params_error_thresh,
                                value=error_reached)
  })
  
  # save best-fitting parameters
  names(tb_optim_res_sens2$par) <- tb_param_names
  all_params_sens2 <- c(beta_pp=tb_optim_res_sens2$par[['beta_pp']], 
                        beta_cc=tb_optim_res_sens2$par[['beta_pp']]*tb_optim_res_sens2$par[['beta_cc:beta_pp']], 
                        c1=tb_optim_res_sens2$par[['c1']],
                        c2=tb_optim_res_sens2$par[['c1']]*tb_optim_res_sens2$par[['c4:c1']],
                        c3=tb_optim_res_sens2$par[['c1']]*tb_optim_res_sens2$par[['c4:c1']],
                        c4=tb_optim_res_sens2$par[['c1']]*tb_optim_res_sens2$par[['c4:c1']],
                        unvarying_params, incarc_optim_res$par)
  # all_params_sens2[['c2']] <- (all_params_sens2[['c1']]*unvarying_params[['c1w']]) + (all_params_sens2[['c4']]*(1-unvarying_params[['c1w']]))
  if ('d' %in% tb_param_names){
    all_params_sens2[['d1']]=tb_optim_res_sens2$par[['d']]
    all_params_sens2[['d2']]=tb_optim_res_sens2$par[['d']]
    all_params_sens2[['d3']]=tb_optim_res_sens2$par[['d']]
    all_params_sens2[['d4']]=tb_optim_res_sens2$par[['d']]
  }
  if ('v' %in% tb_param_names){
    all_params_sens2[['v']] <- tb_optim_res_sens2$par[['v']]
  }
  if ('j' %in% tb_param_names){
    all_params_sens2[['j']] <- tb_optim_res_sens2$par[['j']]
  }
  if ('p' %in% tb_param_names){
    all_params_sens2[['p']] <- tb_optim_res_sens2$par[['p']]
  }
  if ('z' %in% tb_param_names){
    all_params_sens2[['z']] <- tb_optim_res_sens2$par[['z']]
  }
  if ('d1:d4' %in% tb_param_names){
    all_params_sens2[['d1']]=tb_optim_res_sens2$par[['d']]*tb_optim_res_sens2$par[['d1:d4']]
    # all_params_sens2[['d2']] <- (all_params_sens2[['d1']]*unvarying_params[['d1w']]) + (all_params_sens2[['d4']]*(1-unvarying_params[['d1w']]))
  }
  if ('v1ratio' %in% tb_param_names | !is.na(v1.ratio)){
    total.change.d4 <- all_params_sens2[['v']]*(all.settings$change.d.end1-all.settings$change.d.start1) # calculate total change in diagnosis rate outside prison
    if (all.settings$change.d.start2 != Inf & all.settings$change.d.end2 != Inf){
      total.change.d4 <- total.change.d4 + all_params_sens2[['v']]*all.settings$change.d.2.factor*(all.settings$change.d.end2-all.settings$change.d.start2)
    }
    max_possible_v1 <- total.change.d4 / (all.settings$change.d1.end1 - all.settings$change.d1.start1)
    if ('v1ratio' %in% tb_param_names){
      all_params_sens2[['v1']] <- max_possible_v1*tb_optim_res_sens2$par[['v1ratio']]
    } else if (!is.na(v1.ratio)) {
      all_params_sens2[['v1']] <- max_possible_v1*v1.ratio
    }
  }
  
  if (country_p == 'Mexico'){ # for Mexico, change rates in beta in periods 2 and 3 are calibrated
    # all_params_sens2[['j']] <- all_params_sens2[['c1']]*all_params_sens2[['c4:c1']]*c2.perc.change # for Mexico, j is not calibrated, it is sampled from a distribution as yearly percent change from baseline
    if ('z2:z' %in% tb_param_names){
      all_params_sens2[['z2:z']] <- tb_optim_res_sens2$par[['z2:z']]
      all.settings[['change.beta.2.factor']] <- tb_optim_res_sens2$par[['z2:z']]
    }
    if ('z3:z' %in% tb_param_names){
      all_params_sens2[['z3:z']] <- tb_optim_res_sens2$par[['z3:z']]
      all.settings[['change.beta.3.factor']] <- tb_optim_res_sens2$par[['z3:z']]
    }
  } 
  if ('covid.d.factor' %in% tb_param_names){
    all_params_sens2[['covid.d.factor']]=tb_optim_res_sens2$par[['covid.d.factor']]
    # covid.change.d.factor=tb_optim_res_sens2$par[['covid.d.factor']]
  }
}

#---------------------------------------------------#
#   Now run w calibrated params to get outputs 
#---------------------------------------------------#
######## Mass incarc & leveling off ######## 
output_stable_sens2 <- ode(
  func=tb_model_for_calibration,
  y=xstart,
  times=timeunit,
  parms=c(all_params_sens2, all.settings),
  iR_func=incarc_functions$iR_func,
  iE_func=incarc_functions$iE_func,
  r_func=incarc_functions$r_func,
  quasi_2000=2000
)

output_dt_stable_sens2 <- data.table(output_stable_sens2)
output_dt_stable_sens2[,`:=`(N1=S1+E1+L1+I1+R1,
                             N2=S2+E2+L2+I2+R2,
                             N3=S3+E3+L3+I3+R3,
                             N4=S4+E4+L4+I4+R4)]

rates_stable_sens2 <- get_annualized_rates(output_dt_stable_sens2, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)

prison_TB_incid_obs <- rates_stable_sens2[pop == 'Prison' & variable == 'I' & time %in% (prison_incid_yr), rate_per100k]

prison_TB_notif_obs <- rates_stable_sens2[pop == 'Prison' & variable == 'D' & 
                                            time %in% (prison_TB_notif_yrs), 
                                          rate_per100k]

comb_TB_incid_obs <- rates_stable_sens2[pop == 'Combined' & variable == 'I' & 
                                          time %in% comb_TB_incid_yrs, rate_per100k]
comb_TB_notif_obs <- rates_stable_sens2[pop == 'Combined' & variable == 'D' &
                                          time %in% comb_TB_notif_yrs, rate_per100k]

# prison incidence
tb_optim_error1 <- mean(abs((prison_TB_incid_obs - prison_incid)/prison_incid))

# combined incidence
tb_optim_error2 <- mean(abs((comb_TB_incid_obs - comm_TB_incid_exp)/
                              comm_TB_incid_exp))

# prison notifications
tb_optim_error3 <- mean(abs((prison_TB_notif_obs - prison_estimates[Country == country_p & 
                                                                      Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)/
                              (prison_estimates[Country == country_p & 
                                                  Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)))

# combined notifications
tb_optim_error4 <- mean(abs((comb_TB_notif_obs - who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k])/
                              who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k]))

rates_stable_sens2 <- rbind(rates_stable_sens2, data.table(pop='Prison', variable='pop',
                                                           rate_per100k=output_dt_stable_sens2[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                                           total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                            data.table(pop='Post-Release', variable='pop',
                                       rate_per100k=output_dt_stable_sens2[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                       total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                            data.table(pop='Formerly Incarc', variable='pop',
                                       rate_per100k=output_dt_stable_sens2[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                       total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)
print('sens 2: stable...done')
rates_stable_sens2$scenario <- 'Leveled Off'

##### save all parameters & calibration targets #####
optim_res[['sens2_params']] <- c(tb_optim_res_sens2$par,
                                 tb_optim_error=tb_optim_res_sens2$value,
                                 tb_optim_e1=tb_optim_error1,
                                 tb_optim_e2=tb_optim_error2,
                                 tb_optim_e3=tb_optim_error3,
                                 tb_optim_e4=tb_optim_error4)
saveRDS(optim_res, paste0(wd, 'results/', gsub(' ', '', tolower(country_p)), '_', today, '/', 
                          gsub(' ', '', tolower(country_p)), '_', today, '_params_', i, '.RDS'))


######## Counterfactual ######## 
output_counterfactual_sens2 <- ode(
  func=tb_model_for_calibration,
  y=xstart,
  times=timeunit,
  parms=c(all_params_sens2, all.settings.cf),
  iR_func=get_iR_cf,
  iE_func=get_iE_cf,
  r_func=get_r_cf,
  quasi_2000=2000
)

output_dt_counterfactual_sens2 <- data.table(output_counterfactual_sens2)
output_dt_counterfactual_sens2[,`:=`(N1=S1+E1+L1+I1+R1,
                                     N2=S2+E2+L2+I2+R2,
                                     N3=S3+E3+L3+I3+R3,
                                     N4=S4+E4+L4+I4+R4)]

rates_counterfactual_sens2 <- get_annualized_rates(output_dt_counterfactual_sens2, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)
rates_counterfactual_sens2 <- rbind(rates_counterfactual_sens2, data.table(pop='Prison', variable='pop',
                                                                           rate_per100k=output_dt_counterfactual_sens2[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                                                           total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                                    data.table(pop='Post-Release', variable='pop',
                                               rate_per100k=output_dt_counterfactual_sens2[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                               total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                                    data.table(pop='Formerly Incarc', variable='pop',
                                               rate_per100k=output_dt_counterfactual_sens2[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                               total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)

print('sens 2: counterfactual...done')
rates_counterfactual_sens2$scenario <- 'No Growth Since 1990'

rates_all_sens2 <- rbind(rates_stable_sens2, rates_counterfactual_sens2)
rates_all_sens2$idx <- i
rates_all_sens2$analysis <- 'assortative mixing'

##### 3. Prog rate & detection rate is the same in recently released  #####
unvarying_params[['assrt.fctr']] <- 1 # change this back
unvarying_params[['d1w']] <- 0
unvarying_params[['c1w']] <- 0

new_error <- calc_diffs(tb_params_start,
                        iR_func=incarc_functions$iR_func, iE_func=incarc_functions$iE_func, r_func=incarc_functions$r_func, 
                        param_names=tb_param_names, 
                        upper_betap_thresh=upper_betap_thresh,
                        d_thresh=d_thresh,
                        times_to_use=times, quasi_2000=quasi_2000,
                        country_p=country_p, 
                        who_incid_factor=who_incid_factor,
                        prison_incid=prison_incid, prison_incid_yr=prison_incid_yr,
                        # c2.perc.change=c2.perc.change, # Mexico-only
                        # v1.ratio=v1.ratio, # Mexico-only
                        k=1, unvarying_params=c(unvarying_params,
                                                all.settings.tb.optim))
if (new_error <= 1.1*tb_optim_res$value){
  print('Similar error; NOT re-calibrating for sensitivity analysis 3: d1w=0 and c1w=0')
  all_params_sens3 <- all_params
  all_params_sens3[['d1w']] <- 0
  all_params_sens3[['c1w']] <- 0
  tb_optim_res_sens3 <- list()
  tb_optim_res_sens3[['par']] <- tb_optim_res$par
  tb_optim_res_sens3[['value']] <- new_error
} else {
  # last_plotted_error <- Inf
  # smallest_error <- Inf
  print('Sensitivity analysis 3: d1w=0 and c1w=0')
  params_error_thresh <- NULL
  error_reached <- NULL
  tb_optim_res_sens3 <- NULL
  tryCatch({tb_optim_res_sens3 <- optim(tb_params_start, calc_diffs, method='L-BFGS-B', 
                                        lower=tb_params_lower_bounds, upper=tb_params_upper_bounds,
                                        control=list(trace=T, parscale=abs(tb_params_mean)*tb_parscale_factor, maxit=3,
                                                     pgtol=0.0001, factr=1),
                                        iR_func=incarc_functions$iR_func, iE_func=incarc_functions$iE_func, r_func=incarc_functions$r_func,
                                        param_names=tb_param_names, 
                                        upper_betap_thresh=upper_betap_thresh,
                                        d_thresh=d_thresh,
                                        times_to_use=times, quasi_2000=quasi_2000,
                                        country_p=country_p, 
                                        who_incid_factor=who_incid_factor,
                                        prison_incid=prison_incid, prison_incid_yr=prison_incid_yr,
                                        # c2.perc.change=c2.perc.change, # Mexico-only
                                        # v1.ratio=v1.ratio, # Mexico-only
                                        k=1, unvarying_params=c(unvarying_params,
                                                                all.settings.tb.optim),
                              error_thresh=tb_optim_res$value*1.1) # can stop the calibration as soon as it is within range of calibration error from main analysis
  }, error = function(e) {
  # Print the error message
  print(paste("Stopping calibration after reaching error threshold with e =", error_reached))
  tb_optim_res_sens3 <<- list(par=params_error_thresh,
                              value=error_reached)
  })

  
  # save best-fitting parameters
  names(tb_optim_res_sens3$par) <- tb_param_names
  all_params_sens3 <- c(beta_pp=tb_optim_res_sens3$par[['beta_pp']], 
                        beta_cc=tb_optim_res_sens3$par[['beta_pp']]*tb_optim_res_sens3$par[['beta_cc:beta_pp']], 
                        c1=tb_optim_res_sens3$par[['c1']],
                        c2=tb_optim_res_sens3$par[['c1']]*tb_optim_res_sens3$par[['c4:c1']],
                        c3=tb_optim_res_sens3$par[['c1']]*tb_optim_res_sens3$par[['c4:c1']],
                        c4=tb_optim_res_sens3$par[['c1']]*tb_optim_res_sens3$par[['c4:c1']],
                        unvarying_params, incarc_optim_res$par)
  # all_params_sens3[['c2']] <- (all_params_sens3[['c1']]*unvarying_params[['c1w']]) + (all_params_sens3[['c4']]*(1-unvarying_params[['c1w']]))
  if ('d' %in% tb_param_names){
    all_params_sens3[['d1']]=tb_optim_res_sens3$par[['d']]
    all_params_sens3[['d2']]=tb_optim_res_sens3$par[['d']]
    all_params_sens3[['d3']]=tb_optim_res_sens3$par[['d']]
    all_params_sens3[['d4']]=tb_optim_res_sens3$par[['d']]
  }
  if ('v' %in% tb_param_names){
    all_params_sens3[['v']] <- tb_optim_res_sens3$par[['v']]
  }
  if ('j' %in% tb_param_names){
    all_params_sens3[['j']] <- tb_optim_res_sens3$par[['j']]
  }
  if ('p' %in% tb_param_names){
    all_params_sens3[['p']] <- tb_optim_res_sens3$par[['p']]
  }
  if ('z' %in% tb_param_names){
    all_params_sens3[['z']] <- tb_optim_res_sens3$par[['z']]
  }
  if ('d1:d4' %in% tb_param_names){
    all_params_sens3[['d1']]=tb_optim_res_sens3$par[['d']]*tb_optim_res_sens3$par[['d1:d4']]
    # all_params_sens3[['d2']] <- (all_params_sens3[['d1']]*unvarying_params[['d1w']]) + (all_params_sens3[['d4']]*(1-unvarying_params[['d1w']]))
  }
  if ('v1ratio' %in% tb_param_names | !is.na(v1.ratio)){
    total.change.d4 <- all_params_sens3[['v']]*(all.settings$change.d.end1-all.settings$change.d.start1) # calculate total change in diagnosis rate outside prison
    if (all.settings$change.d.start2 != Inf & all.settings$change.d.end2 != Inf){
      total.change.d4 <- total.change.d4 + all_params_sens3[['v']]*all.settings$change.d.2.factor*(all.settings$change.d.end2-all.settings$change.d.start2)
    }
    max_possible_v1 <- total.change.d4 / (all.settings$change.d1.end1 - all.settings$change.d1.start1)
    if ('v1ratio' %in% tb_param_names){
      all_params_sens3[['v1']] <- max_possible_v1*tb_optim_res_sens3$par[['v1ratio']]
    } else if (!is.na(v1.ratio)) {
      all_params_sens3[['v1']] <- max_possible_v1*v1.ratio
    }
  }
  if (country_p == 'Mexico'){ # for Mexico, change rates in beta in periods 2 and 3 are calibrated
    # all_params_sens3[['j']] <- all_params_sens3[['c1']]*all_params_sens3[['c4:c1']]*c2.perc.change # for Mexico, j is not calibrated, it is sampled from a distribution as yearly percent change from baseline
    if ('z2:z' %in% tb_param_names){
      all_params_sens3[['z2:z']] <- tb_optim_res_sens3$par[['z2:z']]
      all.settings[['change.beta.2.factor']] <- tb_optim_res_sens3$par[['z2:z']]
    }
    if ('z3:z' %in% tb_param_names){
      all_params_sens3[['z3:z']] <- tb_optim_res_sens3$par[['z3:z']]
      all.settings[['change.beta.3.factor']] <- tb_optim_res_sens3$par[['z3:z']]
    }
  } 
  if ('covid.d.factor' %in% tb_param_names){
    all_params_sens3[['covid.d.factor']]=tb_optim_res_sens3$par[['covid.d.factor']]
    # covid.change.d.factor=tb_optim_res_sens3$par[['covid.d.factor']]
  }
}

#---------------------------------------------------#
#   Now run w calibrated params to get outputs 
#---------------------------------------------------#
######## Mass incarc & leveling off ######## 
output_stable_sens3 <- ode(
  func=tb_model_for_calibration,
  y=xstart,
  times=timeunit,
  parms=c(all_params_sens3, all.settings),
  iR_func=incarc_functions$iR_func,
  iE_func=incarc_functions$iE_func,
  r_func=incarc_functions$r_func,
  quasi_2000=2000
)

output_dt_stable_sens3 <- data.table(output_stable_sens3)
output_dt_stable_sens3[,`:=`(N1=S1+E1+L1+I1+R1,
                             N2=S2+E2+L2+I2+R2,
                             N3=S3+E3+L3+I3+R3,
                             N4=S4+E4+L4+I4+R4)]

rates_stable_sens3 <- get_annualized_rates(output_dt_stable_sens3, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)

prison_TB_incid_obs <- rates_stable_sens3[pop == 'Prison' & variable == 'I' & time %in% (prison_incid_yr), rate_per100k]

prison_TB_notif_obs <- rates_stable_sens3[pop == 'Prison' & variable == 'D' & 
                                            time %in% (prison_TB_notif_yrs), 
                                          rate_per100k]

comb_TB_incid_obs <- rates_stable_sens3[pop == 'Combined' & variable == 'I' & 
                                          time %in% comb_TB_incid_yrs, rate_per100k]
comb_TB_notif_obs <- rates_stable_sens3[pop == 'Combined' & variable == 'D' &
                                          time %in% comb_TB_notif_yrs, rate_per100k]

# prison incidence
tb_optim_error1 <- mean(abs((prison_TB_incid_obs - prison_incid)/prison_incid))

# combined incidence
tb_optim_error2 <- mean(abs((comb_TB_incid_obs - comm_TB_incid_exp)/
                              comm_TB_incid_exp))

# prison notifications
tb_optim_error3 <- mean(abs((prison_TB_notif_obs - prison_estimates[Country == country_p & 
                                                                      Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)/
                              (prison_estimates[Country == country_p & 
                                                  Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)))

# combined notifications
tb_optim_error4 <- mean(abs((comb_TB_notif_obs - who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k])/
                              who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k]))

rates_stable_sens3 <- rbind(rates_stable_sens3, data.table(pop='Prison', variable='pop',
                                                           rate_per100k=output_dt_stable_sens3[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                                           total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                            data.table(pop='Post-Release', variable='pop',
                                       rate_per100k=output_dt_stable_sens3[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                       total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                            data.table(pop='Formerly Incarc', variable='pop',
                                       rate_per100k=output_dt_stable_sens3[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                       total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)
print('sens 3: stable...done')
rates_stable_sens3$scenario <- 'Leveled Off'

##### save all parameters & calibration targets #####
optim_res[['sens3_params']] <- c(tb_optim_res_sens3$par,
                                 tb_optim_error=tb_optim_res_sens3$value,
                                 tb_optim_e1=tb_optim_error1,
                                 tb_optim_e2=tb_optim_error2,
                                 tb_optim_e3=tb_optim_error3,
                                 tb_optim_e4=tb_optim_error4)
saveRDS(optim_res, paste0(wd, 'results/', gsub(' ', '', tolower(country_p)), '_', today, '/', 
                          gsub(' ', '', tolower(country_p)), '_', today, '_params_', i, '.RDS'))


######## Counterfactual ######## 
output_counterfactual_sens3 <- ode(
  func=tb_model_for_calibration,
  y=xstart,
  times=timeunit,
  parms=c(all_params_sens3, all.settings.cf),
  iR_func=get_iR_cf,
  iE_func=get_iE_cf,
  r_func=get_r_cf,
  quasi_2000=2000
)

output_dt_counterfactual_sens3 <- data.table(output_counterfactual_sens3)
output_dt_counterfactual_sens3[,`:=`(N1=S1+E1+L1+I1+R1,
                                     N2=S2+E2+L2+I2+R2,
                                     N3=S3+E3+L3+I3+R3,
                                     N4=S4+E4+L4+I4+R4)]

rates_counterfactual_sens3 <- get_annualized_rates(output_dt_counterfactual_sens3, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)
rates_counterfactual_sens3 <- rbind(rates_counterfactual_sens3, data.table(pop='Prison', variable='pop',
                                                                           rate_per100k=output_dt_counterfactual_sens3[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                                                           total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                                    data.table(pop='Post-Release', variable='pop',
                                               rate_per100k=output_dt_counterfactual_sens3[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                               total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                                    data.table(pop='Formerly Incarc', variable='pop',
                                               rate_per100k=output_dt_counterfactual_sens3[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                               total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)

print('sens 3: counterfactual...done')
rates_counterfactual_sens3$scenario <- 'No Growth Since 1990'

rates_all_sens3 <- rbind(rates_stable_sens3, rates_counterfactual_sens3)
rates_all_sens3$idx <- i
rates_all_sens3$analysis <- 'c1w=d1w=0'

##### 4. Prop_E = 1/3  #####
unvarying_params[['prop_E']] <- 1/3 
unvarying_params[['d1w']] <- all_samps[i,]$d1w # change this back
unvarying_params[['c1w']] <- all_samps[i,]$c1w # change this back

new_error <- calc_diffs(tb_params_start,
                        iR_func=incarc_functions$iR_func, iE_func=incarc_functions$iE_func, r_func=incarc_functions$r_func, 
                        param_names=tb_param_names, 
                        upper_betap_thresh=upper_betap_thresh,
                        d_thresh=d_thresh,
                        times_to_use=times, quasi_2000=quasi_2000,
                        country_p=country_p, 
                        who_incid_factor=who_incid_factor,
                        prison_incid=prison_incid, prison_incid_yr=prison_incid_yr,
                        # c2.perc.change=c2.perc.change, # Mexico-only
                        # v1.ratio=v1.ratio, # Mexico-only
                        k=1, unvarying_params=c(unvarying_params, 
                                                all.settings.tb.optim))
if (new_error <= 1.1*tb_optim_res$value){
  print('Similar error; NOT re-calibrating for sensitivity analysis 4: prop_E=1/3')
  all_params_sens4 <- all_params
  all_params_sens4[['prop_E']] <- 1/3
  tb_optim_res_sens4 <- list()
  tb_optim_res_sens4[['par']] <- tb_optim_res$par
  tb_optim_res_sens4[['value']] <- new_error
} else {
  # last_plotted_error <- Inf
  # smallest_error <- Inf
  print('Sensitivity analysis 4: prop_E=1/4')
  params_error_thresh <- NULL
  error_reached <- NULL
  tb_optim_res_sens4 <- NULL
  tryCatch({tb_optim_res_sens4 <- optim(tb_params_start, calc_diffs, method='L-BFGS-B', 
                                        lower=tb_params_lower_bounds, upper=tb_params_upper_bounds,
                                        control=list(trace=T, parscale=abs(tb_params_mean)*tb_parscale_factor, maxit=3,
                                                     pgtol=0.0001, factr=1),
                                        iR_func=incarc_functions$iR_func, iE_func=incarc_functions$iE_func, r_func=incarc_functions$r_func,
                                        param_names=tb_param_names, 
                                        upper_betap_thresh=upper_betap_thresh,
                                        d_thresh=d_thresh,
                                        times_to_use=times, quasi_2000=quasi_2000,
                                        country_p=country_p, 
                                        who_incid_factor=who_incid_factor,
                                        prison_incid=prison_incid, prison_incid_yr=prison_incid_yr,
                                        # c2.perc.change=c2.perc.change, # Mexico-only
                                        # v1.ratio=v1.ratio, # Mexico-only
                                        k=1, unvarying_params=c(unvarying_params,
                                                                all.settings.tb.optim),
                                        error_thresh=tb_optim_res$value*1.1) # can stop the calibration as soon as it is within range of calibration error from main analysis
  }, error = function(e) {
    # Print the error message
    print(paste("Stopping calibration after reaching error threshold with e =", error_reached))
    tb_optim_res_sens4 <<- list(par=params_error_thresh,
                                value=error_reached)
  })
  
  # save best-fitting parameters
  names(tb_optim_res_sens4$par) <- tb_param_names
  all_params_sens4 <- c(beta_pp=tb_optim_res_sens4$par[['beta_pp']], 
                        beta_cc=tb_optim_res_sens4$par[['beta_pp']]*tb_optim_res_sens4$par[['beta_cc:beta_pp']], 
                        c1=tb_optim_res_sens4$par[['c1']],
                        c2=tb_optim_res_sens4$par[['c1']]*tb_optim_res_sens4$par[['c4:c1']],
                        c3=tb_optim_res_sens4$par[['c1']]*tb_optim_res_sens4$par[['c4:c1']],
                        c4=tb_optim_res_sens4$par[['c1']]*tb_optim_res_sens4$par[['c4:c1']],
                        unvarying_params, incarc_optim_res$par)
  # all_params_sens4[['c2']] <- (all_params_sens4[['c1']]*unvarying_params[['c1w']]) + (all_params_sens4[['c4']]*(1-unvarying_params[['c1w']]))
  if ('d' %in% tb_param_names){
    all_params_sens4[['d1']]=tb_optim_res_sens4$par[['d']]
    all_params_sens4[['d2']]=tb_optim_res_sens4$par[['d']]
    all_params_sens4[['d3']]=tb_optim_res_sens4$par[['d']]
    all_params_sens4[['d4']]=tb_optim_res_sens4$par[['d']]
  }
  if ('v' %in% tb_param_names){
    all_params_sens4[['v']] <- tb_optim_res_sens4$par[['v']]
  }
  if ('j' %in% tb_param_names){
    all_params_sens4[['j']] <- tb_optim_res_sens4$par[['j']]
  }
  if ('p' %in% tb_param_names){
    all_params_sens4[['p']] <- tb_optim_res_sens4$par[['p']]
  }
  if ('z' %in% tb_param_names){
    all_params_sens4[['z']] <- tb_optim_res_sens4$par[['z']]
  }
  if ('d1:d4' %in% tb_param_names){
    all_params_sens4[['d1']]=tb_optim_res_sens4$par[['d']]*tb_optim_res_sens4$par[['d1:d4']]
    # all_params_sens4[['d2']] <- (all_params_sens4[['d1']]*unvarying_params[['d1w']]) + (all_params_sens4[['d4']]*(1-unvarying_params[['d1w']]))
  }
  if ('v1ratio' %in% tb_param_names | !is.na(v1.ratio)){
    total.change.d4 <- all_params_sens4[['v']]*(all.settings$change.d.end1-all.settings$change.d.start1) # calculate total change in diagnosis rate outside prison
    if (all.settings$change.d.start2 != Inf & all.settings$change.d.end2 != Inf){
      total.change.d4 <- total.change.d4 + all_params_sens4[['v']]*all.settings$change.d.2.factor*(all.settings$change.d.end2-all.settings$change.d.start2)
    }
    max_possible_v1 <- total.change.d4 / (all.settings$change.d1.end1 - all.settings$change.d1.start1)
    if ('v1ratio' %in% tb_param_names){
      all_params_sens4[['v1']] <- max_possible_v1*tb_optim_res_sens4$par[['v1ratio']]
    } else if (!is.na(v1.ratio)) {
      all_params_sens4[['v1']] <- max_possible_v1*v1.ratio
    }
  }
  if (country_p == 'Mexico'){ # for Mexico, change rates in beta in periods 2 and 3 are calibrated
    # all_params_sens4[['j']] <- all_params_sens4[['c1']]*all_params_sens4[['c4:c1']]*c2.perc.change # for Mexico, j is not calibrated, it is sampled from a distribution as yearly percent change from baseline
    if ('z2:z' %in% tb_param_names){
      all_params_sens4[['z2:z']] <- tb_optim_res_sens4$par[['z2:z']]
      all.settings[['change.beta.2.factor']] <- tb_optim_res_sens4$par[['z2:z']]
    }
    if ('z3:z' %in% tb_param_names){
      all_params_sens4[['z3:z']] <- tb_optim_res_sens4$par[['z3:z']]
      all.settings[['change.beta.3.factor']] <- tb_optim_res_sens4$par[['z3:z']]
    }
  } 
  if ('covid.d.factor' %in% tb_param_names){
    all_params_sens4[['covid.d.factor']]=tb_optim_res_sens4$par[['covid.d.factor']]
    # covid.change.d.factor=tb_optim_res_sens4$par[['covid.d.factor']]
  }
}

#---------------------------------------------------#
#   Now run w calibrated params to get outputs 
#---------------------------------------------------#
######## Mass incarc & leveling off ######## 
output_stable_sens4 <- ode(
  func=tb_model_for_calibration,
  y=xstart,
  times=timeunit,
  parms=c(all_params_sens4, all.settings),
  iR_func=incarc_functions$iR_func,
  iE_func=incarc_functions$iE_func,
  r_func=incarc_functions$r_func,
  quasi_2000=2000
)

output_dt_stable_sens4 <- data.table(output_stable_sens4)
output_dt_stable_sens4[,`:=`(N1=S1+E1+L1+I1+R1,
                             N2=S2+E2+L2+I2+R2,
                             N3=S3+E3+L3+I3+R3,
                             N4=S4+E4+L4+I4+R4)]

rates_stable_sens4 <- get_annualized_rates(output_dt_stable_sens4, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)

prison_TB_incid_obs <- rates_stable_sens4[pop == 'Prison' & variable == 'I' & time %in% (prison_incid_yr), rate_per100k]

prison_TB_notif_obs <- rates_stable_sens4[pop == 'Prison' & variable == 'D' & 
                                            time %in% (prison_TB_notif_yrs), 
                                          rate_per100k]

comb_TB_incid_obs <- rates_stable_sens4[pop == 'Combined' & variable == 'I' & 
                                          time %in% comb_TB_incid_yrs, rate_per100k]
comb_TB_notif_obs <- rates_stable_sens4[pop == 'Combined' & variable == 'D' &
                                          time %in% comb_TB_notif_yrs, rate_per100k]

# prison incidence
tb_optim_error1 <- mean(abs((prison_TB_incid_obs - prison_incid)/prison_incid))

# combined incidence
tb_optim_error2 <- mean(abs((comb_TB_incid_obs - comm_TB_incid_exp)/
                              comm_TB_incid_exp))

# prison notifications
tb_optim_error3 <- mean(abs((prison_TB_notif_obs - prison_estimates[Country == country_p & 
                                                                      Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)/
                              (prison_estimates[Country == country_p & 
                                                  Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)))

# combined notifications
tb_optim_error4 <- mean(abs((comb_TB_notif_obs - who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k])/
                              who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k]))

rates_stable_sens4 <- rbind(rates_stable_sens4, data.table(pop='Prison', variable='pop',
                                                           rate_per100k=output_dt_stable_sens4[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                                           total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                            data.table(pop='Post-Release', variable='pop',
                                       rate_per100k=output_dt_stable_sens4[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                       total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                            data.table(pop='Formerly Incarc', variable='pop',
                                       rate_per100k=output_dt_stable_sens4[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                       total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)
print('sens 4: stable...done')
rates_stable_sens4$scenario <- 'Leveled Off'

##### save all parameters & calibration targets #####
optim_res[['sens4_params']] <- c(tb_optim_res_sens4$par,
                                 tb_optim_error=tb_optim_res_sens4$value,
                                 tb_optim_e1=tb_optim_error1,
                                 tb_optim_e2=tb_optim_error2,
                                 tb_optim_e3=tb_optim_error3,
                                 tb_optim_e4=tb_optim_error4)
saveRDS(optim_res, paste0(wd, 'results/', gsub(' ', '', tolower(country_p)), '_', today, '/', 
                          gsub(' ', '', tolower(country_p)), '_', today, '_params_', i, '.RDS'))


######## Counterfactual ######## 
output_counterfactual_sens4 <- ode(
  func=tb_model_for_calibration,
  y=xstart,
  times=timeunit,
  parms=c(all_params_sens4, all.settings.cf),
  iR_func=get_iR_cf,
  iE_func=get_iE_cf,
  r_func=get_r_cf,
  quasi_2000=2000
)

output_dt_counterfactual_sens4 <- data.table(output_counterfactual_sens4)
output_dt_counterfactual_sens4[,`:=`(N1=S1+E1+L1+I1+R1,
                                     N2=S2+E2+L2+I2+R2,
                                     N3=S3+E3+L3+I3+R3,
                                     N4=S4+E4+L4+I4+R4)]

rates_counterfactual_sens4 <- get_annualized_rates(output_dt_counterfactual_sens4, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)
rates_counterfactual_sens4 <- rbind(rates_counterfactual_sens4, data.table(pop='Prison', variable='pop',
                                                                           rate_per100k=output_dt_counterfactual_sens4[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                                                           total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                                    data.table(pop='Post-Release', variable='pop',
                                               rate_per100k=output_dt_counterfactual_sens4[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                               total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                                    data.table(pop='Formerly Incarc', variable='pop',
                                               rate_per100k=output_dt_counterfactual_sens4[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                               total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)

print('sens 4: counterfactual...done')
rates_counterfactual_sens4$scenario <- 'No Growth Since 1990'

rates_all_sens4 <- rbind(rates_stable_sens4, rates_counterfactual_sens4)
rates_all_sens4$idx <- i
rates_all_sens4$analysis <- 'prop_E=1/3'

##### 5. beta_pc = 0  #####
unvarying_params[['prop_E']] <- prop_E # change this back
unvarying_params[['beta_pc']] <- 0

new_error <- calc_diffs(tb_params_start,
                        iR_func=incarc_functions$iR_func, iE_func=incarc_functions$iE_func, r_func=incarc_functions$r_func, 
                        param_names=tb_param_names, 
                        upper_betap_thresh=upper_betap_thresh,
                        d_thresh=d_thresh,
                        times_to_use=times, quasi_2000=quasi_2000,
                        country_p=country_p, 
                        who_incid_factor=who_incid_factor,
                        prison_incid=prison_incid, prison_incid_yr=prison_incid_yr,
                        # c2.perc.change=c2.perc.change, # Mexico-only
                        # v1.ratio=v1.ratio, # Mexico-only
                        k=1, unvarying_params=c(unvarying_params,
                                                all.settings.tb.optim))
if (new_error <= 1.1*tb_optim_res$value){
  print('Similar error; NOT re-calibrating for sensitivity analysis 5: beta_pc=0')
  all_params_sens5 <- all_params
  all_params_sens5[['beta_pc']] <- 0
  tb_optim_res_sens5 <- list()
  tb_optim_res_sens5[['par']] <- tb_optim_res$par
  tb_optim_res_sens5[['value']] <- new_error
} else {
  # last_plotted_error <- Inf
  # smallest_error <- Inf
  print('Sensitivity analysis 5: beta_pc=0')
  params_error_thresh <- NULL
  error_reached <- NULL
  tb_optim_res_sens5 <- NULL
  tryCatch({tb_optim_res_sens5 <- optim(tb_params_start, calc_diffs, method='L-BFGS-B', 
                                        lower=tb_params_lower_bounds, upper=tb_params_upper_bounds,
                                        control=list(trace=T, parscale=abs(tb_params_mean)*tb_parscale_factor, maxit=3,
                                                     pgtol=0.0001, factr=1),
                                        iR_func=incarc_functions$iR_func, iE_func=incarc_functions$iE_func, r_func=incarc_functions$r_func,
                                        param_names=tb_param_names, 
                                        d_thresh=d_thresh,
                                        times_to_use=times, quasi_2000=quasi_2000,
                                        country_p=country_p, 
                                        who_incid_factor=who_incid_factor,
                                        # muTB_factor=muTB_factor,
                                        prison_incid=prison_incid, prison_incid_yr=prison_incid_yr,
                                        # c2.perc.change=c2.perc.change, # Mexico-only
                                        # v1.ratio=v1.ratio, # Mexico-only
                                        k=1, unvarying_params=c(unvarying_params,
                                                                all.settings.tb.optim),
                                        error_thresh=tb_optim_res$value*1.1) # can stop the calibration as soon as it is within range of calibration error from main analysis
  }, error = function(e) {
    # Print the error message
    print(paste("Stopping calibration after reaching error threshold with e =", error_reached))
    tb_optim_res_sens5 <<- list(par=params_error_thresh,
                                value=error_reached)
  })
  
  
  # save best-fitting parameters
  names(tb_optim_res_sens5$par) <- tb_param_names
  all_params_sens5 <- c(beta_pp=tb_optim_res_sens5$par[['beta_pp']], 
                        beta_cc=tb_optim_res_sens5$par[['beta_pp']]*tb_optim_res_sens5$par[['beta_cc:beta_pp']], 
                        c1=tb_optim_res_sens5$par[['c1']],
                        c2=tb_optim_res_sens5$par[['c1']]*tb_optim_res_sens5$par[['c4:c1']],
                        c3=tb_optim_res_sens5$par[['c1']]*tb_optim_res_sens5$par[['c4:c1']],
                        c4=tb_optim_res_sens5$par[['c1']]*tb_optim_res_sens5$par[['c4:c1']],
                        unvarying_params, incarc_optim_res$par)
  # all_params_sens5[['c2']] <- (all_params_sens5[['c1']]*unvarying_params[['c1w']]) + (all_params_sens5[['c4']]*(1-unvarying_params[['c1w']]))
  if ('d' %in% tb_param_names){
    all_params_sens5[['d1']]=tb_optim_res_sens5$par[['d']]
    all_params_sens5[['d2']]=tb_optim_res_sens5$par[['d']]
    all_params_sens5[['d3']]=tb_optim_res_sens5$par[['d']]
    all_params_sens5[['d4']]=tb_optim_res_sens5$par[['d']]
  }
  if ('v' %in% tb_param_names){
    all_params_sens5[['v']] <- tb_optim_res_sens5$par[['v']]
  }
  if ('j' %in% tb_param_names){
    all_params_sens5[['j']] <- tb_optim_res_sens5$par[['j']]
  }
  if ('p' %in% tb_param_names){
    all_params_sens5[['p']] <- tb_optim_res_sens5$par[['p']]
  }
  if ('z' %in% tb_param_names){
    all_params_sens5[['z']] <- tb_optim_res_sens5$par[['z']]
  }
  if ('d1:d4' %in% tb_param_names){
    all_params_sens5[['d1']]=tb_optim_res_sens5$par[['d']]*tb_optim_res_sens5$par[['d1:d4']]
    # all_params_sens5[['d2']] <- (all_params_sens5[['d1']]*unvarying_params[['d1w']]) + (all_params_sens5[['d4']]*(1-unvarying_params[['d1w']]))
  }
  if ('v1ratio' %in% tb_param_names | !is.na(v1.ratio)){
    total.change.d4 <- all_params_sens5[['v']]*(all.settings$change.d.end1-all.settings$change.d.start1) # calculate total change in diagnosis rate outside prison
    if (all.settings$change.d.start2 != Inf & all.settings$change.d.end2 != Inf){
      total.change.d4 <- total.change.d4 + all_params_sens5[['v']]*all.settings$change.d.2.factor*(all.settings$change.d.end2-all.settings$change.d.start2)
    }
    max_possible_v1 <- total.change.d4 / (all.settings$change.d1.end1 - all.settings$change.d1.start1)
    if ('v1ratio' %in% tb_param_names){
      all_params_sens5[['v1']] <- max_possible_v1*tb_optim_res_sens5$par[['v1ratio']]
    } else if (!is.na(v1.ratio)) {
      all_params_sens5[['v1']] <- max_possible_v1*v1.ratio
    }
  }
  
  if (country_p == 'Mexico'){ # for Mexico, change rates in beta in periods 2 and 3 are calibrated
    # all_params_sens5[['j']] <- all_params_sens5[['c1']]*all_params_sens5[['c4:c1']]*c2.perc.change # for Mexico, j is not calibrated, it is sampled from a distribution as yearly percent change from baseline
    if ('z2:z' %in% tb_param_names){
      all_params_sens5[['z2:z']] <- tb_optim_res_sens5$par[['z2:z']]
      all.settings[['change.beta.2.factor']] <- tb_optim_res_sens5$par[['z2:z']]
    }
    if ('z3:z' %in% tb_param_names){
      all_params_sens5[['z3:z']] <- tb_optim_res_sens5$par[['z3:z']]
      all.settings[['change.beta.3.factor']] <- tb_optim_res_sens5$par[['z3:z']]
    }
  } 
  if ('covid.d.factor' %in% tb_param_names){
    all_params_sens5[['covid.d.factor']]=tb_optim_res_sens5$par[['covid.d.factor']]
    # covid.change.d.factor=tb_optim_res_sens5$par[['covid.d.factor']]
  }
}

#---------------------------------------------------#
#   Now run w calibrated params to get outputs 
#---------------------------------------------------#
######## Mass incarc & leveling off ######## 
output_stable_sens5 <- ode(
  func=tb_model_for_calibration,
  y=xstart,
  times=timeunit,
  parms=c(all_params_sens5, all.settings),
  iR_func=incarc_functions$iR_func,
  iE_func=incarc_functions$iE_func,
  r_func=incarc_functions$r_func,
  quasi_2000=2000
)

output_dt_stable_sens5 <- data.table(output_stable_sens5)
output_dt_stable_sens5[,`:=`(N1=S1+E1+L1+I1+R1,
                             N2=S2+E2+L2+I2+R2,
                             N3=S3+E3+L3+I3+R3,
                             N4=S4+E4+L4+I4+R4)]

rates_stable_sens5 <- get_annualized_rates(output_dt_stable_sens5, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)

prison_TB_incid_obs <- rates_stable_sens5[pop == 'Prison' & variable == 'I' & time %in% (prison_incid_yr), rate_per100k]

prison_TB_notif_obs <- rates_stable_sens5[pop == 'Prison' & variable == 'D' & 
                                            time %in% (prison_TB_notif_yrs), 
                                          rate_per100k]

comb_TB_incid_obs <- rates_stable_sens5[pop == 'Combined' & variable == 'I' & 
                                          time %in% comb_TB_incid_yrs, rate_per100k]
comb_TB_notif_obs <- rates_stable_sens5[pop == 'Combined' & variable == 'D' &
                                          time %in% comb_TB_notif_yrs, rate_per100k]

# prison incidence
tb_optim_error1 <- mean(abs((prison_TB_incid_obs - prison_incid)/prison_incid))

# combined incidence
tb_optim_error2 <- mean(abs((comb_TB_incid_obs - comm_TB_incid_exp)/
                              comm_TB_incid_exp))

# prison notifications
tb_optim_error3 <- mean(abs((prison_TB_notif_obs - prison_estimates[Country == country_p & 
                                                                      Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)/
                              (prison_estimates[Country == country_p & 
                                                  Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)))

# combined notifications
tb_optim_error4 <- mean(abs((comb_TB_notif_obs - who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k])/
                              who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k]))

rates_stable_sens5 <- rbind(rates_stable_sens5, data.table(pop='Prison', variable='pop',
                                                           rate_per100k=output_dt_stable_sens5[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                                           total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                            data.table(pop='Post-Release', variable='pop',
                                       rate_per100k=output_dt_stable_sens5[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                       total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                            data.table(pop='Formerly Incarc', variable='pop',
                                       rate_per100k=output_dt_stable_sens5[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                       total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)
print('sens 5: stable...done')
rates_stable_sens5$scenario <- 'Leveled Off'

##### save all parameters & calibration targets #####
optim_res[['sens5_params']] <- c(tb_optim_res_sens5$par,
                                 tb_optim_error=tb_optim_res_sens5$value,
                                 tb_optim_e1=tb_optim_error1,
                                 tb_optim_e2=tb_optim_error2,
                                 tb_optim_e3=tb_optim_error3,
                                 tb_optim_e4=tb_optim_error4)
saveRDS(optim_res, paste0(wd, 'results/', gsub(' ', '', tolower(country_p)), '_', today, '/', 
                          gsub(' ', '', tolower(country_p)), '_', today, '_params_', i, '.RDS'))


######## Counterfactual ######## 
output_counterfactual_sens5 <- ode(
  func=tb_model_for_calibration,
  y=xstart,
  times=timeunit,
  parms=c(all_params_sens5, all.settings.cf),
  iR_func=get_iR_cf,
  iE_func=get_iE_cf,
  r_func=get_r_cf,
  quasi_2000=2000
)

output_dt_counterfactual_sens5 <- data.table(output_counterfactual_sens5)
output_dt_counterfactual_sens5[,`:=`(N1=S1+E1+L1+I1+R1,
                                     N2=S2+E2+L2+I2+R2,
                                     N3=S3+E3+L3+I3+R3,
                                     N4=S4+E4+L4+I4+R4)]

rates_counterfactual_sens5 <- get_annualized_rates(output_dt_counterfactual_sens5, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)
rates_counterfactual_sens5 <- rbind(rates_counterfactual_sens5, data.table(pop='Prison', variable='pop',
                                                                           rate_per100k=output_dt_counterfactual_sens5[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)]*100000,
                                                                           total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                                    data.table(pop='Post-Release', variable='pop',
                                               rate_per100k=output_dt_counterfactual_sens5[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)]*100000,
                                               total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                                    data.table(pop='Formerly Incarc', variable='pop',
                                               rate_per100k=output_dt_counterfactual_sens5[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)]*100000,
                                               total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)

print('sens 5: counterfactual...done')
rates_counterfactual_sens5$scenario <- 'No Growth Since 1990'

rates_all_sens5 <- rbind(rates_stable_sens5, rates_counterfactual_sens5)
rates_all_sens5$idx <- i
rates_all_sens5$analysis <- 'beta_pc=0'

##### KNIT EVERYTHING TOGETHER ######
rates_all$analysis <- 'main analysis'
if (all.settings[['change.betap.start1']] != Inf & country_p != 'El Salvador'){ # if sensitivity analysis #1 was done
  rates_all_sens1$analysis <- 'no change in beta_pp'
  rates_all_combined <- rbind(rates_all,
                              rates_all_sens1,
                              rates_all_sens2,
                              rates_all_sens3,
                              rates_all_sens4,
                              rates_all_sens5)
} else {
  rates_all_combined <- rbind(rates_all,
                              rates_all_sens2,
                              rates_all_sens3,
                              rates_all_sens4,
                              rates_all_sens5)
}

write.csv(rates_all_combined, paste0(wd, 'results/model_outputs_', gsub(' ', '', tolower(country_p)), '_', today, '/', 
                                  gsub(' ', '', tolower(country_p)), '_', today, '_all_outputs_', i, '.csv'), row.names=F)

