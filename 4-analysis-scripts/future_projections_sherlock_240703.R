# Run future projections for all countries except El Salvador
# as of 7/13/24, for decarceration scenarios we are modeling
# 25% or 50% decreases in entry rates and/or duration
# previously we were doing _% decrease in entry rate and _% increase in release rate 
# duration = 1/releaserate

library(data.table)
library(deSolve)

args <- commandArgs(trailingOnly = T)
i_start <- as.integer(args[1]) # retrieve from bash script call
i_end <- as.integer(args[2]) # retrieve from bash script call

today <- as.character(args[3])
country_p <- as.character(args[4])

wd <- '~/TBprisons/' # Sherlock
# print(paste0(wd, 'results/', gsub(" ", "", tolower(country_p)), '_', today, '/'))

source(paste0(wd, 'scripts/tb_incarc_models_240528_newformulation.R')) # script with model functions
source(paste0(wd, 'scripts/', gsub(" ", "", tolower(country_p)), '_main_settings_', today, '.R'))

good_idx <- readRDS(paste0(wd, 'scripts/good_idx_', gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))

set.seed(2024) # just run for 1000 sets of fitted params
good_idx_use <- sample(good_idx, 1000)

horiz.start=2013
horiz.end=2036
time.increment=1
timeunit <- c(0,seq(horiz.start-5,horiz.end+1, by=time.increment))
xstart <- c(S1=0.005*0.95*100000, E1=0, L1=0, I1=0.005*0.05*100000, R1=0,
            S2=0.005*0.95*100000, E2=0, L2=0, I2=0.005*0.05*100000, R2=0,
            S3=0, E3=0, L3=0, I3=0, R3=0,
            S4=0.99*0.95*100000, E4=0, L4=0, I4=0.99*0.05*100000, R4=0,
            I1shadow=0, I2shadow=0, I3shadow=0, I4shadow=0,
            D1shadow=0, D2shadow=0, D3shadow=0, D4shadow=0,
            muI1shadow=0, muI2shadow=0, muI3shadow=0, muI4shadow=0)

intrvn.start=2024
intrvn.end=2034
this.year=2023

all_params <- readRDS(paste0(wd, 'results/all_params_', gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))

total_length <- length(all_params)

all_combined <- vector('list', length=length(i_start:i_end))

for (i in i_start:i_end){
  if (i > total_length){
    print(paste0('reached end of all_params object at i=', i))
    break
  }
  current_idx <- all_params[[i]]$main_params[['idx']]
  if (!current_idx %in% good_idx_use){
    next
  }
  
  ###### continual growth ######
  # first compute how incarceration & release rates have changed in the last 10 years
  intrvn.iR.factor=all_params[[i]]$incarc_functions$iR_func(this.year) / all_params[[i]]$incarc_functions$iR_func(this.year-10)
  intrvn.iE.factor=all_params[[i]]$incarc_functions$iE_func(this.year) / all_params[[i]]$incarc_functions$iE_func(this.year-10)
  intrvn.r.factor=all_params[[i]]$incarc_functions$r_func(this.year) / all_params[[i]]$incarc_functions$r_func(this.year-10)
  
  all.settings[c('intrvn.start','intrvn.end','intrvn.iR.factor','intrvn.iE.factor','intrvn.r.factor')] <- c(intrvn.start,
                                                                                                            intrvn.end,
                                                                                                            intrvn.iR.factor,
                                                                                                            intrvn.iE.factor,
                                                                                                            intrvn.r.factor)
  incarc_functions_growth <- get_incarc_functions(incarc_params=all_params[[i]]$main_params,
                                                  unvarying_incarc_params=c(all_params[[i]]$main_params,
                                                                            all.settings))
  
  output_growth <- ode(
    func=tb_model_for_calibration,
    y=xstart,
    times=timeunit,
    parms=c(all_params[[i]]$main_params, all.settings),
    iR_func=incarc_functions_growth$iR_func,
    iE_func=incarc_functions_growth$iE_func,
    r_func=incarc_functions_growth$r_func,
    quasi_2000=2000
  )
  
  output_dt_growth <- data.table(output_growth)
  output_dt_growth[,`:=`(N1=S1+E1+L1+I1+R1,
                         N2=S2+E2+L2+I2+R2,
                         N3=S3+E3+L3+I3+R3,
                         N4=S4+E4+L4+I4+R4)]
  # plot(output_dt_growth$time, output_dt_growth$N1, xlim=c(1990,2050))
  rates_growth <- get_annualized_rates(output_dt_growth, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)
  
  rates_growth <- rbind(rates_growth, data.table(pop='Prison', variable='pop',
                                                 rate_per100k=output_dt_growth[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)], # note: when we ran this, these were just "N1" or "N2" or "N3"; we back-calculated actual prevalence (ie N1/(N1+N2+N3+N4)) in the analysis stage, and then corrected this code to reflect what should have been done
                                                 total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                        data.table(pop='Post-Release', variable='pop',
                                   rate_per100k=output_dt_growth[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)], 
                                   total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                        data.table(pop='Formerly Incarc', variable='pop',
                                   rate_per100k=output_dt_growth[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)],
                                   total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)
  rates_growth$scenario <- 'Continual Growth'
  rates_growth$perc_change_iR <- (intrvn.iR.factor-1)*100
  rates_growth$perc_change_iE <- (intrvn.iE.factor-1)*100
  rates_growth$perc_change_r <- (intrvn.r.factor-1)*100
  # print('growth...done')
  
  ##### 0, 25%, 50% decreases in entry rates and duration #####
  j_vect <- c(0,0,0,0.25,0.25,0.5,0.5)
  k_vect <- c(0,0.25,0.5,0,0.25,0,0.5)
  rates_intrvn_combined <- data.table()
  
  for (c in 1:length(j_vect)){
    j <- j_vect[c]
    k <- k_vect[c]
    intrvn.iR.factor=1-j
    intrvn.iE.factor=1-j
    intrvn.r.factor=1/(1-k) # if we want a k% decrease in duration, r changes by this much (i.e. intrvn.r.factor=2 for a 50% decrease in duration)
    
    all.settings[c('intrvn.iR.factor','intrvn.iE.factor','intrvn.r.factor')] <- c(intrvn.iR.factor,
                                                                                  intrvn.iE.factor,
                                                                                  intrvn.r.factor)
    incarc_functions_intrvn <- get_incarc_functions(incarc_params=all_params[[i]]$main_params,
                                                    unvarying_incarc_params=c(all_params[[i]]$main_params,
                                                                              all.settings))
    
    output_intrvn <- ode(
      func=tb_model_for_calibration,
      y=xstart,
      times=timeunit,
      parms=c(all_params[[i]]$main_params, all.settings),
      iR_func=incarc_functions_intrvn$iR_func,
      iE_func=incarc_functions_intrvn$iE_func,
      r_func=incarc_functions_intrvn$r_func,
      quasi_2000=2000
    )
    
    output_dt_intrvn <- data.table(output_intrvn)
    output_dt_intrvn[,`:=`(N1=S1+E1+L1+I1+R1,
                           N2=S2+E2+L2+I2+R2,
                           N3=S3+E3+L3+I3+R3,
                           N4=S4+E4+L4+I4+R4)]
    
    rates_intrvn <- get_annualized_rates(output_dt_intrvn, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)
    
    rates_intrvn <- rbind(rates_intrvn, data.table(pop='Prison', variable='pop',
                                                   rate_per100k=output_dt_intrvn[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)], # see note above
                                                   total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                          data.table(pop='Post-Release', variable='pop',
                                     rate_per100k=output_dt_intrvn[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)],
                                     total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                          data.table(pop='Formerly Incarc', variable='pop',
                                     rate_per100k=output_dt_intrvn[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)],
                                     total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)
    # print(paste('intrvn...done', j, k))
    # rates_intrvn$scenario <- paste0(j*100, '% lower entry rates; ', k*100, '% higher release rate')
    rates_intrvn$scenario <- 'Decarceration'
    rates_intrvn$perc_change_iR <- (intrvn.iR.factor-1)*100
    rates_intrvn$perc_change_iE <- (intrvn.iE.factor-1)*100
    rates_intrvn$perc_change_r <- (intrvn.r.factor-1)*100
    rates_intrvn_combined <- rbind(rates_intrvn_combined, rates_intrvn, fill=T)
  }
  rates_all <- rbindlist(list(rates_growth, rates_intrvn_combined))
  rates_all$idx <- current_idx
  all_combined[[(i-i_start+1)]] <- rates_all
  print(paste0(i, ' done!'))
}

saveRDS(all_combined, paste0(wd, 'results/future_projections_', gsub(" ", "", tolower(country_p)), '_', today, 
                             '_', i_start, '_', i_end, '.RDS'))

