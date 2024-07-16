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
# this.year=2023

all_params <- readRDS(paste0(wd, 'results/all_params_', gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))

total_length <- length(all_params)

all_combined <- vector('list', length=length(i_start:i_end))

for (i in i_start:i_end){
  if (i > total_length){
    print(paste0('reached end of all_params object at i=', i))
    break
  }
  current_idx <- all_params[[i]]$main_params[['idx']]
  if (!current_idx %in% good_idx){
    next
  }
  
  ###### continue SoE ######
  incarc_functions_cont_SoE <- all_params[[i]]$incarc_functions
  
  output_cont_SoE <- ode(
    func=tb_model_for_calibration,
    y=xstart,
    times=timeunit,
    parms=c(all_params[[i]]$main_params, all.settings),
    iR_func=incarc_functions_cont_SoE$iR_func,
    iE_func=incarc_functions_cont_SoE$iE_func,
    r_func=incarc_functions_cont_SoE$r_func,
    quasi_2000=2000
  )
  
  output_dt_cont_SoE <- data.table(output_cont_SoE)
  output_dt_cont_SoE[,`:=`(N1=S1+E1+L1+I1+R1,
                         N2=S2+E2+L2+I2+R2,
                         N3=S3+E3+L3+I3+R3,
                         N4=S4+E4+L4+I4+R4)]
  # plot(output_dt_cont_SoE$time, output_dt_cont_SoE$N1, xlim=c(1990,2035))
  rates_cont_SoE <- get_annualized_rates(output_dt_cont_SoE, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)
  
  rates_cont_SoE <- rbind(rates_cont_SoE, data.table(pop='Prison', variable='pop',
                                                 rate_per100k=output_dt_cont_SoE[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)], # note: when we ran this, these were just "N1" or "N2" or "N3"; we back-calculated actual prevalence (ie N1/(N1+N2+N3+N4)) in the analysis stage, and then corrected this code to reflect what should have been done
                                                 total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                        data.table(pop='Post-Release', variable='pop',
                                   rate_per100k=output_dt_cont_SoE[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)],
                                   total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                        data.table(pop='Formerly Incarc', variable='pop',
                                   rate_per100k=output_dt_cont_SoE[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)],
                                   total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)
  rates_cont_SoE$scenario <- 'Continue state of emergency'
  # rates_cont_SoE$perc_change_iR <- 0
  # rates_cont_SoE$perc_change_iE <- 0
  # rates_cont_SoE$perc_change_r <- 0
  # print('cont_SoE...done')
  
  ###### other scenarios ######
  scenario.vect <- c('Gradual passive abatement','Active 10-year reversion',
                     'Active 5-year reversion','Active 2-year reversion')
  intrvn.end.vect <- c(2034, 2025, 2025, 2025)
  intrvn.iE.factor.vect <- c(1, 1, 1, incarc_functions_cont_SoE$iE_func(1990)/incarc_functions_cont_SoE$iE_func(2022))
  intrvn.iR.factor.vect <- c(1, 1, 1, incarc_functions_cont_SoE$iR_func(1990)/incarc_functions_cont_SoE$iR_func(2022))
  intrvn.r.factor.vect <- c(1, 1.25, 2, 4.75)
  
  intrvn.start2.vect <- c(Inf, Inf, Inf, 2026)
  intrvn.end2.vect <- c(Inf, Inf, Inf, 2026+1e-10)
  intrvn.iE.factor2.vect <- c(1, 1, 1, intrvn.iE.factor.vect[4])
  intrvn.iR.factor2.vect <- c(1, 1, 1, intrvn.iR.factor.vect[4])
  intrvn.r.factor2.vect <- c(1, 1, 1, incarc_functions_cont_SoE$r_func(1990)/incarc_functions_cont_SoE$r_func(2022))
  
  rates_intrvn_combined <- data.table()
  
  for (s in 1:length(scenario.vect)){
    scenario_p <- scenario.vect[s]
    intrvn.end <- intrvn.end.vect[s]
    intrvn.iE.factor <- intrvn.iE.factor.vect[s]
    intrvn.iR.factor <- intrvn.iR.factor.vect[s]
    intrvn.r.factor <- intrvn.r.factor.vect[s]
    intrvn.start2 <- intrvn.start2.vect[s]
    intrvn.end2 <- intrvn.end2.vect[s]
    intrvn.iE.factor2 <- intrvn.iE.factor2.vect[s]
    intrvn.iR.factor2 <- intrvn.iR.factor2.vect[s]
    intrvn.r.factor2 <- intrvn.r.factor2.vect[s]
    
    all.settings.current <- all.settings
    all.settings.current[c('intrvn.start','intrvn.end',
                   'intrvn.iE.factor','intrvn.iR.factor',
                   'intrvn.r.factor')] <- c(intrvn.start, intrvn.end, 
                                            intrvn.iE.factor, intrvn.iR.factor, 
                                            intrvn.r.factor)
    all.settings.current[['intrvn']] <- 'End SoE'
    all.settings.current <- c(all.settings.current, list(intrvn.start2=intrvn.start2,
                                         intrvn.end2=intrvn.end2,
                                         intrvn.iE.factor2=intrvn.iE.factor2,
                                         intrvn.iR.factor2=intrvn.iR.factor2,
                                         intrvn.r.factor2=intrvn.r.factor2))
    
    incarc_functions_intrvn <- get_incarc_functions_elsal(iE_func=incarc_functions_cont_SoE$iE_func,
                                                          iR_func=incarc_functions_cont_SoE$iR_func,
                                                          r_func=incarc_functions_cont_SoE$r_func,
                                                          SoEf=all_params[[i]]$main_params[['SoEf']],
                                                          SoEf_iR=all_samps_elsal[current_idx,]$SoEf_iR,
                                                          SoE_r=all_samps_elsal[current_idx,]$SoE_r,
                                                          SoEf2=all_params[[i]]$main_params[['SoEf2']],
                                                          unvarying_incarc_params=c(all_params[[i]]$main_params,
                                                                                    all.settings.current))
    
    plot(seq(1990,2035), incarc_functions_intrvn$iE_func(seq(1990,2035)))
    plot(seq(1990,2035), incarc_functions_cont_SoE$iE_func(seq(1990,2035)))
    
    plot(seq(1990,2035), incarc_functions_intrvn$r_func(seq(1990,2035)))
    plot(seq(1990,2035), incarc_functions_cont_SoE$r_func(seq(1990,2035)))
    
    # print(scenario_p)
    
    output_intrvn <- ode(
      func=tb_model_for_calibration,
      y=xstart,
      times=timeunit,
      parms=c(all_params[[i]]$main_params, all.settings.current),
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
    
    # print(plot(output_dt_intrvn$time, output_dt_intrvn$N1, xlim=c(1990,2035)))
    # print(output_dt_intrvn[time>=2021 & time <= 2034,c('time','N1'),with=F])
    rates_intrvn <- get_annualized_rates(output_dt_intrvn, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)
    
    rates_intrvn <- rbind(rates_intrvn, data.table(pop='Prison', variable='pop',
                                                   rate_per100k=output_dt_intrvn[time >= horiz.start & time <= horiz.end, N1/(N1+N2+N3+N4)], # note: when we ran this, these were just "N1" or "N2" or "N3"; we back-calculated actual prevalence (ie N1/(N1+N2+N3+N4)) in the analysis stage, and then corrected this code to reflect what should have been done
                                                   total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                          data.table(pop='Post-Release', variable='pop',
                                     rate_per100k=output_dt_intrvn[time >= horiz.start & time <= horiz.end, N2/(N1+N2+N3+N4)],
                                     total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                          data.table(pop='Formerly Incarc', variable='pop',
                                     rate_per100k=output_dt_intrvn[time >= horiz.start & time <= horiz.end, N3/(N1+N2+N3+N4)],
                                     total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)
    # print(paste('intrvn...done', j, k))
    # rates_intrvn$scenario <- paste0(j*100, '% lower entry rates; ', k*100, '% higher release rate')
    rates_intrvn$scenario <- scenario_p
    # rates_intrvn$perc_change_iR <- (intrvn.iR.factor-1)*100
    # rates_intrvn$perc_change_iE <- (intrvn.iE.factor-1)*100
    # rates_intrvn$perc_change_r <- (intrvn.r.factor-1)*100
    rates_intrvn_combined <- rbind(rates_intrvn_combined, rates_intrvn, fill=T)
  }
  
  rates_all <- rbindlist(list(rates_cont_SoE, rates_intrvn_combined))
  rates_all$idx <- current_idx
  all_combined[[(i-i_start+1)]] <- rates_all
  print(paste0(i, ' done!'))
}

saveRDS(all_combined, paste0(wd, 'results/future_projections_', gsub(" ", "", tolower(country_p)), '_', today, 
                             '_', i_start, '_', i_end, '.RDS'))

