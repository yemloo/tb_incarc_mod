# use this script to do the following:
# - run the "abolition" scenario for tPAF
# - return model outputs:
#   - releases
#   - prevalences

library(data.table)
library(deSolve)

args <- commandArgs(trailingOnly = T)

today <- as.character(args[1])
country_p <- as.character(args[2])

wd <- '~/TBprisons/' # Sherlock
# print(paste0(wd, 'results/', gsub(" ", "", tolower(country_p)), '_', today, '/'))

source(paste0(wd, 'scripts/tb_incarc_models_240528_newformulation.R')) # script with model functions
source(paste0(wd, 'scripts/', gsub(" ", "", tolower(country_p)), '_main_settings_', today, '.R'))

# new as of 12/30: only do this for the good fits
good_idx <- readRDS(paste0(wd, 'scripts/good_idx_', gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))

horiz.start=1990
horiz.end=2024
time.increment=1
timeunit <- c(0,seq(horiz.start,horiz.end+1, by=time.increment))

intrvn.start=1990
intrvn.end=2009

all_params <- readRDS(paste0(wd, 'results/all_params_', gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))
total_length <- length(all_params)

all_incarc_mat <- vector('list', length=3000)
all_tb_mat <- vector('list', length=3000)
all_abol <- vector('list', length=3000)

for (i in 1:3000){
  if (i > total_length){
    print(paste0('reached end of all_params object at i=', i))
    break
  }
  current_idx <- all_params[[i]]$main_params[['idx']]
  if (!current_idx %in% good_idx){
    next
  }
  
  # run and save incarc output matrices
  xstart <- c(P=0, S=0, R=0, N=100000, E=0, Ishadow=0, Ireshadow=0, Eshadow=0)
  output <- ode(
    func=prison.model.with.growth,
    y=xstart,
    times=timeunit,
    parms=c(all_params[[i]]$main_params, all.settings),
    iR_func=all_params[[i]]$incarc_functions$iR_func,
    iE_func=all_params[[i]]$incarc_functions$iE_func,
    r_func=all_params[[i]]$incarc_functions$r_func,
    quasi_2000=2000
  )
  
  output_dt <- data.table(output)
  output_dt[, Ntotal := P+S+R+E+N]
  output_dt$idx <- current_idx
  
  all_incarc_mat[[i]] <- output_dt[time>=1990]
  
  # run and save tb output matrices
  xstart <- c(S1=0.005*0.95*100000, E1=0, L1=0, I1=0.005*0.05*100000, R1=0,
              S2=0.005*0.95*100000, E2=0, L2=0, I2=0.005*0.05*100000, R2=0,
              S3=0, E3=0, L3=0, I3=0, R3=0,
              S4=0.99*0.95*100000, E4=0, L4=0, I4=0.99*0.05*100000, R4=0,
              I1shadow=0, I2shadow=0, I3shadow=0, I4shadow=0,
              D1shadow=0, D2shadow=0, D3shadow=0, D4shadow=0,
              muI1shadow=0, muI2shadow=0, muI3shadow=0, muI4shadow=0)
  output <- ode(
    func=tb_model_for_calibration,
    y=xstart,
    times=timeunit,
    parms=c(all_params[[i]]$main_params, all.settings),
    iR_func=all_params[[i]]$incarc_functions$iR_func,
    iE_func=all_params[[i]]$incarc_functions$iE_func,
    r_func=all_params[[i]]$incarc_functions$r_func,
    quasi_2000=2000
  )
  
  output_dt <- data.table(output)
  output_dt[,`:=`(N1=S1+E1+L1+I1+R1,
                  N2=S2+E2+L2+I2+R2,
                  N3=S3+E3+L3+I3+R3,
                  N4=S4+E4+L4+I4+R4)]
  output_dt$idx <- current_idx
  all_tb_mat[[i]] <- output_dt[time>=1990]
  
  # abolition
  iR_func <- approxfun(x=c(intrvn.start, intrvn.end), 
                       y=c(all_params[[i]]$main_params[['iR']], 0),
                       rule=2)
  iE_func <- approxfun(x=c(intrvn.start, intrvn.end), 
                       y=c(all_params[[i]]$main_params[['iR']]*all_params[[i]]$main_params[['iE:iR']], 0),
                       rule=2)
  r_func <- approxfun(x=c(intrvn.start, intrvn.end), 
                       y=c(all_params[[i]]$main_params[['r']], 2), # 2 for all countries except Peru, 1.5 for Peru
                       rule=2)
  
  output <- ode(
    func=tb_model_for_calibration,
    y=xstart,
    times=timeunit,
    parms=c(all_params[[i]]$main_params, all.settings),
    iR_func=iR_func,
    iE_func=iE_func,
    r_func=r_func,
    quasi_2000=2000
  )
  
  output_dt <- data.table(output)
  output_dt[,`:=`(N1=S1+E1+L1+I1+R1,
                  N2=S2+E2+L2+I2+R2,
                  N3=S3+E3+L3+I3+R3,
                  N4=S4+E4+L4+I4+R4)]
  
  # ggplot(output_dt[time>=1990], aes(x=time, y=N1)) + geom_line()
  # tail(output_dt)
  
  rates <- get_annualized_rates(output_dt, horiz.start = horiz.start, horiz.end = horiz.end, by.increment = time.increment)
  
  rates <- rbind(rates, data.table(pop='Prison', variable='pop',
                                                 rate_per100k=output_dt[time >= horiz.start & time <= horiz.end, N1],
                                                 total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                        data.table(pop='Post-Release', variable='pop',
                                   rate_per100k=output_dt[time >= horiz.start & time <= horiz.end, N2],
                                   total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),
                        data.table(pop='Formerly Incarc', variable='pop',
                                   rate_per100k=output_dt[time >= horiz.start & time <= horiz.end, N3],
                                   total=NA, time=seq(horiz.start, horiz.end, by=time.increment)),fill=T)
  rates$scenario <- 'Abolition'
  rates$idx <- current_idx
  all_abol[[i]] <- rates
  print(paste0(current_idx, ' done!'))
}

saveRDS(all_incarc_mat, paste0(wd, 'results/incarc_mat_', gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))
saveRDS(all_tb_mat, paste0(wd, 'results/tb_mat_', gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))
saveRDS(all_abol, paste0(wd, 'results/abolition_', gsub(" ", "", tolower(country_p)), '_', today, '.RDS'))
                            
