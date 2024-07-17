library(data.table)
library(ggplot2)
wd <- '~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/' # local
wd_save <- '~/Desktop/Stanford/TBprisons_git/'

all_params_dt_all <- data.table()
all_countries <- c('Argentina','Brazil','Colombia','El Salvador','Mexico','Peru')
today_list <- c('240617','240603','240612','240605','240611','240612')
for (c in 1:length(all_countries)){
  country_p <- all_countries[c]
  today_p <- today_list[c]
  print(country_p)
  # read in and bind the final param sets (with calibration errors)
  all_params <- lapply(list.files(paste0(wd, 'results/', gsub(" ", "", tolower(country_p)), '_', today_p, ''),
                                  pattern = "params.*\\.RDS", full.names = T), readRDS)
  good_idx <- readRDS(paste0(wd_save, '3-outputs/good_idx_', gsub(" ", "", tolower(country_p)), '_', today_p, '.RDS'))
  
  exclude_from_dt <- c('pop_grow','pop_grow_func','mort_genpop_func','j')
  
  all_params_list <- lapply(all_params, function(x){
    if (x$main_params[['idx']] %in% good_idx){
      temp <- as.data.table(x$main_params[setdiff(names(x$main_params), exclude_from_dt)])
      
      # get percent changes in entry and release rates
      iE_vect <- x$incarc_functions$iE_func(seq(1990, 2019))
      iE_perc_change <- (iE_vect[length(iE_vect)] - iE_vect[1]) / iE_vect[1] * 100
      iE_perc_change_max <- (max(iE_vect) - iE_vect[1]) / iE_vect[1] * 100
      iE_perc_change_max_yr <- seq(1990,2019)[which.max(iE_vect)]
      
      iR_vect <- x$incarc_functions$iR_func(seq(1990, 2019))
      iR_perc_change <- (iR_vect[length(iR_vect)] - iR_vect[1]) / iR_vect[1] * 100
      iR_perc_change_max <- (max(iR_vect) - iR_vect[1]) / iR_vect[1] * 100
      iR_perc_change_max_yr <- seq(1990,2019)[which.max(iR_vect)]
      
      r_vect <- x$incarc_functions$r_func(seq(1990, 2019))
      r_perc_change <- (r_vect[length(r_vect)] - r_vect[1]) / r_vect[1] * 100
      r_perc_change_max <- (min(r_vect) - r_vect[1]) / r_vect[1] * 100
      
      temp$iE_perc_change <- iE_perc_change
      temp$iE_perc_change_max <- iE_perc_change_max
      temp$iE_perc_change_max_yr <- iE_perc_change_max_yr
      
      temp$iR_perc_change <- iR_perc_change
      temp$iR_perc_change_max <- iR_perc_change_max
      temp$iR_perc_change_max_yr <- iR_perc_change_max_yr
      
      temp$r_perc_change <- r_perc_change
      temp$r_perc_change_max <- r_perc_change_max
      
      return(temp)
    }
  })
  
  all_params_list <- all_params_list[!sapply(all_params_list,is.null)]
  
  all_params_dt <- rbindlist(all_params_list)
  all_params_dt$country <- country_p
  
  all_params_dt_all <- rbind(all_params_dt_all, all_params_dt, fill=T)
}

saveRDS(all_params_dt_all, paste0(wd_save, '5-params/all_params_all_countries_240708.RDS'))

# starting values for calib (priors)
start_params_all <- data.table()
for (c in 1:length(all_countries)){
  country_p <- all_countries[c]
  today_p <- today_list[c]
  print(country_p)
  good_idx <- readRDS(paste0(wd_save, '3-outputs/good_idx_',gsub(" ", "", tolower(country_p)), '_', today_p, '.RDS'))
  
  if (country_p == 'El Salvador'){
    all_samps_calib_start <- fread(paste0(wd, 'params/uncertainty_analysis/elsal_samps_calib_start_', today_p, '.csv'))
  } else if (country_p == 'Mexico'){
    all_samps_calib_start <- fread(paste0(wd, 'params/uncertainty_analysis/mex_samps_calib_start_', today_p, '.csv'))
  } else {
    all_samps_calib_start <- fread(paste0(wd, 'params/uncertainty_analysis/', gsub(" ", "", tolower(country_p)), '_samps_calib_start_', today_p, '.csv'))
  }
  
  all_samps_calib_start$country <- country_p
  start_params_all <- rbind(start_params_all, all_samps_calib_start, fill=T)
}

saveRDS(start_params_all, paste0(wd_save, '5-params/uncertainty_analysis/start_params_all_countries_240708.RDS'))
