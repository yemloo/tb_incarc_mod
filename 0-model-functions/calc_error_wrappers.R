#-------------------------------------------
# Author: Yiran Liu
# Date Modified: 6.11.24
# Description: This contains the wrapper functions for optimization, which take
# a given parameter set and return the output of the loss function for the first
# or second step of calibration
#-------------------------------------------
library(deSolve)

################################################################################
#           CALIBRATING INCARC PARAMS
################################################################################

# wrapper function to calculate errors between target & model output for calibrating incarceration sub-model
# calibration targets: incarc prevalence, admissions, recidivism percent (% of prison pop w/ prior incarc, or % of admissions w/ prior incarc)
optim_for_iE_iR_perct_grow <- function(incarc_params, incarc_param_names, timeunit=timeunit, country_p,
                                       unvarying_incarc_params,
                                       xstart = c(P=0, S=0, R=0, N=100000,E=0, Ishadow=0, Ireshadow=0, Eshadow=0),
                                       incarc.prev.times=c(), 
                                       incarc_prev_known=c(), # calibration target
                                       recid_percent_known, recid.time=Inf, # calibration target
                                       admissions_known=c(), admissions.times=c(), # calibration target
                                       fit.end.idx=c(0), # to toggle which "end" year for incarceration prevalence to fit to (ie 2023 vs 2019) - idx subtracted from the last of incarc_prev_known
                                       intrvn='Continue SoE',
                                       quasi_2000=2000
){
  # params is a vector, usually with the following:
  # 1. iR
  # 2. ratio of iE:iR (note: iE = iN)
  # 3. r
  # 4. k1 (rate of change in iR and iE in interval 1)
  # 5. k2 (rate of change in iR and iE in interval 2)
  # 6. k3 (rate of change in iR and iE in interval 3)
  # 7. covidf (change factor in admissions and release rates during COVID)
  # 8. SoEf (change factor in admissions rates during state of emergency - added)
  # 9. k4 (rate of change in iR and iE in interval 4)
  
  names(incarc_params) <- incarc_param_names
  incarc_params[['iE']] <- incarc_params[['iR']]*incarc_params[['iE:iR']]
  
  incarc_functions <- get_incarc_functions(incarc_params=incarc_params, # named vector, should include iR, iE, r, k1 (at minimum); may include k2, k3, covidf, SoEf
                                           unvarying_incarc_params=unvarying_incarc_params) # contains all settings
  if (country_p == 'El Salvador'){
    new_funcs <- get_incarc_functions_elsal(iE_func=incarc_functions$iE_func,
                               iR_func=incarc_functions$iR_func,
                               r_func=incarc_functions$r_func,
                               unvarying_incarc_params=unvarying_incarc_params,
                               SoEf=incarc_params[['SoEf']],
                               SoEf_iR=unvarying_incarc_params[['SoEf_iR']],
                               SoE_r=unvarying_incarc_params[['SoE_r']],
                               SoEf2=incarc_params[['SoEf2']], 
                               # intrvn=intrvn,
                               quasi_2000=quasi_2000)
    incarc_functions <- new_funcs
  }
  
  all_params <- c(incarc_params, unvarying_incarc_params)
  
  #### run incarc model with this set of params ####
  output <- ode(
    func=prison.model.with.growth,
    y=xstart,
    times=timeunit,
    parms=all_params,
    iR_func=incarc_functions$iR_func,
    iE_func=incarc_functions$iE_func,
    r_func=incarc_functions$r_func,
    quasi_2000=quasi_2000
  )
  
  output_dt <- data.table(output)
  output_dt[, Ntotal := P+S+R+E+N]
  
  if (length(admissions.times)>0){
    admissions_obs <- (output_dt[time %in% c(admissions.times+1), Ishadow] - output_dt[time %in% admissions.times, Ishadow]) /
      output_dt[time %in% c(admissions.times), Ntotal] * 100000
  }
  
  incarc_prev_obs <- output_dt[time %in% incarc.prev.times, (S + P)/Ntotal*100000] # population incarc per 100K
  
  if (country_p == "Mexico"){ # in Mexico, "recid.percent" is the percent of all *admissions* that are re-admissions
    all_admissions_obs <- (output_dt[time %in% c(recid.time+1), Ishadow] - output_dt[time %in% recid.time, Ishadow])
    readmissions_obs <- output_dt[time %in% c(recid.time+1), Ireshadow] - output_dt[time %in% recid.time, Ireshadow]
    percent_readmissions <- readmissions_obs/all_admissions_obs
  } else {
    recid_percent_obs <- output_dt[time %in% recid.time, S] / (output_dt[time %in% recid.time, S] + output_dt[time %in% recid.time, P]) # percentage with prior incarc
  }
  
  #### errors ###
  # incarc prev over entire period
  e_one = mean(((incarc_prev_obs - incarc_prev_known)/incarc_prev_known)^2) 
  # incarc prev at baseline
  e_two = ((incarc_prev_obs[1] - incarc_prev_known[1])/incarc_prev_known[1])^2 
  # incarc prev at end of period
  e_three = mean(((incarc_prev_obs[length(incarc_prev_known)-fit.end.idx] - # fit.end.idx = c(0) if you want to use prev in the final year; = c(0,1) if you want to use average for the last two years
                     incarc_prev_known[length(incarc_prev_known)-fit.end.idx])/
                    incarc_prev_known[length(incarc_prev_known)-fit.end.idx])^2) 
  # recidivism
  if (country_p == "Mexico"){
    e_four = mean(((percent_readmissions - recid_prct)/recid_prct)^2)
  } else {
    e_four = mean(((recid_percent_obs - recid_prct)/recid_prct)^2)
  }
  # admissions
  if (length(admissions.times)>0){
    e_five = mean(((admissions_obs - admissions_known)/admissions_known)^2)
    error = (e_one + e_two + e_three + e_four + e_five) / 5
  } else { 
    error = (e_one + e_two + e_three + e_four) / 4
  }
  
  #### Uncomment the below when troubleshooting to plot fit ####
  # if (error < smallest_error){
  #   print(e_one)
  #   print(e_two)
  #   print(e_three)
  #   print(e_four)
  #   if (length(admissions.times)>0){
  #     print(e_five)
  #   }
  # 
  #   print(incarc_params)
  #   # print(admissions_obs)
  # 
  #   cat('\n\n')
  #   smallest_error <<- error
  #   # print(output_dt[time >= (2018-2000+quasi_2000) & time <= (2024-2000+quasi_2000)])
  #   # print(incarc_functions$iR_func(seq(quasi_2000-15+0.5, quasi_2000+30+0.5)))
  #   # print(incarc_functions$iE_func(seq(quasi_2000-15+0.5, quasi_2000+30+0.5)))
  #   # print(incarc_functions$r_func(seq(quasi_2000-15+0.5, quasi_2000+30+0.5)))
  #   print(ggplot(data.table(prev_obs=incarc_prev_obs, prev_known=incarc_prev_known,
  #                           time=incarc.prev.times), aes(x=time, y=prev_obs)) +
  #           geom_point(color='blue') + geom_point(aes(x=time, y=prev_known), color='black'))
  # 
  # }
  return(error)
}

################################################################################
#           CALIBRATING TB PARAMS
################################################################################

# function that gets fed into optim - runs the model with given set of parameters/targets, calculates outputs and computes error compared to data targets
# calibration targets: prison & general pop TB incidence & notifications
calc_diffs <- function(params_to_calibrate, 
                       param_names,
                       times_to_use=c(),
                       iR_func,
                       iE_func,
                       r_func,
                       quasi_2000=2000, country_p='Brazil',
                       who_incid_factor=c(), # factor to convert gen pop notification rates to incidence estimates (sampled in uncertainty analysis)
                       prison_incid, prison_incid_yr=c(2010), # per 100k, data target sampled in uncertainty analysis
                       # c2.perc.change=NA, # Mexico-only
                       # v1.ratio=NA, # Mexico-only
                       k=1, # multiplier for error inflation when final beta or final d are out of bounds
                       upper_betap_thresh=75,
                       d_thresh=2,
                       unvarying_params=c(),
                       error_thresh=0
                       ){ 
  
  with(as.list(unvarying_params),{
    
    ##########################################################
    ####### set all model parameters in this iteration #######
    ##########################################################
    
    names(params_to_calibrate) <- param_names
    all_params <- c(beta_pp=params_to_calibrate[['beta_pp']], 
                    beta_cc=params_to_calibrate[['beta_pp']]*params_to_calibrate[['beta_cc:beta_pp']], 
                    c1=params_to_calibrate[['c1']],
                    c2=params_to_calibrate[['c1']]*params_to_calibrate[['c4:c1']],
                    c3=params_to_calibrate[['c1']]*params_to_calibrate[['c4:c1']],
                    c4=params_to_calibrate[['c1']]*params_to_calibrate[['c4:c1']],
                    unvarying_params)
    # all_params[['c2']] <- (all_params[['c1']]*unvarying_params[['c1w']]) + (all_params[['c4']]*(1-unvarying_params[['c1w']]))
    if ('d' %in% param_names){
      all_params[['d1']] <- params_to_calibrate[['d']]
      all_params[['d2']] <- params_to_calibrate[['d']]
      all_params[['d3']] <- params_to_calibrate[['d']]
      all_params[['d4']] <- params_to_calibrate[['d']]
    }
    if ('v' %in% param_names){
      all_params[['v']] <- params_to_calibrate[['v']]
    }
    if ('j' %in% param_names){
      all_params[['j']] <- params_to_calibrate[['j']]
    }
    if ('p' %in% param_names){
      all_params[['p']] <- params_to_calibrate[['p']]
    }
    if ('z' %in% param_names){
      all_params[['z']] <- params_to_calibrate[['z']]
    }
    if ('d1:d4' %in% param_names){
      all_params[['d1']]=params_to_calibrate[['d']]*params_to_calibrate[['d1:d4']]
      # all_params[['d2']] <- (all_params[['d1']]*unvarying_params[['d1w']]) + (all_params[['d4']]*(1-unvarying_params[['d1w']]))
    }
    if ('v1ratio' %in% param_names | !is.na(v1.ratio)){
      total.change.d4 <- all_params[['v']]*(change.d.end1-change.d.start1) # calculate total change in diagnosis rate outside prison
      if (change.d.start2 != Inf & change.d.end2 != Inf){
        total.change.d4 <- total.change.d4 + all_params[['v']]*change.d.2.factor*(change.d.end2-change.d.start2)
      }
      max_possible_v1 <- total.change.d4 / (change.d1.end1 - change.d1.start1)
      if ('v1ratio' %in% param_names){
        all_params[['v1']] <- max_possible_v1*params_to_calibrate[['v1ratio']]
      } else if (!is.na(v1.ratio)) {
        all_params[['v1']] <- max_possible_v1*v1.ratio
      }
    }
    if (country_p == 'Mexico'){ # for Mexico, change rates in beta in periods 2 and 3 are calibrated
      all_params[['j']] <- all_params[['c4']]*c2.perc.change # for Mexico, j is not calibrated, it is sampled from a distribution as yearly percent change from baseline
      if ('z2:z' %in% param_names){
        change.beta.2.factor <- params_to_calibrate[['z2:z']]
        all_params[['change.beta.2.factor']] <- change.beta.2.factor
      }
      if ('z3:z' %in% param_names){
        change.beta.3.factor <- params_to_calibrate[['z3:z']]
        # change.beta.3.factor <- -change.beta.2.factor # new 6/6/24: opposite of change.beta.2.factor
        all_params[['change.beta.3.factor']] <- change.beta.3.factor
      }
    }
    if ('covid.d.factor' %in% param_names){
      all_params[['covid.d.factor']] <- params_to_calibrate[['covid.d.factor']]
      # covid.change.d.factor <- params_to_calibrate[['covid.d.factor']]
    } else {
      all_params[['covid.d.factor']] <- 1
    }
    
    
    ####################################
    ####### set up and run model #######
    ####################################
    
    
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
      times=times_to_use,
      parms=all_params,
      iR_func=iR_func,
      iE_func=iE_func,
      r_func=r_func,
      quasi_2000=quasi_2000
    )
    
    output_dt <- data.table(output)
    output_dt[,`:=`(N1=S1+E1+L1+I1+R1,
                    N2=S2+E2+L2+I2+R2,
                    N3=S3+E3+L3+I3+R3,
                    N4=S4+E4+L4+I4+R4)]
    
    # print(tail(output_dt))
    # print(ggplot(output_dt) + geom_line(aes(x=time, y=I1/N1)) + # uncomment if you need to check incarc fit again
    #         geom_line(aes(x=time, y=I2/N2)) +
    #         geom_line(aes(x=time, y=I3/N3)) +
    #         geom_line(aes(x=time, y=I4/N4)) +
    #         scale_y_log10())
    # print(ggplot(output_dt[time >= (quasi_2000 - 10)], aes(x=time, y=N1)) + geom_line())
    modelres <- get_annualized_rates(output_dt, horiz.start = quasi_2000-10, horiz.end = quasi_2000+24)
    
    # print(prison_incid_yr)
    # print(prison_incid)
    prison_TB_incid_obs <- modelres[pop == 'Prison' & variable == 'I' & time %in% c(prison_incid_yr-2000+quasi_2000), rate_per100k]
    # print(prison_TB_incid_obs)
    prison_TB_notif_yrs <- prison_estimates[Country == country_p & !is.na(NR_Obs_New) & Year <= 2022, Year]
    prison_TB_notif_obs <- modelres[pop == 'Prison' & variable == 'D' & 
                                      time %in% (prison_TB_notif_yrs - 2000 + quasi_2000), 
                                    rate_per100k]
    
    comb_TB_incid_yrs <- seq(2000, 2022)
    comb_TB_notif_yrs <- who_data[country == country_p & year >= 1990 & year <= 2022 &
                                    !is.na(c_newinc_per100k), year]
    comb_TB_death_yrs <- seq(2000, 2022)
    
    if (country_p == 'Mexico'){
      comb_TB_incid_yrs <- setdiff(comb_TB_incid_yrs, c(2004)) # exclude 2004
      comb_TB_notif_yrs <- setdiff(comb_TB_notif_yrs, c(1995, 2004)) # remove these two years which are outliers and are driving the fit down
    }
    comb_TB_incid_obs <- modelres[pop == 'Combined' & variable == 'I' & 
                                    time %in% (comb_TB_incid_yrs - 2000 + quasi_2000), rate_per100k]
    # print(comb_TB_incid_obs)
    comb_TB_notif_obs <- modelres[pop == 'Combined' & variable == 'D' &
                                    time %in% (comb_TB_notif_yrs - 2000 + quasi_2000), rate_per100k]
    # print(comb_TB_notif_obs)
    comb_TB_deaths_obs <- modelres[pop == 'Combined' & variable == 'muI' & 
                                     time %in% (comb_TB_death_yrs - 2000 + quasi_2000), rate_per100k]
    
    # prison incidence
    # prison_incid <- prison_estimates[Country == country_p & 
    #                                    Year %in% prison_incid_yr, NR_Obs_New]*prison_incid_factor*100000
    error1 <- mean(abs((prison_TB_incid_obs - prison_incid)/prison_incid))
    
    # combined incidence 
    comm_TB_incid_exp <- who_data[country == country_p & year %in% comb_TB_incid_yrs, c_newinc_per100k]*who_incid_factor
    # change2020from2019 <- who_data[country == country_p & year == 2020, e_inc_100k] / who_data[country == country_p & year == 2019, e_inc_100k] 
    # comm_TB_incid_exp[length(comm_TB_incid_exp-1)] <- comm_TB_incid_exp[length(comm_TB_incid_exp-2)]*change2020from2019 # adjust the value for 2020 since we don't want to use the same multiplier there
    error2 <- mean(abs((comb_TB_incid_obs - comm_TB_incid_exp)/
                         comm_TB_incid_exp))
    
    # prison notifications
    error3 <- mean(abs((prison_TB_notif_obs - prison_estimates[Country == country_p & Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)/
                         (prison_estimates[Country == country_p & Year %in% prison_TB_notif_yrs,  NR_Obs_New]*100000)))
    # combined notifications
    error4 <- mean(abs((comb_TB_notif_obs - who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k])/
                         who_data[country == country_p & year %in% comb_TB_notif_yrs, c_newinc_per100k]))
    
    
    final_error <- mean(c(error1,error2,error3,error4)) # 12/12/23: removed inclusion of deaths in error
    
    crossed_bounds <- FALSE
    beta_thresh <- 3
    # beta_thresh <- 2/all_params[['assrt.fctr']] # consider assrt.fctr bc w/ assortative mixing beta_cc will be lower
    c_thresh <- 0.02
    
    # set bounds for final betas, prog rates, diag rates (based on face validity) - inflate error if any of these are beyond reasonable bounds
    if (change.beta.start1 != Inf & change.beta.end1 != Inf){
      final_beta <- all_params[['beta_cc']] + all_params[['z']]*(change.beta.end1-change.beta.start1)
      if (change.beta.start2 != Inf & change.beta.end2 != Inf & all_params[['z']]*change.beta.2.factor < 0){
        final_beta <- final_beta + all_params[['z']]*change.beta.2.factor*(change.beta.end2-change.beta.start2)
        if (change.beta.start3 != Inf & change.beta.end3 != Inf & all_params[['z']]*change.beta.3.factor < 0){
          final_beta <- final_beta + all_params[['z']]*change.beta.3.factor*(change.beta.end3-change.beta.start3)
        }
      }
      if (final_beta < beta_thresh){ 
        crossed_bounds <- TRUE
        final_error <- final_error + k*(exp((beta_thresh-final_beta)/beta_thresh) - 1)
        # print(paste0('Final Beta too low: ', final_beta))
      } 
    }
    
    if (change.prog.start1 != Inf & change.prog.end1 != Inf){
      final_prog <- all_params[['c4']] + all_params[['j']]*(change.prog.end1-change.prog.start1)
      if (change.prog.start2 != Inf & change.prog.end2 != Inf & all_params[['j']]*change.prog.2.factor < 0){
        final_prog <- final_prog + all_params[['j']]*change.prog.2.factor*(change.prog.end2-change.prog.start2)
      }
      if (final_prog < c_thresh){
        crossed_bounds <- TRUE
        final_error <- final_error + k*(exp((c_thresh-final_prog)/c_thresh) - 1)
        # print(paste0('Final progression rate too low: ', final_prog))
      } 
    }
    
    if (change.d.start1 != Inf & change.d.end1 != Inf){
      final_d <- all_params[['d4']] + all_params[['v']]*(change.d.end1-change.d.start1)
      if (change.d.start2 != Inf & change.d.end2 != Inf){
        final_d <- final_d + all_params[['v']]*change.d.2.factor*(change.d.end2-change.d.start2)
      }
      if (final_d > d_thresh){
        crossed_bounds <- TRUE
        final_error <- final_error + k*(exp((final_d-d_thresh)/d_thresh) - 1)
        # print(paste0('Final d too high: ', final_d))
      } 
    }
    
    if (change.betap.start1 != Inf & change.betap.end1 != Inf){
      final_beta_P <- all_params[['beta_pp']] + all_params[['p']]*(change.betap.end1-change.betap.start1)
      if (change.betap.start2 != Inf & change.betap.end2 != Inf){
        final_beta_P <- final_beta_P + all_params[['p']]*change.betap.2.factor*(change.betap.end2-change.betap.start2)
      }
      if (final_beta_P > upper_betap_thresh){
        crossed_bounds <- TRUE
        final_error <- final_error + k*(exp((final_beta_P-upper_betap_thresh)/upper_betap_thresh) - 1)
        # cat("\n\n")
        # print(paste0('Final beta prison too high: ', final_beta_P))
      } 
    }
    
    if (mean(comb_TB_incid_obs)<5){ # if incid too low...
      final_error <- final_error + k*(exp(5-mean(comb_TB_incid_obs)) - 1)
    }
    
    # ### Uncomment the below when troubleshooting to plot fit ####
    # plot=T
    # if(final_error < (0.97*last_plotted_error) & plot == T){
    # # if (plot == T){
    #   who_country <- who_data[country == country_p, .(year, c_newinc_per100k, e_mort_100k)]
    #   setnames(who_country, c('year','c_newinc_per100k','e_mort_100k'), c('time', 'D', 'muI'))
    #   if (country_p == 'Mexico'){
    #     who_country[time>=2000 & time <= 2022 & time != 2004,
    #                 `:=`(I=who_country[time>=2000 & time <= 2022 & time != 2004, D]*who_incid_factor)]
    #   } else {
    #     who_country[time>=2000 & time <= 2022,`:=`(I=who_country[time>=2000 & time <= 2022, D]*who_incid_factor)]
    #   }
    #   # who_country[,muI := muI+muTB_factor]
    #   who_country <- melt(who_country, id.vars = 'time', value.name = 'rate_per100k')
    #   who_country$pop <- 'Combined'
    #   who_country$source <- 'Data'
    # 
    #   prison_country <- prison_estimates[Country == country_p, .(Year, NR_Obs_New)]
    #   setnames(prison_country, c('Year','NR_Obs_New'), c('time','D'))
    #   prison_country[time %in% prison_incid_yr, I:=prison_incid/100000]
    #   prison_country <- melt(prison_country, id.vars = 'time', value.name = 'rate_per100k')
    #   prison_country$rate_per100k <- prison_country$rate_per100k*100000
    #   prison_country$pop <- 'Prison'
    #   prison_country$source <- 'Data'
    # 
    #   modelres$source <- 'Model Output'
    #   modelres$time <- modelres$time - quasi_2000 + 2000
    #   compare_with_data <- rbind(modelres[pop %in% c('Prison','Combined') & variable %in% c('I','D','muI') & time < 2025],
    #                              who_country[time >= 1990 & time <= 2022],
    #                              prison_country,
    #                              fill=TRUE)
    # 
    #   compare_with_data$variable <- factor(compare_with_data$variable,
    #                                        levels = c('I','D','muI'),
    #                                        labels = c('Cases','Notifications','Deaths'))
    # 
    #   plt <- ggplot() +
    #     geom_line(data=compare_with_data[source == 'Model Output'],
    #               aes(x=time, y=rate_per100k, color=source)) +
    #     geom_point(data=compare_with_data[source == 'Data'],
    #                aes(x=time, y=rate_per100k, color=source), size=0.5) +
    #     facet_grid(pop~variable, scales = 'free') + theme_bw() +
    #     scale_color_manual(values=get_palette('locuszoom',12)) + xlab('year') +
    #     theme(axis.text.x=element_text(angle=45, hjust=1)) + ylim(0,NA)
    #   print(plt)
    #   last_plotted_error <<- final_error
    # }
    # 
    # print(paste0('Error: ', final_error))
    # print(paste0('Params = ', params_to_calibrate))
    # print(error1)
    # print(error2)
    # print(error3)
    # print(error4)
    # # print(error5)
    # cat('\n\n')
    
    if (final_error <= error_thresh) {
      params_error_thresh <<- params_to_calibrate
      error_reached <<- final_error
      # Throw an error to stop the optimization process
      stop("Error threshold reached")
    }
    
    return(final_error)
  })
}
