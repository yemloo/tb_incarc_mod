# script to get time-varying params
# also plots them over time for Fig S5

wd <- '~/Desktop/Stanford/TBprisons_git/'
library(data.table)
library(ggplot2)

# func to get param vals at vector of times t
# this code is copied over from model function but is modified to apply to a vector of time values to return param vals at each timepoint
get_param_vals <- function(t_vect, params, all_settings, 
                           mort_genpop_func, pop_grow_func,
                           iR_func, iE_func, r_func, quasi_2000=2000){
  with(as.list(c(t_vect, params, all_settings)),{
    
    #### This first section governs changing parameters over time ####
    q2 <- iR_func(t_vect-quasi_2000+2000) # q2 is equivalent to iR
    q3 <- iE_func(t_vect-quasi_2000+2000) # q3 is equivalent to iE
    q4 <- q3 # q4 is equivalent to iN
    r <- r_func(t_vect-quasi_2000+2000)
    
    mu4 <- mort_genpop_func(t_vect-quasi_2000+2000)
    mu3 <- mu4
    mu2 <- mu.postrel.irr*mu4
    mu1 <- mu.incarc.irr*mu4
    
    births.deaths.ratio <- sapply(t_vect, function(t){
      births.deaths.ratio <- pop_grow_func(t-quasi_2000+2000)/100
      return(births.deaths.ratio)
    })
    births.deaths.ratio <- births.deaths.ratio / mu4 + 1
    
    beta_cc <- sapply(t_vect, function(t){
      if (t > change.beta.start1){
        if (t <= change.beta.end1){
          beta_cc <- beta_cc + z*(t-change.beta.start1)
        } else {
          beta_cc <- beta_cc + z*(change.beta.end1-change.beta.start1)
        }
      }
      
      if (t > change.beta.start2){
        if (t <= change.beta.end2){
          beta_cc <- beta_cc + z*change.beta.2.factor*(t-change.beta.start2)
        } else {
          beta_cc <- beta_cc + z*change.beta.2.factor*(change.beta.end2-change.beta.start2)
        }
      }
      
      if (t > change.beta.start3){
        if (t <= change.beta.end3){
          beta_cc <- beta_cc + z*change.beta.3.factor*(t-change.beta.start3)
        } else {
          beta_cc <- beta_cc + z*change.beta.3.factor*(change.beta.end3-change.beta.start3)
        }
      }
      
      return(beta_cc)
    })
    
    beta_pp <- sapply(t_vect, function(t){
      if (t > change.betap.start1){
        if (change.betap.exp == TRUE){ # # this is if you want beta in prison to change exponentially rather than linearly, in the form of y = beta_P_baseline + exp(p*t)
          if (t <= change.betap.end1){
            beta_pp <- beta_pp+exp(p*(t-change.betap.start1))-1
          } else {
            beta_pp <- beta_pp+exp(p*(change.betap.end1-change.betap.start1))-1
          }
        } else {
          if (t <= change.betap.end1){
            beta_pp <- beta_pp+p*(t-change.betap.start1)
          } else {
            beta_pp <- beta_pp+p*(change.betap.end1-change.betap.start1)
          }
        }
      }
      
      if (t > change.betap.start2){
        if (change.betap.exp == TRUE){ 
          if (t <= change.betap.end2){
            if (change.betap.2.factor < 0) { # if decay
              beta_pp <- beta_pp*exp(p*change.betap.2.factor*(t-change.betap.start2))
            } else if (change.betap.2.factor >= 0) {
              beta_pp <- beta_pp+exp(p*change.betap.2.factor*(t-change.betap.start2))-1
            }
          } else {
            if (change.betap.2.factor < 0) { 
              beta_pp <- beta_pp*exp(p*change.betap.2.factor*(change.betap.end2-change.betap.start2))
            } else if (change.betap.2.factor >= 0) {
              beta_pp <- beta_pp+exp(p*change.betap.2.factor*(change.betap.end2-change.betap.start2))-1
            }
          }
        } else {
          if (t <= change.betap.end2){
            beta_pp <- beta_pp+p*change.betap.2.factor*(t-change.betap.start2)
          } else {
            beta_pp <- beta_pp+p*change.betap.2.factor*(change.betap.end2-change.betap.start2)
          }
        }
      }
      
      return(beta_pp)
    })
    
    c4 <- sapply(t_vect, function(t){
      if (t > change.prog.start1){
        if (t <= change.prog.end1){
          c4 <- c4 + j*(t-change.prog.start1)
        } else {
          c4 <- c4 + j*(change.prog.end1-change.prog.start1)
        }
      }
      
      if (t > change.prog.start2){
        if (t <= change.prog.end2){
          c4 <- c4 + j*change.prog.2.factor*(t-change.prog.start2)
        } else {
          c4 <- c4 + j*change.prog.2.factor*(change.prog.end2-change.prog.start2)
        }
      }
      
      return(c4)
    })
    
    c3 <- c4
    
    d4 <- sapply(t_vect, function(t){
      if (t > change.d.start1){
        if (t <= change.d.end1){
          d4 <- d4+v*(t-change.d.start1)
        } else {
          d4 <- d4+v*(change.d.end1-change.d.start1)
        }
      }
      
      if (t > change.d.start2){
        if (t <= change.d.end2){
          d4 <- d4+v*change.d.2.factor*(t-change.d.start2)
        } else {
          d4 <- d4+v*change.d.2.factor*(change.d.end2-change.d.start2)
        }
      }
      
      if (t > covid.change.d.start & t <= covid.change.d.end){
        d4 <- d4*covid.d.factor
      }
      
      return(d4)
    })
    
    d3 <- d4
    
    d1 <- sapply(t_vect, function(t){ # d1 doesn't change except for during covid in brazil
      
      if (t > change.d1.start1){
        if (change.d1.exp == TRUE){
          if (t <= change.d1.end1){
            d1 <- d1+exp(v1*(t-change.d1.start1))-1
          } else if (change.d1.temp == FALSE) { # if not temporary, keep the change in diagnosis rates
            d1 <- d1+exp(v1*(change.d1.end1-change.d1.start1))-1
          } # if temporary and t > change.d1.end1, d1 is not changed (equals baseline value)
        } else {
          if (t <= change.d1.end1){
            d1 <- d1+v1*(t-change.d1.start1)
          } else if (change.d1.temp == FALSE) { # if not temporary, keep the change in diagnosis rates
            d1 <- d1+v1*(change.d1.end1-change.d1.start1)
          } # if temporary and t > change.d1.end1, d1 is not changed
        }
      }
      
      if (t > spike.d1.start1 & t <= spike.d1.end1) {
        d1 <- d1*spike.d1.factor
      }
      
      if (t > covid.change.d.start & t <= covid.change.d.end){
        d1 <- d1*covid.d.factor
      }
      
      return(d1)
    })
    
    # progression rate & diagnosis rate for recently released is a WEIGHTED average of the respective rates in prison and never incarc, with weights sampled from distribution
    d2 <- (d1*d1w) + (d4*(1-d1w))
    c2 <- (c1*c1w) + (c4*(1-c1w))
    
    return(list(t=t_vect,
                q2=q2,
                q3=q3,
                q4=q4,
                mu1=mu1,
                mu2=mu2,
                mu3=mu3,
                mu4=mu4,
                r=r,
                beta_cc=beta_cc,
                beta_pp=beta_pp,
                c1=c1,
                c2=c2,
                c3=c3,
                c4=c4,
                d1=d1,
                d2=d2,
                d3=d3,
                d4=d4,
                births.deaths.ratio=births.deaths.ratio
    ))
  })
}

all_countries <- c('Argentina','Brazil','Colombia','El Salvador','Mexico','Peru')
today_list <- c('240617','240603','240612','240605','240611','240612')

for (i in 1:length(all_countries)){
  today <- today_list[i]
  country_p <- all_countries[i]
  
  all_params <- readRDS(paste0(wd, '5-params/all_params_', gsub(' ', '', tolower(country_p)), '_', today, '.RDS'))
  source(paste0(wd, '1-data-and-settings/', gsub(' ', '', tolower(country_p)), '_main_settings_', today, '.R'))
  good_idx <- readRDS(paste0(wd, '3-outputs/good_idx_', gsub(' ', '', tolower(country_p)), '_', today, '.RDS'))
  
  all_params_overtime <- vector('list', length=length(all_params))
  for (i in 1:length(all_params)){
    current_idx <- all_params[[i]]$idx
    if (!current_idx %in% good_idx){
      next
    }
    
    params_p <- get_param_vals(t_vect=seq(1990,2024,by=0.25), params=all_params[[i]]$main_params, 
                               iR_func=all_params[[i]]$incarc_functions$iR_func,
                               iE_func=all_params[[i]]$incarc_functions$iE_func,
                               r_func=all_params[[i]]$incarc_functions$r_func,
                               mort_genpop_func = all_params[[i]]$main_params$mort_genpop_func,
                               pop_grow_func = all_params[[i]]$main_params$pop_grow_func,
                               all_settings = all.settings)
    params_p <- c(params_p,
                  idx=current_idx)
    all_params_overtime[[i]] <- params_p
  }
  
  all_params_overtime_dt <- lapply(all_params_overtime, as.data.table)
  all_params_overtime_dt <- rbindlist(all_params_overtime_dt)
  
  remove(all_params_overtime)
  
  write.csv(all_params_overtime_dt, 
            file=paste0('~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/params/', 
            gsub(' ', '', tolower(country_p)), '_', today, '_params_overtime.csv'),
            row.names = F)
  
  all_params_overtime_dt <- all_params_overtime_dt[,!c('q3','mu3','c3','d3'),with=F] # remove these, are redundant with values in never incarc
  setnames(all_params_overtime_dt, # adjust these to be consistent with param notation in manuscript
           c('q2','q4',
             'mu1','mu2','mu4',
             'c1','c2','c4',
             'd1','d2','d4',
             'births.deaths.ratio'),
           c('q_r','q_d, q_n',
             'mu_p','mu_r','mu_d, mu_n',
             'tau_p','tau_r','tau_d, tau_n',
             'delta_p','delta_r','delta_d, delta_n',
             'vi'))
  
  all_params_overtime_dt_melt <- melt(all_params_overtime_dt, id.vars = c('t','idx'),
                                      variable.name = 'param')
  
  all_params_overtime_dt_melt_summary <- all_params_overtime_dt_melt[,.(med=median(value),
                                                                        lo=quantile(value, 0.025),
                                                                        hi=quantile(value, 0.975)),
                                                                     by=c('t','param')]
  all_params_overtime_dt_melt_summary$param <- factor(all_params_overtime_dt_melt_summary$param,
                                                      levels=c('q_r','q_d, q_n','r','beta_cc','beta_pp',
                                                               'tau_p','tau_r','tau_d, tau_n',
                                                               'delta_p','delta_r','delta_d, delta_n',
                                                               'mu_p','mu_r','mu_d, mu_n','vi'))
  ggplot(all_params_overtime_dt_melt_summary[!param %in% c('tau_p','tau_r','tau_d, tau_n', # these no longer change over time
                                                           'mu_p','mu_r') & t <= 2019]) + # changes in mu_p and mu_r are based on changes in mu_n
    geom_ribbon(aes(x=t, ymin=lo, ymax=hi), alpha=0.2) +
    geom_line(aes(x=t, y=med)) +
    theme_bw() + facet_wrap(~param, scales='free_y', ncol=5) +
    labs(x='year',y='value') + ggtitle(country_p)
  ggsave(paste0(wd, '6-figures/figureS5_time.vary.params.', gsub(' ', '', tolower(country_p)),
                '.', today, '.pdf'), height=3.5, width=10)
}

for (i in 1:length(all_countries)){
  today <- today_list[i]
  country_p <- all_countries[i]
  print(country_p)
  temp <- fread(paste0('~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/params/', 
                        gsub(' ', '', tolower(country_p)), '_', today, '_params_overtime.csv'))
  print(apply(temp[t == 1990,!c('t'),with=F], 2, function(x) {
    paste0(round(median(x), 4), ' (', round(quantile(x, 0.025),4), ', ',
                       round(quantile(x, 0.975),4), ')')
  }))
  cat('\n\n')
}

