wd <- '~/Library/CloudStorage/Box-Box/yiranliu/STANFORD/TBprisons/' # local
wd_save <- '~/Desktop/Stanford/TBprisons_git/'

# read in data
latam_incarc_data <- fread(paste0(wd, 'data/latam_incarc_data_231203.csv'))
prison_estimates <- fread(paste0(wd, 'data/prison_estimates_latam.csv'))
who_data <- fread(paste0(wd, 'data/who_data_wsplines.csv'))
setorder(who_data, country, year)
setorder(prison_estimates, Country, Year)

# also make a dt to track strata pop sizes
all_pop_sizes <- data.table()

###### ARGENTINA ###### 
country_p <- 'Argentina'
today <- 240617

# read in and bind the tb model output matrices
tb_mat <- readRDS(paste0(wd_save, '3-outputs/tb_mat_', gsub(' ', '', tolower(country_p)), '_', today, '.RDS'))
tb_mat_dt <- rbindlist(tb_mat[-which(sapply(tb_mat, is.null))])
length(unique(tb_mat_dt$idx))
setorder(tb_mat_dt, idx, time)

tb_mat_dt$country <- country_p
all_pop_sizes <- rbind(all_pop_sizes, tb_mat_dt[,c('time','N1','N2','N3','N4','idx','country'),
                                                with=F])

tb_mat_dt[, muIshadow := muI1shadow + muI2shadow + muI3shadow + muI4shadow]
tb_mat_dt[, muIshadow.next := shift(.SD, 1, type='lead'), .SDcols='muIshadow', by='idx']
tb_mat_dt[, Ideathrate := (muIshadow.next - muIshadow)/(N1+N2+N3+N4) * 100000]

tb_mat_dt[,`:=`(TBprev.prison = I1/N1,
                LTBIprev.prison = (E1+L1)/N1)]

tb_mat_dt_summary <- tb_mat_dt[!is.na(Ideathrate),
                               .(TBprev.prison.med = median(TBprev.prison),
                                 TBprev.prison.lo = quantile(TBprev.prison, 0.025),
                                 TBprev.prison.hi = quantile(TBprev.prison, 0.975),
                                 LTBIprev.prison.med = median(LTBIprev.prison),
                                 LTBIprev.prison.lo = quantile(LTBIprev.prison, 0.025),
                                 LTBIprev.prison.hi = quantile(LTBIprev.prison, 0.975),
                                 Ideathrate.med = median(Ideathrate),
                                 Ideathrate.lo = quantile(Ideathrate, 0.025),
                                 Ideathrate.hi = quantile(Ideathrate, 0.975)),
                               by='time']
ggplot(tb_mat_dt_summary[time %in% who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019, year]]) +
  geom_ribbon(aes(x=time, ymin=Ideathrate.lo, ymax=Ideathrate.hi), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=Ideathrate.med), color='navyblue') +
  geom_point(data=who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019], aes(x=year, y=e_mort_100k),size=0.5) +
  geom_errorbar(data=who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019], 
                aes(x=year, ymin=e_mort_100k_lo, ymax=e_mort_100k_hi), linewidth=0.2) +
  theme_bw() + labs(x='Year', y='TB deaths per 100k') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBdeaths_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=TBprev.prison.lo*100, ymax=TBprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=TBprev.prison.med*100), color='navyblue') +
  geom_hline(aes(yintercept = 1.680), linetype='dotted', linewidth=1) +
  geom_hline(aes(yintercept = 0.830), linetype='dotted') +
  geom_hline(aes(yintercept = 2.970), linetype='dotted') +
  theme_bw() + labs(x='Year', y='TB prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBprev_prison_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=LTBIprev.prison.lo*100, ymax=LTBIprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=LTBIprev.prison.med*100), color='navyblue') +
  geom_hline(aes(yintercept = 51.61), linetype='dotted', linewidth=1) +
  geom_hline(aes(yintercept = 39.46), linetype='dotted') +
  geom_hline(aes(yintercept = 63.58), linetype='dotted') +
  theme_bw() + labs(x='Year', y='LTBI prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/LTBIprev_prison_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

###### BRAZIL ###### 
country_p <- 'Brazil'
today <- 240603

val.data.brazil <- data.table(time=c(2008,2013,2004,2010,2001),
                             TBprev.prison=c(0.8306, 0.917, 2.5, 0.4, 2.065),
                             TBprev.prison.lo=c(NA, 0.623, NA, NA, NA),
                             TBprev.prison.hi=c(NA, 1.302, NA, NA, NA),
                             LTBIprev.prison=c(73, 
                                               20.837, # weighted for male & female
                                               61.5,
                                               49,
                                               64.5),
                             loc=c('SP','MS','BA','MS','SP'),
                             citation=c('Nogueira.2012','Carbone.2015','Lemos.2009','Estevan.2013','Abrahao.2006')) 

# read in and bind the tb model output matrices
tb_mat <- readRDS(paste0(wd_save, '3-outputs/tb_mat_', gsub(' ', '', tolower(country_p)), '_', today, '.RDS'))
tb_mat_dt <- rbindlist(tb_mat[-which(sapply(tb_mat, is.null))])
length(unique(tb_mat_dt$idx))
setorder(tb_mat_dt, idx, time)

tb_mat_dt$country <- country_p
all_pop_sizes <- rbind(all_pop_sizes, tb_mat_dt[,c('time','N1','N2','N3','N4','idx','country'),
                                                with=F])

tb_mat_dt[, muIshadow := muI1shadow + muI2shadow + muI3shadow + muI4shadow]
tb_mat_dt[, muIshadow.next := shift(.SD, 1, type='lead'), .SDcols='muIshadow', by='idx']
tb_mat_dt[, Ideathrate := (muIshadow.next - muIshadow)/(N1+N2+N3+N4) * 100000]

tb_mat_dt[,`:=`(TBprev.prison = I1/N1,
                LTBIprev.prison = (E1+L1)/N1)]

tb_mat_dt_summary <- tb_mat_dt[!is.na(Ideathrate),
                               .(TBprev.prison.med = median(TBprev.prison),
                                 TBprev.prison.lo = quantile(TBprev.prison, 0.025),
                                 TBprev.prison.hi = quantile(TBprev.prison, 0.975),
                                 LTBIprev.prison.med = median(LTBIprev.prison),
                                 LTBIprev.prison.lo = quantile(LTBIprev.prison, 0.025),
                                 LTBIprev.prison.hi = quantile(LTBIprev.prison, 0.975),
                                 Ideathrate.med = median(Ideathrate),
                                 Ideathrate.lo = quantile(Ideathrate, 0.025),
                                 Ideathrate.hi = quantile(Ideathrate, 0.975)),
                               by='time']
ggplot(tb_mat_dt_summary[time %in% who_data[country == country_p & !is.na(e_mort_100k), year]]) +
  geom_ribbon(aes(x=time, ymin=Ideathrate.lo, ymax=Ideathrate.hi), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=Ideathrate.med), color='navyblue') +
  geom_point(data=who_data[country == country_p & !is.na(e_mort_100k)], aes(x=year, y=e_mort_100k),size=0.5) +
  geom_errorbar(data=who_data[country == country_p & !is.na(e_mort_100k)], 
                aes(x=year, ymin=e_mort_100k_lo, ymax=e_mort_100k_hi), linewidth=0.2) +
  theme_bw() + labs(x='Year', y='TB deaths per 100k') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBdeaths_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=TBprev.prison.lo*100, ymax=TBprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=TBprev.prison.med*100), color='navyblue') +
  geom_point(data=val.data.brazil, aes(x=time, y=TBprev.prison), size=0.5) +
  theme_bw() + labs(x='Year', y='TB prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBprev_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=LTBIprev.prison.lo*100, ymax=LTBIprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=LTBIprev.prison.med*100), color='navyblue') +
  geom_point(data=val.data.brazil, aes(x=time, y=LTBIprev.prison), size=0.5) +
  theme_bw() + labs(x='Year', y='LTBI prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/LTBIprev_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

###### COLOMBIA ###### 
country_p <- 'Colombia'
today <- 240612

val.data.colombia <- data.table(time=c(2015, 2013, 2011),
                              TBprev.prison=c(1.026, 0.24422, NA),
                              TBprev.prison.lo=c(NA,NA,NA),
                              TBprev.prison.hi=c(NA,NA,NA),
                              LTBIprev.prison=c(67.6, NA, 77.6),
                              loc=c('Guaduas','Tolima','Medellin,Itagui'),
                              citation=c('Guerra.2019','Alarcon-Robayo.2016','Rueda.2014'))

# read in and bind the tb model output matrices
tb_mat <- readRDS(paste0(wd_save, '3-outputs/tb_mat_', gsub(' ', '', tolower(country_p)), '_', today, '.RDS'))
tb_mat_dt <- rbindlist(tb_mat[-which(sapply(tb_mat, is.null))])
length(unique(tb_mat_dt$idx))
setorder(tb_mat_dt, idx, time)

tb_mat_dt$country <- country_p
all_pop_sizes <- rbind(all_pop_sizes, tb_mat_dt[,c('time','N1','N2','N3','N4','idx','country'),
                                                with=F])

tb_mat_dt[, muIshadow := muI1shadow + muI2shadow + muI3shadow + muI4shadow]
tb_mat_dt[, muIshadow.next := shift(.SD, 1, type='lead'), .SDcols='muIshadow', by='idx']
tb_mat_dt[, Ideathrate := (muIshadow.next - muIshadow)/(N1+N2+N3+N4) * 100000]

tb_mat_dt[,`:=`(TBprev.prison = I1/N1,
                LTBIprev.prison = (E1+L1)/N1)]

tb_mat_dt_summary <- tb_mat_dt[!is.na(Ideathrate),
                               .(TBprev.prison.med = median(TBprev.prison),
                                 TBprev.prison.lo = quantile(TBprev.prison, 0.025),
                                 TBprev.prison.hi = quantile(TBprev.prison, 0.975),
                                 LTBIprev.prison.med = median(LTBIprev.prison),
                                 LTBIprev.prison.lo = quantile(LTBIprev.prison, 0.025),
                                 LTBIprev.prison.hi = quantile(LTBIprev.prison, 0.975),
                                 Ideathrate.med = median(Ideathrate),
                                 Ideathrate.lo = quantile(Ideathrate, 0.025),
                                 Ideathrate.hi = quantile(Ideathrate, 0.975)),
                               by='time']
ggplot(tb_mat_dt_summary[time %in% who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019, year]]) +
  geom_ribbon(aes(x=time, ymin=Ideathrate.lo, ymax=Ideathrate.hi), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=Ideathrate.med), color='navyblue') +
  geom_point(data=who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019], aes(x=year, y=e_mort_100k),size=0.5) +
  geom_errorbar(data=who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019], 
                aes(x=year, ymin=e_mort_100k_lo, ymax=e_mort_100k_hi), linewidth=0.2) +
  theme_bw() + labs(x='Year', y='TB deaths per 100k') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBdeaths_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=TBprev.prison.lo*100, ymax=TBprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=TBprev.prison.med*100), color='navyblue') +
  geom_point(data=val.data.colombia, aes(x=time, y=TBprev.prison), size=0.5) +
  theme_bw() + labs(x='Year', y='TB prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBprev_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=LTBIprev.prison.lo*100, ymax=LTBIprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=LTBIprev.prison.med*100), color='navyblue') +
  geom_point(data=val.data.colombia, aes(x=time, y=LTBIprev.prison), size=0.5) +
  theme_bw() + labs(x='Year', y='LTBI prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/LTBIprev_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

###### EL SALVADOR ###### 
country_p <- 'El Salvador'
today <- 240605

# read in and bind the tb model output matrices
tb_mat <- readRDS(paste0(wd_save, '3-outputs/tb_mat_', gsub(' ', '', tolower(country_p)), '_', today, '.RDS'))
tb_mat_dt <- rbindlist(tb_mat[-which(sapply(tb_mat, is.null))])
length(unique(tb_mat_dt$idx))
setorder(tb_mat_dt, idx, time)

tb_mat_dt$country <- country_p
all_pop_sizes <- rbind(all_pop_sizes, tb_mat_dt[,c('time','N1','N2','N3','N4','idx','country'),
                                                with=F])

tb_mat_dt[, muIshadow := muI1shadow + muI2shadow + muI3shadow + muI4shadow]
tb_mat_dt[, muIshadow.next := shift(.SD, 1, type='lead'), .SDcols='muIshadow', by='idx']
tb_mat_dt[, Ideathrate := (muIshadow.next - muIshadow)/(N1+N2+N3+N4) * 100000]

tb_mat_dt[,`:=`(TBprev.prison = I1/N1,
                LTBIprev.prison = (E1+L1)/N1)]

tb_mat_dt_summary <- tb_mat_dt[!is.na(Ideathrate),
                               .(TBprev.prison.med = median(TBprev.prison),
                                 TBprev.prison.lo = quantile(TBprev.prison, 0.025),
                                 TBprev.prison.hi = quantile(TBprev.prison, 0.975),
                                 LTBIprev.prison.med = median(LTBIprev.prison),
                                 LTBIprev.prison.lo = quantile(LTBIprev.prison, 0.025),
                                 LTBIprev.prison.hi = quantile(LTBIprev.prison, 0.975),
                                 Ideathrate.med = median(Ideathrate),
                                 Ideathrate.lo = quantile(Ideathrate, 0.025),
                                 Ideathrate.hi = quantile(Ideathrate, 0.975)),
                               by='time']
ggplot(tb_mat_dt_summary[time %in% who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019, year]]) +
  geom_ribbon(aes(x=time, ymin=Ideathrate.lo, ymax=Ideathrate.hi), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=Ideathrate.med), color='navyblue') +
  geom_point(data=who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019], aes(x=year, y=e_mort_100k),size=0.5) +
  geom_errorbar(data=who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019], 
                aes(x=year, ymin=e_mort_100k_lo, ymax=e_mort_100k_hi), linewidth=0.2) +
  theme_bw() + labs(x='Year', y='TB deaths per 100k') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBdeaths_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=TBprev.prison.lo*100, ymax=TBprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=TBprev.prison.med*100), color='navyblue') +
  geom_hline(aes(yintercept = 1.680), linetype='dotted', linewidth=1) +
  geom_hline(aes(yintercept = 0.830), linetype='dotted') +
  geom_hline(aes(yintercept = 2.970), linetype='dotted') +
  theme_bw() + labs(x='Year', y='TB prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBprev_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=LTBIprev.prison.lo*100, ymax=LTBIprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=LTBIprev.prison.med*100), color='navyblue') +
  geom_hline(aes(yintercept = 51.61), linetype='dotted', linewidth=1) +
  geom_hline(aes(yintercept = 39.46), linetype='dotted') +
  geom_hline(aes(yintercept = 63.58), linetype='dotted') +
  theme_bw() + labs(x='Year', y='LTBI prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/LTBIprev_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

###### MEXICO ###### 
country_p <- 'Mexico'
today <- 240611

# read in and bind the tb model output matrices
tb_mat <- readRDS(paste0(wd_save, '3-outputs/tb_mat_', gsub(' ', '', tolower(country_p)), '_', today, '.RDS'))
tb_mat_dt <- rbindlist(tb_mat[-which(sapply(tb_mat, is.null))])
length(unique(tb_mat_dt$idx))
setorder(tb_mat_dt, idx, time)

tb_mat_dt$country <- country_p
all_pop_sizes <- rbind(all_pop_sizes, tb_mat_dt[,c('time','N1','N2','N3','N4','idx','country'),
                                                with=F])

tb_mat_dt[, muIshadow := muI1shadow + muI2shadow + muI3shadow + muI4shadow]
tb_mat_dt[, muIshadow.next := shift(.SD, 1, type='lead'), .SDcols='muIshadow', by='idx']
tb_mat_dt[, Ideathrate := (muIshadow.next - muIshadow)/(N1+N2+N3+N4) * 100000]

tb_mat_dt[,`:=`(TBprev.prison = I1/N1,
                LTBIprev.prison = (E1+L1)/N1)]

tb_mat_dt_summary <- tb_mat_dt[!is.na(Ideathrate),
                               .(TBprev.prison.med = median(TBprev.prison),
                                 TBprev.prison.lo = quantile(TBprev.prison, 0.025),
                                 TBprev.prison.hi = quantile(TBprev.prison, 0.975),
                                 LTBIprev.prison.med = median(LTBIprev.prison),
                                 LTBIprev.prison.lo = quantile(LTBIprev.prison, 0.025),
                                 LTBIprev.prison.hi = quantile(LTBIprev.prison, 0.975),
                                 Ideathrate.med = median(Ideathrate),
                                 Ideathrate.lo = quantile(Ideathrate, 0.025),
                                 Ideathrate.hi = quantile(Ideathrate, 0.975)),
                               by='time']
ggplot(tb_mat_dt_summary[time %in% who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019, year]]) +
  geom_ribbon(aes(x=time, ymin=Ideathrate.lo, ymax=Ideathrate.hi), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=Ideathrate.med), color='navyblue') +
  geom_point(data=who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019], aes(x=year, y=e_mort_100k),size=0.5) +
  geom_errorbar(data=who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019], 
                aes(x=year, ymin=e_mort_100k_lo, ymax=e_mort_100k_hi), linewidth=0.2) +
  theme_bw() + labs(x='Year', y='TB deaths per 100k') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBdeaths_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=TBprev.prison.lo*100, ymax=TBprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=TBprev.prison.med*100), color='navyblue') +
  geom_hline(aes(yintercept = 1.680), linetype='dotted', linewidth=1) +
  geom_hline(aes(yintercept = 0.830), linetype='dotted') +
  geom_hline(aes(yintercept = 2.970), linetype='dotted') +
  theme_bw() + labs(x='Year', y='TB prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBprev_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=LTBIprev.prison.lo*100, ymax=LTBIprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=LTBIprev.prison.med*100), color='navyblue') +
  geom_hline(aes(yintercept = 51.61), linetype='dotted', linewidth=1) +
  geom_hline(aes(yintercept = 39.46), linetype='dotted') +
  geom_hline(aes(yintercept = 63.58), linetype='dotted') +
  theme_bw() + labs(x='Year', y='LTBI prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/LTBIprev_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

###### PERU ###### 
country_p <- 'Peru'
today <- 240612

# read in and bind the tb model output matrices
tb_mat <- readRDS(paste0(wd_save, '3-outputs/tb_mat_', gsub(' ', '', tolower(country_p)), '_', today, '.RDS'))
tb_mat_dt <- rbindlist(tb_mat[-which(sapply(tb_mat, is.null))])
length(unique(tb_mat_dt$idx))
setorder(tb_mat_dt, idx, time)

tb_mat_dt$country <- country_p
all_pop_sizes <- rbind(all_pop_sizes, tb_mat_dt[,c('time','N1','N2','N3','N4','idx','country'),
                                                with=F])

tb_mat_dt[, muIshadow := muI1shadow + muI2shadow + muI3shadow + muI4shadow]
tb_mat_dt[, muIshadow.next := shift(.SD, 1, type='lead'), .SDcols='muIshadow', by='idx']
tb_mat_dt[, Ideathrate := (muIshadow.next - muIshadow)/(N1+N2+N3+N4) * 100000]

tb_mat_dt[,`:=`(TBprev.prison = I1/N1,
                LTBIprev.prison = (E1+L1)/N1)]

tb_mat_dt_summary <- tb_mat_dt[!is.na(Ideathrate),
                               .(TBprev.prison.med = median(TBprev.prison),
                                 TBprev.prison.lo = quantile(TBprev.prison, 0.025),
                                 TBprev.prison.hi = quantile(TBprev.prison, 0.975),
                                 LTBIprev.prison.med = median(LTBIprev.prison),
                                 LTBIprev.prison.lo = quantile(LTBIprev.prison, 0.025),
                                 LTBIprev.prison.hi = quantile(LTBIprev.prison, 0.975),
                                 Ideathrate.med = median(Ideathrate),
                                 Ideathrate.lo = quantile(Ideathrate, 0.025),
                                 Ideathrate.hi = quantile(Ideathrate, 0.975)),
                               by='time']
ggplot(tb_mat_dt_summary[time %in% who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019, year]]) +
  geom_ribbon(aes(x=time, ymin=Ideathrate.lo, ymax=Ideathrate.hi), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=Ideathrate.med), color='navyblue') +
  geom_point(data=who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019], aes(x=year, y=e_mort_100k),size=0.5) +
  geom_errorbar(data=who_data[country == country_p & !is.na(e_mort_100k) & year <= 2019], 
                aes(x=year, ymin=e_mort_100k_lo, ymax=e_mort_100k_hi), linewidth=0.2) +
  theme_bw() + labs(x='Year', y='TB deaths per 100k') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBdeaths_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=TBprev.prison.lo*100, ymax=TBprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=TBprev.prison.med*100), color='navyblue') +
  geom_hline(aes(yintercept = 1.680), linetype='dotted', linewidth=1) +
  geom_hline(aes(yintercept = 0.830), linetype='dotted') +
  geom_hline(aes(yintercept = 2.970), linetype='dotted') +
  theme_bw() + labs(x='Year', y='TB prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/TBprev_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

ggplot(tb_mat_dt_summary) +
  geom_ribbon(aes(x=time, ymin=LTBIprev.prison.lo*100, ymax=LTBIprev.prison.hi*100), fill=alpha('navyblue', 0.3)) +
  geom_line(aes(x=time, y=LTBIprev.prison.med*100), color='navyblue') +
  geom_hline(aes(yintercept = 51.61), linetype='dotted', linewidth=1) +
  geom_hline(aes(yintercept = 39.46), linetype='dotted') +
  geom_hline(aes(yintercept = 63.58), linetype='dotted') +
  theme_bw() + labs(x='Year', y='LTBI prevalence in prison (%)') +
  ggtitle(country_p) + scale_x_continuous(expand = c(0,0))
ggsave(paste0(wd_save, '6-figures/LTBIprev_fit_', gsub(' ','',tolower(country_p)), '_', today, '.pdf'),
       height=2.5, width=3)

# save pop size dt
table(all_pop_sizes$country)
write.csv(all_pop_sizes, paste0(wd_save,'3-outputs/all_pop_sizes.csv'), row.names = F)
