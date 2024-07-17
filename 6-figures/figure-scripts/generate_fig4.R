# this script generates each part of fig 4, which we assemble in PPT
all_future_temp <- data.table(all_future)
all_future_med_temp <- data.table(all_future_med)

all_future_temp$scenario <- factor(all_future_temp$scenario, levels = rev(c('Stable',
                                                                        'Continual Growth',
                                                              'Reduce entries 25%',
                                                              'Reduce entries 50%',
                                                              'Reduce duration 25%',
                                                              'Reduce duration 50%',
                                                              'Both 25%',
                                                              'Both 50%')),
                              labels = rev(c('Stable',
                                         'Continue trends',
                                         'Reduce entries 25%',
                                         'Reduce entries 50%',
                                         'Reduce duration 25%',
                                         'Reduce duration 50%',
                                         'Both 25%',
                                         'Both 50%')))

all_future_med_temp$scenario <- factor(all_future_med_temp$scenario, levels = rev(c('Stable',
                                                                          'Continual Growth',
                                                                      'Reduce entries 25%',
                                                                      'Reduce entries 50%',
                                                                      'Reduce duration 25%',
                                                                      'Reduce duration 50%',
                                                                      'Both 25%',
                                                                      'Both 50%')),
                                  labels = rev(c('Stable',
                                                 'Continue trends',
                                             'Reduce entries 25%',
                                             'Reduce entries 50%',
                                             'Reduce duration 25%',
                                             'Reduce duration 50%',
                                             'Both 25%',
                                             'Both 50%')))
pal <- c('Stable'='#454546',
         'Continue trends'='#B5B5B5',
         'Reduce entries 25%'='#EED37F', 
         'Reduce entries 50%'='#DC9D20',
         'Reduce duration 25%'='#9ecac0',
         'Reduce duration 50%'='#638e9c',
         'Both 25%'='#c1b0d0',
         'Both 50%'='#916E9E')

ggplot(all_future_med_temp[variable == 'pop' & time >= 2013 & time <= 2034 &
                        country != 'El Salvador']) +
  geom_line(aes(x=time, y=rate_per100k, color=scenario),linewidth=0.8) +
  geom_point(aes(x=time, y=rate_per100k*1.03), alpha=0) +
  geom_hline(data = pop_main_dt_summary[pop_hist == 'Current Incarc.' & time == 1990 & 
                                          scenario == 'Leveled Off' & country != 'El Salvador'],
             aes(yintercept=rate_per100k), linetype='dashed') +
  facet_wrap(~country, nrow=1) +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0), limits = c(0,NA)) + 
  theme_classic() +
  scale_color_manual(values=pal, breaks=rev(levels(all_future_med_temp$scenario))) +
  labs(x='Year', y='Incarceration prevalence per 100 000', color='') +
  theme(strip.text = element_text(size=12), legend.position = 'top',
        legend.text = element_text(size=10)) + 
  guides(color = guide_legend(nrow = 1))
ggsave(paste0(wd, '6-figures/figure4a_240713.pdf'), height=3.5, width=12)

all_future_2034 <- all_future_temp[variable == 'I' & time == 2034 & !scenario %in% c('Stable')]
min_dt <- all_future_2034[,.(y_min=quantile(perc_change_from_stable_rate_per100k, 0.25)-1.5*IQR(perc_change_from_stable_rate_per100k),
                             y_max=quantile(perc_change_from_stable_rate_per100k, 0.75)+1.5*IQR(perc_change_from_stable_rate_per100k)),
           by=c('scenario','country')] # we don't want to plot outliers, but they will still affect the ylimits - so find the boxplot min for each group and omit outliers below the min
all_future_2034 <- merge(all_future_2034, min_dt, by=c('scenario','country'))
ggplot(all_future_2034[perc_change_from_stable_rate_per100k >= y_min &
                         perc_change_from_stable_rate_per100k <= y_max & 
                         country != 'El Salvador']) +
  geom_boxplot(aes(x=scenario, y=perc_change_from_stable_rate_per100k, 
                   fill=scenario), 
               outlier.shape = NA) +
  geom_hline(aes(yintercept=0), linetype = 'dotted') +
  facet_wrap(~country, scales='free_x',nrow=1) +
  theme_classic() + 
  scale_fill_manual(values=alpha(pal, 1)) +
  theme(axis.text.x=element_text(hjust=1, angle=45),
        legend.position = 'none',
  strip.text = element_text(size=12)) +
  labs(x=NULL, y='% difference in population TB incidence',
       fill='Scenario') +
  coord_flip()
ggsave(paste0(wd, '6-figures/figure4b_240713.pdf'), height=3, width=11.5)

# El Salvador
elsal_future_pop_med$scenario <- factor(elsal_future_pop_med$scenario,
                                         levels=rev(c('Continue state of emergency',
                                                      'Gradual passive abatement',
                                                      'Active 10-year reversion',
                                                      'Active 5-year reversion',
                                                      'Active 2-year reversion')),
                                         labels=rev(c('Continue state of emergency',
                                                      'Gradual passive abatement',
                                                      'Active 10-year reversion',
                                                      'Active 5-year reversion',
                                                      'Active 2-year reversion')))
elsal_future_pop_med$country = 'El Salvador'
elsal_future$scenario <- factor(elsal_future$scenario,
                                 levels=rev(c('Continue state of emergency',
                                              'Gradual passive abatement',
                                              'Active 10-year reversion',
                                              'Active 5-year reversion',
                                              'Active 2-year reversion')),
                                 labels=rev(c('Continue state of emergency',
                                              'Gradual passive abatement',
                                              'Active 10-year reversion',
                                              'Active 5-year reversion',
                                              'Active 2-year reversion')))
elsal_future$country <- 'El Salvador'

ggplot(elsal_future_pop_med[time >= 2013 & time <= 2034]) +
  geom_line(aes(x=time, y=rate_per100k, color=scenario),linewidth=0.8) +
  geom_point(aes(x=time, y=rate_per100k*1.03), alpha=0) +
  theme_classic() + 
  scale_color_manual(values=get_palette('nejm', 8)[c(1,3,7,4,2,5,7)],
                     breaks=rev(levels(elsal_future_pop_med$scenario))) + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0), limits = c(0,NA)) +
  geom_hline(aes(yintercept=pop_main_dt_summary[time == 1990 & pop_hist == 'Current Incarc.' &
                                                  scenario=='Leveled Off' & 
                                                  country == 'El Salvador',rate_per100k]), linetype = 'dashed') +
  labs(x='Year', y='Incarceration prevalence per 100 000',color='') +
  facet_wrap(~country) +
  theme(legend.spacing.y = unit(0.4, 'cm'),
        legend.box.just = 'right',
        strip.text = element_text(size=12)) +
  guides(color = guide_legend(byrow = TRUE))
ggsave(paste0(wd, '6-figures/figure4c_left_240713.pdf'), height=3, width=7.5)

elsal_future_2034 <- elsal_future[variable == 'I' & time == 2034]
min_dt <- elsal_future_2034[,.(y_min=quantile(perc_change_from_2021_rate_per100k, 0.25)-1.5*IQR(perc_change_from_2021_rate_per100k),
                             y_max=quantile(perc_change_from_2021_rate_per100k, 0.75)+1.5*IQR(perc_change_from_2021_rate_per100k)),
                          by=c('scenario','country')] # we don't want to plot outliers, but they will still affect the ylimits - so find the boxplot min for each group and omit outliers below the min
elsal_future_2034 <- merge(elsal_future_2034, min_dt, by=c('scenario','country'))
ggplot(elsal_future_2034[perc_change_from_2021_rate_per100k >= y_min &
                           perc_change_from_2021_rate_per100k <= y_max]) +
  geom_boxplot(aes(x=scenario, y=perc_change_from_2021_rate_per100k, fill=scenario), 
               outlier.shape = NA) +
  geom_hline(aes(yintercept=0), linetype = 'dotted') +
  theme_classic() + 
  scale_fill_manual(values=get_palette('nejm', 8)[c(1,3,7,4,2,5,7)],
                    breaks=rev(levels(elsal_future_pop_med$scenario))) +
  facet_wrap(~country) +
  theme(legend.position = 'none',strip.text = element_text(size=12)) +
  coord_flip() +
  labs(x=NULL, y='% change in population TB incidence since 2021',fill='Scenario')
ggsave(paste0(wd, '6-figures/figure4c_right_240713.pdf'), height=3, width=6.5)
