fig2a <- ggplot() +
  geom_ribbon(data=obs_v_counter_medians[variable == 'I' & pop == 'Combined' & time < 2020],
              aes(x=time, ymin=rate_per100k.counterfct.lo, 
                  ymax=rate_per100k.counterfct.hi, fill='No Change Since 1990'), alpha=0.2) +
  geom_ribbon(data=obs_v_counter_medians[variable == 'I' & pop == 'Combined' & time < 2020],
              aes(x=time, ymin=rate_per100k.observed.lo, 
                  ymax=rate_per100k.observed.hi, fill='Observed'), alpha=0.3) +
  geom_line(data=obs_v_counter_medians[variable == 'I' & pop == 'Combined' & time < 2020],
            aes(x=time, y=rate_per100k.counterfct, color='No Change Since 1990'), linewidth=1) +
  geom_line(data=obs_v_counter_medians[variable == 'I' & pop == 'Combined' & time < 2020] ,
            aes(x=time, y=rate_per100k.observed, color='Observed'), linewidth=1) +
  geom_point(data=who_data[country %in% analyzed_countries & year <= 2019 & year >= 1990],
             aes(x=year, y=e_inc_100k), size=0.2, color='black') +
  facet_wrap(~country, scales = 'free', nrow=1) +
  scale_color_manual(values=get_palette('jama',2),
                     limits=c('Observed','No Change Since 1990')) +
  scale_fill_manual(values=get_palette('jama',2),
                    limits=c('Observed','No Change Since 1990')) +
  theme_classic() + scale_x_continuous(expand=c(0,0)) +
  labs(x='Year',
       y='Pop. TB incidence/100 000',
       color='Incarceration Scenario',
       fill='Incarceration Scenario')

fig2b <- ggplot() +
  geom_hline(yintercept = 0, color=get_palette('jama',2)[2], linewidth=1) +
  geom_ribbon(data=obs_v_counter_medians[variable == 'I' & time < 2020 & pop == 'Combined'],
              aes(x=time, ymin=RD.obs.v.counter.lo, ymax=RD.obs.v.counter.hi), fill=alpha(get_palette('jama',2)[1],0.2), color=NA) + 
  geom_line(data=obs_v_counter_medians[variable == 'I' & time < 2020 & pop == 'Combined'],
            aes(x=time, y=RD.obs.v.counter), color=get_palette('jama',2)[1], linewidth=1) +
  facet_wrap(~country, scales = 'free', nrow=1) + scale_color_jama() +
  theme_classic() + scale_x_continuous(expand=c(0,0)) +
  labs(fill='',
       x='Year',
       y='Excess TB cases/100 000')

mypal <- c('#342E4F','#713E5E','#ab7d8c','#e3d6b3','darkgrey','#C5A07D')

fig2c <- ggplot() +
  geom_area(data=obs_v_counter_medians[variable == 'I' & time < 2020 & pop == 'Combined'],
            aes(x=time, y=RD.obs.v.counter, fill='Never Incarcerated'), linewidth=1) +
  geom_area(data=sum_incarc_history_medians[variable == 'I' & time < 2020],
            aes(x=time, y=RD.obs.v.counter.adj, fill='Previously Incarcerated')) +
  geom_area(data=obs_v_counter_medians[variable == 'I' & time < 2020 & pop == 'Prison'],
            aes(x=time, y=RD.obs.v.counter, fill='In Prison (Undetected)')) +
  geom_area(data=obs_v_counter_medians[variable == 'D' & time < 2020 & pop == 'Prison'],
            aes(x=time, y=RD.obs.v.counter, fill='In Prison (Notified)')) +
  geom_line(data=obs_v_counter_medians[variable == 'I' & time < 2020 & pop == 'Combined'],
            aes(x=time, y=RD.obs.v.counter), color='black', linewidth=0.5) +
  facet_wrap(~country, scales = 'free_y', nrow=1) + 
  scale_fill_manual(values=mypal,
                    limits=c('In Prison (Notified)','In Prison (Undetected)','Previously Incarcerated','Never Incarcerated')) +
  theme_classic() + scale_x_continuous(expand=c(0,0)) +
  labs(fill='Status of Person with TB',
       x='Year',
       y='Excess TB cases/100 000')

leg_ab <- get_legend(fig2a + guides(color = guide_legend(nrow = 1)) +
                    theme(legend.position = "top"))
leg_c <- get_legend(fig2c + guides(color = guide_legend(nrow = 1)) +
                      theme(legend.position = "bottom"))

fig2 <- plot_grid(
  fig2a + theme(legend.position="none", plot.margin = margin(0.4, 0.2, 0.2, 0.2, 'cm')),
  fig2b + theme(legend.position="none", plot.margin = margin(0.4, 0.2, 0.2, 0.2, 'cm')),
  fig2c + theme(legend.position="none", plot.margin = margin(0.4, 0.2, 0.2, 0.2, 'cm')),
  align = 'vh',
  labels = c("A", "B", "C"),
  hjust = -1.5,
  vjust = 0.3,
  nrow = 3
)

plot_grid(leg_ab, fig2, leg_c, ncol = 1, rel_heights = c(.3, 4.5, .3))
ggsave(paste0(wd, '6-figures/figure2_240702.pdf'), height=6.5, width=10.25)
