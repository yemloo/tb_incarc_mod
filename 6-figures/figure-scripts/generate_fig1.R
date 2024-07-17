map <- data.table(map_data('world'))
map <- map[region %in% unique(c(wpb.data.interp$Country, analyzed_countries))]
# map
map_plot <- ggplot(map, aes(map_id=region)) + 
  geom_map(map=map, fill='light grey') +
  geom_map(data=map[region %in% analyzed_countries],
           map=map[region %in% analyzed_countries], 
           aes(fill=region)) +
  expand_limits(x = map$long, y = map$lat) +
  theme_void() +
  scale_fill_jama() +
  labs(fill='Country') +
  theme(legend.position = c(0.28,0.35))

# incarc prev
incarc <- ggplot() +
  geom_line(data=wpb.data.interp[Year >= 1990 & Year <= 2019], 
            aes(x=Year, y=`Incarceration Prevalence`, color=Country, group=Country),
            color='grey') +
  geom_line(data=latam_incarc_data[Country %in% analyzed_countries &
                                     !is.na(`Incarceration Prevalence`) &
                                              Year >= 1990 & Year <= 2019],
            aes(x=Year, y=`Incarceration Prevalence`, color=Country, group=Country), 
            linewidth=0.8) +
  theme_classic() + scale_color_jama() + 
  scale_x_continuous(expand=c(0,0)) + 
  labs(color='Country',
       x='Year',
       y='Incarceration prevalence per 100 000')

# total TB notifs
totalTB <- ggplot(who_data[country %in% wpb.data.interp$Country & year >= 1990 & year <= 2019 &
                  !is.na(c_newinc_per100k),], 
       aes(x=year, y=c_newinc_per100k, group=country)) + 
  geom_line(color='light grey') +
  geom_line(data=who_data[country %in% analyzed_countries & year >= 1990 & year <= 2019 &
                            !is.na(c_newinc_per100k)],
            aes(x=year, y=c_newinc_per100k, color=country, group=country), size=0.8) +
  theme_classic() + scale_color_jama() + 
  scale_x_continuous(expand=c(0,0)) + 
  labs(color='Country',
       x='Year',
       y='Total TB notification rate per 100 000')

# prison TB notifs
prisonTB <- ggplot(prison_estimates[Country %in% wpb.data.interp$Country & Year <= 2019 &
                          !is.na(notifs_prison_final) &
                          !is.na(NR_Obs_New),], 
       aes(x=Year, y=NR_Obs_New*100000, group=Country)) + 
  geom_line(color='light grey') +
  geom_line(data=prison_estimates[Country %in% analyzed_countries & Year <= 2019 &
                                    !is.na(notifs_prison_final) & 
                                    !is.na(NR_Obs_New),],
            aes(x=Year, y=NR_Obs_New*100000, color=Country, group=Country), size=0.8) +
  theme_classic() + scale_color_jama() +
  scale_x_continuous(expand=c(0,0)) + 
  labs(color='Country',
       x='Year',
       y='Prison TB notification rate per 100 000')

plot_grid(
  plot_grid(map_plot + theme(plot.margin = margin(0,0,0,6)),
            prisonTB + theme(legend.position="none"), 
            nrow=2),
  plot_grid(incarc + theme(legend.position="none"),
            totalTB + theme(legend.position="none"),
            align='vh',
            axis='lr',
            nrow=2),
  ncol=2)
ggsave(paste0(wd, '6-figures/figure1.pdf'), height=6, width=6)
