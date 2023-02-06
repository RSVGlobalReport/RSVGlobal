#Deus & Dan (code adapted from Gigi - https://github.com/Gigi112/RSV_tutorials/blob/main/2_RSV_timing_explain/RSV_timing_tutorial.Rmd)
#18/11/2022
#global reemergence of RSV onset, duration and peak


#====================================================================
#====================================================================

plotA = plotly::ggplotly(
  rsv_onset_us_cov %>%
    mutate(Season = covper) %>%
    mutate(country = factor(country, levels = c("National", "Mid West", "North East", "West", "South"))) %>%
    
    ggplot(aes(x = epiwk, y = country, color = Season)) +
    geom_point(size = 4, position = position_dodge(width = 0.2)) + 
    geom_errorbar(aes(xmin = l_epiwk, xmax = u_epiwk), width = 0, size = 2, position = position_dodge(width = 0.2)) +
    scale_x_continuous(breaks = seq(1, 53, 4)) + 
    theme_bw(base_size = 14, base_family = 'Lato') + 
    labs(x = "Epi week", y = "", title = "RSV onset by location & season in the US") +
    theme(legend.position = "bottom", legend.title = element_blank())
)


#====================================================================
#====================================================================

#reshape the onset dataset
us_scatterXY1 <-
rsv_onset_us_cov %>%
  pivot_wider(names_from = covper, values_from = epiwk) %>%
  fill(`preCOVID-19`, .direction = "up") %>%
  filter(!is.na(`2021/22`)) %>%
  mutate(Region = country)

#compute the slope
lm(`2021/22` ~ `preCOVID-19`, data = us_scatterXY1) #intercept: -99.940, slope: 2.989

plotB = plotly::ggplotly(
us_scatterXY1 %>%
ggplot(aes(x = `preCOVID-19`, y = `2021/22`, color = Region), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) + 
  geom_errorbar(aes(ymin = l_epiwk, ymax = u_epiwk), width = 0, size = 0.8, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = -99.940, slope = 2.989, color = "black", linetype = "dashed") +
  scale_x_continuous(breaks = seq(40, 50, 1), limits = c(40,50)) + 
  scale_y_continuous(breaks = seq(20, 53, 3), limits = c(20,50)) + 
  theme_bw(base_size = 14, base_family = 'Lato') + 
  labs(x = "PreCOVID-19 mean onset", y = "RSV onset during 2021/22 season", title = "RSV onset by location in the United States") +
  theme(legend.position = "right", legend.title = element_blank())
)


#====================================================================
#====================================================================

#reshape the onset dataset
us_scatterXY2 <-
  rsv_onset_us_cov %>%
  pivot_wider(names_from = covper, values_from = epiwk) %>%
  fill(`preCOVID-19`, .direction = "up") %>%
  filter(!is.na(`2022/23`)) %>%
  mutate(Region = country)

#compute the slope
lm(`2022/23` ~ `preCOVID-19`, data = us_scatterXY2) #intercept: 2.7678, slope: 0.8076

plotC = plotly::ggplotly(
  us_scatterXY2 %>%
    ggplot(aes(x = `preCOVID-19`, y = `2022/23`, color = Region), position = position_dodge(width = 0.5)) +
    geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) + 
    geom_errorbar(aes(ymin = l_epiwk, ymax = u_epiwk), width = 0, size = 0.8, position = position_dodge(width = 0.5)) +
    geom_abline(intercept = 2.7678, slope = 0.8076, color = "black", linetype = "dashed") +
    scale_x_continuous(breaks = seq(40, 50, 1), limits = c(40,50)) + 
    scale_y_continuous(breaks = seq(20, 53, 3), limits = c(20,50)) + 
    theme_bw(base_size = 14, base_family = 'Lato') + 
    labs(x = "PreCOVID-19 mean onset", y = "RSV onset during 2022/23 season", title = "RSV onset by location in the United States") +
    theme(legend.position = "right", legend.title = element_blank())
)

#====================================================================
#====================================================================

#save plots
htmlwidgets::saveWidget(as_widget(plotA), here("output", "onset_each_region", file = paste0("onsetscatterall_USA.html")))
htmlwidgets::saveWidget(as_widget(plotB), here("output", "onset_each_region", file = paste0("onsetscatter2021_USA.html")))
htmlwidgets::saveWidget(as_widget(plotC), here("output", "onset_each_region", file = paste0("onsetscatter2022_USA.html")))
unlink(paste0(here("output", "onset_each_region", paste0("onsetscatterall_USA_files"))), recursive = TRUE) #delete metadata
unlink(paste0(here("output", "onset_each_region", paste0("onsetscatter2021_USA_files"))), recursive = TRUE) #delete metadata
unlink(paste0(here("output", "onset_each_region", paste0("onsetscatter2022_USA_files"))), recursive = TRUE) #delete metadata
