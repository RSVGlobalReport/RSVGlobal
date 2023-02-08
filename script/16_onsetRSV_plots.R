#Deus & Dan (code adapted from Gigi - https://github.com/Gigi112/RSV_tutorials/blob/main/2_RSV_timing_explain/RSV_timing_tutorial.Rmd)
#18/11/2022
#global reemergence of RSV onset, duration and peak


#====================================================================
#====================================================================

#subset the correct dataset for plotting
rsv_onset_cov_usa <-
rsv_onset_cov %>%
  filter(country == "West" | country == "North East" | country == "National" | country == "South" | country == "Mid West")

#reshape the onset datasets for scatter plotting
usa_scatterXY <-
  left_join(
    left_join(
      dsPreCov <-
        rsv_onset_cov_usa %>%
        pivot_wider(names_from = covper, values_from = epiwk) %>%
        fill(`preCOVID-19`, .direction = "up") %>%
        mutate(US_region = country) %>% 
        filter(is.na(`2021/22`), is.na(`2022/23`)) %>%
        select(country, `preCOVID-19`, l_epiwk, u_epiwk) %>%
        rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
    
      ds2021_22 <-
        rsv_onset_cov_usa %>%
        pivot_wider(names_from = covper, values_from = epiwk) %>%
        fill(`preCOVID-19`, .direction = "up") %>%
        mutate(US_region = country) %>% 
        select(country, `2021/22`, l_epiwk, u_epiwk) %>%
        filter(!is.na(`2021/22`)) %>%
        rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")
      ),
    
    ds2022_23 <-
      rsv_onset_cov_usa %>%
      pivot_wider(names_from = covper, values_from = epiwk) %>%
      fill(`preCOVID-19`, .direction = "up") %>%
      mutate(US_region = country) %>% 
      select(country, `2022/23`, l_epiwk, u_epiwk) %>%
      filter(!is.na(`2022/23`)) %>%
      rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk")) %>%
  rename("US_region" = "country")

#compute the slope and intercept for 2021/22 an 2022/23 seasons
usa_scatterLM1 <- lm(`2021/22` ~ `preCOVID-19`, data = usa_scatterXY)
usa_scatterLM2 <- lm(`2022/23` ~ `preCOVID-19`, data = usa_scatterXY)

plotA <-
usa_scatterXY %>%
ggplot(aes(x = `preCOVID-19`, y = `2021/22`, color = US_region), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = usa_scatterLM1$coefficients[1], slope = usa_scatterLM1$coefficients[2], color = "black", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  scale_y_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  theme_bw(base_size = 14, base_family = 'Lato') + 
  labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2021/22 season", title = "RSV onset in the United States") +
  theme(legend.position = "none", legend.title = element_blank())

plotB <-
  usa_scatterXY %>%
  ggplot(aes(x = `preCOVID-19`, y = `2022/23`, color = US_region), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) + 
  geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = usa_scatterLM2$coefficients[1], slope = usa_scatterLM2$coefficients[2], color = "black", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  scale_y_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  theme_bw(base_size = 14, base_family = 'Lato') + 
  labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2022/23 season", title = "") +
  theme(legend.position = "right", legend.title = element_blank())


plotC <- plotA + plotB

#save plots
htmlwidgets::saveWidget(as_widget(plotC), here("output", "onset_each_region", file = paste0("onset_scatter_USA.html")))
#htmlwidgets::saveWidget(as_widget(plotC), here("output", "onset_each_region", file = paste0("onsetscatter2022_USA.html")))
unlink(paste0(here("output", "onset_each_region", paste0("onsetscatter2021_USA_files"))), recursive = TRUE) #delete metadata
#unlink(paste0(here("output", "onset_each_region", paste0("onsetscatter2022_USA_files"))), recursive = TRUE) #delete metadata


#====================================================================
#====================================================================


#subset the correct dataset for plotting
rsv_onset_cov_nh <-
  rsv_onset_cov %>%
  filter(country != "West", country != "North East", country != "National", country != "South", country != "Mid West")

#reshape the onset datasets for scatter plotting
nh_scatterXY <-
  left_join(
    left_join(
      dsPreCov <-
        rsv_onset_cov_nh %>%
        pivot_wider(names_from = covper, values_from = epiwk) %>%
        fill(`preCOVID-19`, .direction = "up") %>%
        mutate(US_region = country) %>% 
        filter(is.na(`2021/22`), is.na(`2022/23`)) %>%
        select(country, `preCOVID-19`, l_epiwk, u_epiwk) %>%
        rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
      
      ds2021_22 <-
        rsv_onset_cov_nh %>%
        pivot_wider(names_from = covper, values_from = epiwk) %>%
        fill(`preCOVID-19`, .direction = "up") %>%
        mutate(US_region = country) %>% 
        select(country, `2021/22`, l_epiwk, u_epiwk) %>%
        filter(!is.na(`2021/22`)) %>%
        rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")
    ),
    
    ds2022_23 <-
      rsv_onset_cov_nh %>%
      pivot_wider(names_from = covper, values_from = epiwk) %>%
      fill(`preCOVID-19`, .direction = "up") %>%
      mutate(US_region = country) %>% 
      select(country, `2022/23`, l_epiwk, u_epiwk) %>%
      filter(!is.na(`2022/23`)) %>%
      rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk")) 

#compute the slope and intercept for 2021/22 an 2022/23 seasons
nh_scatterLM1 <- lm(`2021/22` ~ `preCOVID-19`, data = nh_scatterXY)
nh_scatterLM2 <- lm(`2022/23` ~ `preCOVID-19`, data = nh_scatterXY)

plotA <-
  nh_scatterXY %>%
  ggplot(aes(x = `preCOVID-19`, y = `2021/22`, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = nh_scatterLM1$coefficients[1], slope = nh_scatterLM1$coefficients[2], color = "black", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  scale_y_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  theme_bw(base_size = 14, base_family = 'Lato') + 
  labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2021/22 season", title = "RSV onset by in Europe") +
  theme(legend.position = "none", legend.title = element_blank())

plotB <-
  nh_scatterXY %>%
  ggplot(aes(x = `preCOVID-19`, y = `2022/23`, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) + 
  geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = nh_scatterLM2$coefficients[1], slope = nh_scatterLM2$coefficients[2], color = "black", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  scale_y_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  theme_bw(base_size = 14, base_family = 'Lato') + 
  labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2022/23 season", title = "") +
  theme(legend.position = "right", legend.title = element_blank())

plotC <- plotA + plotB

#save plots
htmlwidgets::saveWidget(as_widget(plotC), here("output", "onset_each_region", file = paste0("onset_scatter_USA.html")))
#htmlwidgets::saveWidget(as_widget(plotC), here("output", "onset_each_region", file = paste0("onsetscatter2022_USA.html")))
unlink(paste0(here("output", "onset_each_region", paste0("onsetscatter2021_USA_files"))), recursive = TRUE) #delete metadata
#unlink(paste0(here("output", "onset_each_region", paste0("onsetscatter2022_USA_files"))), recursive = TRUE) #delete metadata


#====================================================================
#====================================================================


#subset the correct dataset for plotting
rsv_onset_trop <-
  rsv_onset_cov %>%
  filter(country != "West", country != "North East", country != "National", country != "South", country != "Mid West")

#reshape the onset datasets for scatter plotting
sh_scatterXY <-
  left_join(
    left_join(
      dsPreCov <-
        rsv_onset_trop %>%
        pivot_wider(names_from = covper, values_from = epiwk) %>%
        fill(`preCOVID-19`, .direction = "up") %>%
        mutate(US_region = country) %>% 
        filter(is.na(`2021`), is.na(`2022`)) %>%
        select(country, `preCOVID-19`, l_epiwk, u_epiwk) %>%
        rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
      
      ds2021_22 <-
        rsv_onset_trop %>%
        pivot_wider(names_from = covper, values_from = epiwk) %>%
        fill(`preCOVID-19`, .direction = "up") %>%
        mutate(US_region = country) %>% 
        select(country, `2021`, l_epiwk, u_epiwk) %>%
        filter(!is.na(`2021`)) %>%
        rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")
    ),
    
    ds2022_23 <-
      rsv_onset_trop %>%
      pivot_wider(names_from = covper, values_from = epiwk) %>%
      fill(`preCOVID-19`, .direction = "up") %>%
      mutate(US_region = country) %>% 
      select(country, `2022`, l_epiwk, u_epiwk) %>%
      filter(!is.na(`2022`)) %>%
      rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk")) 

#compute the slope and intercept for 2021/22 an 2022/23 seasons
sh_scatterLM1 <- lm(`2021/22` ~ `preCOVID-19`, data = sh_scatterXY)
sh_scatterLM2 <- lm(`2022/23` ~ `preCOVID-19`, data = sh_scatterXY)

plotA <-
  sh_scatterXY %>%
  ggplot(aes(x = `preCOVID-19`, y = `2021`, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = nh_scatterLM1$coefficients[1], slope = nh_scatterLM1$coefficients[2], color = "black", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  scale_y_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  theme_bw(base_size = 14, base_family = 'Lato') + 
  labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2021 season", title = "RSV onset by in Europe") +
  theme(legend.position = "none", legend.title = element_blank())

plotB <-
  sh_scatterXY %>%
  ggplot(aes(x = `preCOVID-19`, y = `2022/23`, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) + 
  geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = nh_scatterLM2$coefficients[1], slope = nh_scatterLM2$coefficients[2], color = "black", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  scale_y_continuous(breaks = seq(0, 52, 4), limits = c(20,52)) + 
  theme_bw(base_size = 14, base_family = 'Lato') + 
  labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2022/23 season", title = "") +
  theme(legend.position = "right", legend.title = element_blank())

plotC <- plotA + plotB

#save plots
htmlwidgets::saveWidget(as_widget(plotC), here("output", "onset_each_region", file = paste0("onset_scatter_USA.html")))
#htmlwidgets::saveWidget(as_widget(plotC), here("output", "onset_each_region", file = paste0("onsetscatter2022_USA.html")))
unlink(paste0(here("output", "onset_each_region", paste0("onsetscatter2021_USA_files"))), recursive = TRUE) #delete metadata
#unlink(paste0(here("output", "onset_each_region", paste0("onsetscatter2022_USA_files"))), recursive = TRUE) #delete metadata
