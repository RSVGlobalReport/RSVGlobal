#Deus & Dan (code adapted from Gigi - https://github.com/Gigi112/RSV_tutorials/blob/main/2_RSV_timing_explain/RSV_timing_tutorial.Rmd)
#18/11/2022
#global reemergence of RSV onset, duration and peak

#combine datasets for plotting RSV timing (onset) by hemisphere 

#====================================================================
#====================================================================

#SOUTHERN HEMISPHERE NORMAL VS 2021
r3 <-
  rsv_onset %>% filter(hemi == "SH") %>%
    mutate(epiper = if_else(yr == 2017 | yr == 2018 | yr == 2019, " Mean onset (2017-19)", 
                            if_else(yr == 2021, "2021", NULL))) %>%
  filter(!is.na(epiper)) %>%
    group_by(country, epiper) %>%
    summarise(epiavg = mean(epiwk, na.rm = TRUE),
              l_epiavg = mean(l_epiwk, na.rm = TRUE),
              u_epiavg = mean(u_epiwk, na.rm = TRUE)) %>%
    
  ggplot(aes(x = epiavg, y = country, color = epiper)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(xmin = l_epiavg, xmax = u_epiavg), width = 0, size = 0.8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = seq(1, 52, 4), limits = c(1,52)) + 
  theme_bw() +
  labs(x = "Epidemiological week", y = "", title = "SOUTHERN HEMISPHERE") +
  theme(legend.position = "bottom", legend.title = element_blank())

#====================================================================
#====================================================================

#SOUTHERN HEMISPHERE NORMAL VS 2022
r4 <-
  rsv_onset %>% filter(hemi == "SH") %>%
  mutate(epiper = if_else(yr == 2017 | yr == 2018 | yr == 2019, " Mean onset (2017-19)", 
                          if_else(yr == 2022, "2022", NULL))) %>%
  filter(!is.na(epiper)) %>%
  group_by(country, epiper) %>%
  summarise(epiavg = mean(epiwk, na.rm = TRUE),
            l_epiavg = mean(l_epiwk, na.rm = TRUE),
            u_epiavg = mean(u_epiwk, na.rm = TRUE)) %>%
  
  ggplot(aes(x = epiavg, y = country, color = epiper)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(xmin = l_epiavg, xmax = u_epiavg), width = 0, size = 0.8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = seq(1, 52, 4), limits = c(1,52)) + 
  theme_bw() +
  labs(x = "Epidemiological week", y = "", title = "SOUTHERN HEMISPHERE") +
  theme(legend.position = "bottom", legend.title = element_blank())

#====================================================================
#====================================================================

print(r3 + r4)

