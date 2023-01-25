#Deus & Dan (code adapted from Gigi - https://github.com/Gigi112/RSV_tutorials/blob/main/2_RSV_timing_explain/RSV_timing_tutorial.Rmd)
#18/11/2022
#global reemergence of RSV onset, duration and peak

#combine datasets for plotting RSV timing (onset) by region 

#====================================================================
#====================================================================

#AFRICAN REGION
p1 <-
  rsv_onset %>% filter(region == "AFR") %>%
    mutate(epiper = if_else(yr == 2017 | yr == 2018 | yr == 2019, " Mean onset (2017-19)", 
                            if_else(yr == 2021, "2021", "2022"))) %>%
    group_by(country, epiper) %>%
    summarise(epiavg = mean(epiwk, na.rm = TRUE),
              l_epiavg = mean(l_epiwk, na.rm = TRUE),
              u_epiavg = mean(u_epiwk, na.rm = TRUE)) %>%
    
  ggplot(aes(x = epiavg, y = country, color = epiper)) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) + 
  geom_errorbar(aes(xmin = l_epiavg, xmax = u_epiavg), width = 0, size = 0.8, position = position_dodge(width = 0.2)) +
  scale_x_continuous(breaks = seq(1, 52, 4)) + 
  theme_bw() +
  labs(x = "Epidemiological week", y = "", title = "AFRICA") +
  theme(legend.position = "none")

#====================================================================
#====================================================================

#AMERICAS REGION
p2 <-
  rsv_onset %>% filter(region == "AMR") %>%
  mutate(epiper = if_else(yr == 2017 | yr == 2018 | yr == 2019, " Mean onset (2017-19)", 
                          if_else(yr == 2021, "2021", "2022"))) %>%
  group_by(country, epiper) %>%
  summarise(epiavg = mean(epiwk, na.rm = TRUE),
            l_epiavg = mean(l_epiwk, na.rm = TRUE),
            u_epiavg = mean(u_epiwk, na.rm = TRUE)) %>%
  
  ggplot(aes(x = epiavg, y = country, color = epiper)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(xmin = l_epiavg, xmax = u_epiavg), width = 0, size = 0.8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = seq(1, 52, 4)) + 
  theme_bw() +
  labs(x = "Epidemiological week", y = "", title = "AMERICAS") +
  theme(legend.position = "bottom", legend.title = element_blank())

#====================================================================
#====================================================================

#EASTERN MEDITERRANEAN REGION
p3 <-
  rsv_onset %>% filter(region == "EMR") %>%
  mutate(epiper = if_else(yr == 2017 | yr == 2018 | yr == 2019, " Mean onset (2017-19)", 
                          if_else(yr == 2021, "2021", "2022"))) %>%
  group_by(country, epiper) %>%
  summarise(epiavg = mean(epiwk, na.rm = TRUE),
            l_epiavg = mean(l_epiwk, na.rm = TRUE),
            u_epiavg = mean(u_epiwk, na.rm = TRUE)) %>%
  
  ggplot(aes(x = epiavg, y = country, color = epiper)) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) + 
  geom_errorbar(aes(xmin = l_epiavg, xmax = u_epiavg), width = 0, size = 0.8, position = position_dodge(width = 0.2)) +
  scale_x_continuous(breaks = seq(1, 52, 4)) + 
  theme_bw() +
  labs(x = "Epidemiological week", y = "", title = "EASTERN MEDITERRANEAN") +
  theme(legend.position = "none") 

#====================================================================
#====================================================================

print(p1 + p2 + p3)




