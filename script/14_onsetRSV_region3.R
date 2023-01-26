#Deus & Dan (code adapted from Gigi - https://github.com/Gigi112/RSV_tutorials/blob/main/2_RSV_timing_explain/RSV_timing_tutorial.Rmd)
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#SOUTH EAST ASIAN REGION
#====================================================================
print(
  rsv_onset_us %>% filter(region == "AMR") %>%
  mutate(epiper = if_else(yr == 2017 | yr == 2018 | yr == 2019, " Mean onset (2017-2019)", 
                          if_else(yr == 2021, "2021", "2022"))) %>%
  group_by(country, epiper) %>%
  summarise(epiavg = mean(epiwk, na.rm = TRUE),
            l_epiavg = mean(l_epiwk, na.rm = TRUE),
            u_epiavg = mean(u_epiwk, na.rm = TRUE)) %>%
  
  ggplot(aes(x = epiavg, y = factor(country, levels = c("National", "Mid West", "North East", "West", "South")), color = epiper)) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) + 
  geom_errorbar(aes(xmin = l_epiavg, xmax = u_epiavg), width = 0, size = 0.8, position = position_dodge(width = 0.2)) +
  scale_x_continuous(breaks = seq(1, 53, 4)) + 
  theme_bw() +
  labs(x = "Epi week", y = "", title = "UNITED STATES OF AMERICA") +
  theme(legend.position = "right", legend.title = element_blank())
)
