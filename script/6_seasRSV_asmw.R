#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#ASMW
#====================================================================

#weekly year on year RSV cases in Africa/South East Asia/Middle East/Western pacific
print(
rsv_asmw %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, ncol = 3, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
  theme(strip.background = element_rect(fill = "light blue")) +
  labs(title = "Weekly RSV cases in African/SEAR/ME/WPR countries, 2018-2022", x = "Date", y = "RSV cases")
)

#weekly seasonal RSV dynamics before and after COVID-19 by regions for each year
print(
  rsv_asmw %>%
    filter(yr !=2020) %>%
    group_by(country, yr, wk) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>% #sum up weekly cases to monthly for each year
    ungroup() %>%
    
    ggplot(aes(x = wk, y = cases, group = yr, color = factor(yr))) +
    geom_line(size = 1) + 
    scale_x_continuous(breaks = seq(1, 52, 4)) +
    facet_wrap(. ~ country, ncol = 3, scales = "free_y") +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    labs(title = "Weekly seasonal dynamics of RSV cases by African/SEAR/ME/WPR country & year", x = "Week (Jan-Dec)", y = "RSV cases") + 
    theme(legend.position = "bottom", strip.background = element_rect(fill = "light blue")) +
    guides(color = guide_legend(title = ""))
)

#weekly seasonal RSV dynamics before and after COVID-19 by regions aggregated across all years
print(
  rsv_asmw %>%
    mutate(covid = if_else(date < "2020-01-01", "PreCOVID-19 (2018-19)", 
                           if_else(date >= "2021-01-01" , "PostCOVID-19 (2021-22)", NA_character_))) %>%
    filter(!is.na(covid)) %>%
    group_by(country, wk, covid) %>%
    summarise(mcases = mean(cases, rm.na = TRUE)) %>%
    ungroup() %>%
    
    ggplot(aes(x = wk, y = mcases, group = covid, color = covid)) +
    geom_line(size = 1) + 
    scale_x_continuous(breaks = seq(1, 52, 4)) +
    facet_wrap(. ~ country, ncol = 3, scales = "free_y") +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    labs(title = "Weekly seasonal dynamics of RSV cases by country & COVID-19 phase", x = "Week (Jan-Dec)", y = "RSV cases") + 
    theme(legend.position = "bottom", strip.background = element_rect(fill = "light blue")) +
    guides(color = guide_legend(title = "Reporting period"))
)
