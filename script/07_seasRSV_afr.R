#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#====================================================================

#weekly year on year RSV cases
print(
  rsv_asmw %>%
    dplyr::filter(country %in% c("Central African Republic", "Côte d'Ivoire", "Madagascar", "South Africa")) %>% 
    dplyr::group_by(country) %>%
    dplyr::mutate(cases = zoo::rollmean(cases, k = 3, fill = NA, align = 'right')) %>%
    dplyr::ungroup() %>%
    
    ggplot(aes(x = zoo::as.yearmon(date, "%b %y"), y = cases)) +
    geom_line() + 
    facet_wrap(. ~ country, ncol = 2, scales = "free_y") +
    theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
    theme(strip.background = element_rect(fill = "light yellow")) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
    labs(title = "14-days rolling average RSV cases, 2017-2022", subtitle = "(Stratified by African country)", x = "Date", y = "RSV cases")
)

#====================================================================
#====================================================================

#weekly seasonal RSV dynamics for each year
print(
rsv_asmw %>%
  dplyr::filter(country %in% c("Central African Republic", "Côte d'Ivoire", "Madagascar", "South Africa")) %>% 
  ggplot(aes(x = wk, y = cases, group = yr, color = factor(yr))) +
  geom_line(size = 1) +
  facet_wrap(. ~ country, ncol = 2, scales = "free_y") +
  labs(title = "Weekly seasonal RSV cases", subtitle = "(Stratified by African country & year)", x = "Week", y = "RSV cases")  +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = seq(1, 53, 4)) +
  theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow"))
)

#====================================================================
#====================================================================

#weekly seasonal RSV dynamics before and after COVID-19 by regions aggregated across all years
print(
rsv_asmw %>%
  dplyr::filter(country %in% c("Central African Republic", "Côte d'Ivoire", "Madagascar", "South Africa")) %>%
  mutate(covid = if_else(date < "2020-01-01", "Pre-C19 (2017-19)", if_else(date >= "2021-01-01" , "Post-C19 (2021-22)", NA_character_))) %>%
  filter(!is.na(covid)) %>%
  group_by(country, wk, covid) %>%
  summarise(mcases = mean(cases, rm.na = TRUE)) %>%
  ungroup() %>%
  
  ggplot(aes(x = wk, y = mcases, group = covid, color = covid)) +
  geom_line(size = 1) + 
  scale_x_continuous(breaks = seq(1, 53, 4)) +
  facet_wrap(. ~ country, ncol = 2, scales = "free_y") +
  theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(title = "Weekly seasonal RSV cases", subtitle = "(Stratified by African country & Covid-19 phase)", x = "Week", y = "RSV cases") + 
  theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow")) +
  guides(color = guide_legend(title = ""))
)
  