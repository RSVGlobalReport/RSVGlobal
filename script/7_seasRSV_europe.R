#By Deus Thindwa
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================

#year on year weekly RSV cases in European countries
print(
rsv_euro %>%
  ggplot(aes(x = date(date), y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
  labs(title = "Weekly RSV cases in European countries", x = "MMWR Date", y = "RSV cases")
)

#year on year monthly RSV cases in European countries
print(
rsv_euro %>%
  arrange(date, country) %>%
  group_by(date = round_date(date, "month"), country) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
  labs(title = "Monthly RSV cases in European countries", x = "MMWR Date", y = "RSV cases")
)

#seasonal RSV dynamics before and after COVID-19 and mean timing of peak in European countries
print(
  rsv_euro %>%
    mutate(covid = if_else(date < "2020-01-01", "PreCOVID-19", 
                           if_else(date >= "2021-01-01" , "PostCOVID-19", NA_character_)),
           mon = month(date, label = TRUE, abbr = TRUE)) %>%
    filter(!is.na(covid)) %>%
    arrange(date, country) %>%
    
    group_by(country, date = round_date(date, "month"), mon, covid) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>% #sum up weekly cases to monthly for each year
    ungroup() %>%
    
    group_by(country, mon, covid) %>%
    summarise(mcases = mean(cases, rm.na = TRUE)) %>% #average monthly cases across years
    ungroup() %>%
    
    group_by(country, covid) %>%
    mutate(pcases = mcases/sum(mcases, na.rm = TRUE)) %>% #compute share of averaged cases in each month
    ungroup() %>%
    
    ggplot(aes(x = factor(mon, levels(factor(mon))[c(6,7,8,9,10,11,12,1,2,3,4,5)]), y = pcases, group = covid, color = covid)) +
    geom_line(size = 1) + 
    scale_y_continuous(breaks = seq(0, 1, 0.10), labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(. ~ country, scales = "free_y") +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    labs(title = "Seasonal dynamics of RSV cases in European countries", x = "Months", y = "RSV cases (%)") + 
    theme(legend.position = c(0.97, 0), legend.justification = c(1, 0)) +
    guides(color = guide_legend(title = "Reporting period"))
)

#====================================================================

#seasonal RSV dynamics before and after COVID-19 by regions for each year
print(
  rsv_euro %>%
    mutate(covid = if_else(date < "2020-01-01", "PreCOVID-19", 
                           if_else(date >= "2021-01-01" , "PostCOVID-19", "DurCOVID-19")),
           mon = month(date, label = TRUE, abbr = TRUE)) %>%
    filter(!is.na(covid), yr >= 2018) %>%
    arrange(date, country) %>%
    
    group_by(country, yr, date = round_date(date, "month"), mon, covid) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>% #sum up weekly cases to monthly for each year
    ungroup() %>%
    
    group_by(country, yr, covid) %>%
    mutate(pcases = cases/sum(cases, na.rm = TRUE)) %>% #compute share of averaged cases in each month
    ungroup() %>%
    
    ggplot(aes(x = factor(mon, levels(factor(mon))[c(6,7,8,9,10,11,12,1,2,3,4,5)]), y = pcases, group = yr, color = factor(yr))) +
    geom_line(size = 1) + 
    scale_y_continuous(breaks = seq(0, 1, 0.15), labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(. ~ country, scales = "free_y") +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    labs(title = "Seasonal dynamics of RSV cases in European countries in each year", x = "Months", y = "RSV cases (%)") + 
    theme(legend.position = c(0.97, 0), legend.justification = c(1, 0)) +
    guides(color = guide_legend(title = "Reporting period"))
)
