#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#WEEKLY
#====================================================================
#define week number from mid-year to completely capture the peak in Northern hemisphere
wkno = c(24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)

#year on year weekly RSV cases in European countries
print(
rsv_euro %>%
  ggplot(aes(x = date(date), y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
  theme(strip.background = element_rect(fill = "orange")) +
  labs(title = "Weekly RSV cases in European countries, 2018-2022", x = "Date", y = "RSV cases")
)

#weekly seasonal RSV dynamics before and after COVID-19 by regions for each year
print(
  rsv_euro %>%
    filter(yr != 2020) %>%
    group_by(country, yr, wk) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>%
    ungroup() %>%

    ggplot(aes(x = factor(wk, levels(factor(wk))[c(wkno)]), y = cases, group = yr, color = factor(yr))) +
    geom_line(size = 1) + 
    scale_x_discrete(breaks = seq(1, 52, 3)) +
    facet_wrap(. ~ country, scales = "free_y") +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    labs(title = "Weekly seasonal dynamics of RSV cases by European country & year", x = "Week (Jun-May)", y = "RSV cases") + 
    theme(legend.position = c(0.97, 0.02), legend.justification = c(1, 0), strip.background = element_rect(fill = "orange")) +
    guides(color = guide_legend(title = ""))
)

#weekly seasonal RSV dynamics before and after COVID-19 by regions aggregated across all years
print(
  rsv_euro %>%
    mutate(covid = if_else(date < "2020-01-01", "PreCOVID-19 (2018-19)", 
                           if_else(date >= "2021-01-01" , "PostCOVID-19 (2021-22)", NA_character_))) %>%
    filter(!is.na(covid), yr != 2020) %>%
    group_by(country, wk, covid) %>%
    summarise(mcases = mean(cases, rm.na = TRUE)) %>%
    ungroup() %>%
    
    ggplot(aes(x = factor(wk, levels(factor(wk))[c(wkno)]), y = mcases, group = covid, color = covid)) +
    geom_line(size = 1) + 
    scale_x_discrete(breaks = seq(1, 52, 4)) +
    facet_wrap(. ~ country, scales = "free_y") +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    labs(title = "Weekly seasonal dynamics of RSV cases by European country & COVID-19 phase", x = "Week (Jun-May)", y = "RSV cases") + 
    theme(legend.position = c(0.97, 0.05), legend.justification = c(1, 0)) +
    guides(color = guide_legend(title = "Reporting period"))
)
#====================================================================
#MONTHLY
#====================================================================

#year on year monthly RSV cases in European countries
print(
rsv_euro %>%
  group_by(date = round_date(date, "month"), country) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
  theme(strip.background = element_rect(fill = "orange")) +
  labs(title = "Monthly RSV cases in European countries, 2018-2022", x = "Date", y = "RSV cases")
)

#seasonal RSV dynamics before and after COVID-19 by regions for each year
print(
  rsv_euro %>%
    mutate(mon = month(date, label = TRUE, abbr = TRUE)) %>%
    filter(yr != 2020) %>%
    group_by(country, yr, date = round_date(date, "month"), mon) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>%
    ungroup() %>%
    
    ggplot(aes(x = factor(mon, levels(factor(mon))[c(6,7,8,9,10,11,12,1,2,3,4,5)]), y = cases, group = yr, color = factor(yr))) +
    geom_line(size = 1) + 
    facet_wrap(. ~ country, scales = "free_y") +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    labs(title = "Monthly seasonal dynamics of RSV cases by European country & year", x = "Month", y = "RSV cases") + 
    theme(legend.position = c(0.97, 0), legend.justification = c(1, 0), strip.background = element_rect(fill = "orange")) +
    guides(color = guide_legend(title = ""))
)

#seasonal RSV dynamics before and after COVID-19 and mean timing of peak in European countries
print(
  rsv_euro %>%
    mutate(covid = if_else(date < "2020-01-01", "PreCOVID-19 (2018-19)", 
                           if_else(date >= "2021-01-01" , "PostCOVID-19 (2021-22)", NA_character_)),
           mon = month(date, label = TRUE, abbr = TRUE)) %>%
    filter(!is.na(covid)) %>%
    group_by(country, date = round_date(date, "month"), mon, covid) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(country, mon, covid) %>%
    summarise(mcases = mean(cases, rm.na = TRUE)) %>%
    ungroup() %>%
    
    ggplot(aes(x = factor(mon, levels(factor(mon))[c(6,7,8,9,10,11,12,1,2,3,4,5)]), y = mcases, group = covid, color = covid)) +
    geom_line(size = 1) + 
    facet_wrap(. ~ country, scales = "free_y") +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    labs(title = "Monthly seasonal dynamics of RSV cases by European country & COVID-19 phase", x = "Month", y = "RSV cases") + 
    theme(legend.position = c(0.97, 0), legend.justification = c(1, 0), strip.background = element_rect(fill = "orange")) +
    guides(color = guide_legend(title = "Reporting period"))
)
