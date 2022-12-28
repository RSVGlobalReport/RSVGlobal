#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================

#timing of seasonal RSV cases before and after COVID-19 by regions
print(
  rsv_regn %>%
    mutate(covid = if_else(date < "2020-01-01", "PreCOVID-19 (2018-19)", 
                           if_else(date >= "2021-01-01" , "PostCOVID-19 (2021-22)", NA_character_)),
           mon = month(date, label = TRUE, abbr = TRUE)) %>%
    filter(!is.na(covid)) %>%
    group_by(date = round_date(date, "month"), mon, region, covid) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>% #sum up weekly cases to monthly for each year
    ungroup() %>%
    group_by(covid, region, mon) %>%
    summarise(mcases = mean(cases, rm.na = TRUE)) %>% #average monthly cases across years
    ungroup() %>%
    group_by(covid, region) %>%
    mutate(pcases = mcases/sum(mcases, na.rm = TRUE)) %>% #compute share of averaged cases in each month
    ungroup() %>%
    
    ggplot(aes(x = mon, y = pcases, group = covid, color = covid)) +
    geom_line(size = 1) + 
    coord_polar(clip = "off") +
    scale_y_continuous(breaks = seq(0, 1, 0.05), labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(. ~ region) +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
    labs(title = "Monthly timing of the peak RSV cases by region & COVID-19 phase", x = "", y = "") + 
    theme(legend.position = c(0.95, 0.1), legend.justification = c(1, 0), strip.background = element_rect(fill = "light yellow")) +
    guides(color = guide_legend(title = "Reporting period")) +
    theme(aspect.ratio = 1)
)

#timing of seasonal RSV cases before and after COVID-19 by Africa/SEAR/ME/WPR country
print(
  rsv_asmw %>%
    mutate(covid = if_else(date < "2020-01-01", "PreCOVID-19 (2018-19)", 
                           if_else(date >= "2021-01-01" , "PostCOVID-19 (2021-22)", NA_character_)),
           mon = month(date, label = TRUE, abbr = TRUE)) %>%
    filter(!is.na(covid)) %>%
    group_by(date = round_date(date, "month"), mon, country, covid) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>% #sum up weekly cases to monthly for each year
    ungroup() %>%
    group_by(covid, country, mon) %>%
    summarise(mcases = mean(cases, rm.na = TRUE)) %>% #average monthly cases across years
    ungroup() %>%
    group_by(covid, country) %>%
    mutate(pcases = mcases/sum(mcases, na.rm = TRUE)) %>% #compute share of averaged cases in each month
    ungroup() %>%
    
    ggplot(aes(x = mon, y = pcases, group = covid, color = covid)) +
    geom_line(size = 1) + 
    coord_polar(clip = "off") +
    scale_y_continuous(breaks = seq(0, 1, 0.05), labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(. ~ country) +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
    labs(title = "Monthly timing of the peak RSV cases by African/SEAR/ME/WPR country & COVID-19 phase", x = "", y = "") + 
    theme(legend.position = "bottom") +
    guides(color = guide_legend(title = "Reporting period"), strip.background = element_rect(fill = "light blue")) +
    theme(aspect.ratio = 1)
)

#timing of seasonal RSV cases before and after COVID-19 by European countries
print(
  rsv_euro %>%
    mutate(covid = if_else(date < "2020-01-01", "PreCOVID-19 (2018-19)", 
                           if_else(date >= "2021-01-01" , "PostCOVID-19 (2021-22)", NA_character_)),
           mon = month(date, label = TRUE, abbr = TRUE)) %>%
    filter(!is.na(covid)) %>%
    group_by(date = round_date(date, "month"), mon, country, covid) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>% #sum up weekly cases to monthly for each year
    ungroup() %>%
    group_by(covid, country, mon) %>%
    summarise(mcases = mean(cases, rm.na = TRUE)) %>% #average monthly cases across years
    ungroup() %>%
    group_by(covid, country) %>%
    mutate(pcases = mcases/sum(mcases, na.rm = TRUE)) %>% #compute share of averaged cases in each month
    ungroup() %>%
    
    ggplot(aes(x = mon, y = pcases, group = covid, color = covid)) +
    geom_line(size = 1) + 
    coord_polar(clip = "off") +
    scale_y_continuous(breaks = seq(0, 1, 0.05), labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(. ~ country) +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
    labs(title = "Monthly timing of the peak RSV cases by European country & COVID-19 phase", x = "", y = "") + 
    theme(legend.position = "bottom", strip.background = element_rect(fill = "orange")) +
    guides(color = guide_legend(title = "Reporting period")) +
    theme(aspect.ratio = 1)
)

#timing of seasonal RSV cases before and after COVID-19 by regions
print(
  rsv_amer %>%
    mutate(covid = if_else(date < "2020-01-01", "PreCOVID-19", 
                           if_else(date >= "2021-01-01" , "PostCOVID-19", NA_character_)),
           mon = month(date, label = TRUE, abbr = TRUE)) %>%
    filter(!is.na(covid)) %>%
    group_by(date = round_date(date, "month"), mon, country, covid) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>% #sum up weekly cases to monthly for each year
    ungroup() %>%
    group_by(covid, country, mon) %>%
    summarise(mcases = mean(cases, rm.na = TRUE)) %>% #average monthly cases across years
    ungroup() %>%
    group_by(covid, country) %>%
    mutate(pcases = mcases/sum(mcases, na.rm = TRUE)) %>% #compute share of averaged cases in each month
    ungroup() %>%
    
    ggplot(aes(x = mon, y = pcases, group = covid, color = covid)) +
    geom_line(size = 1) + 
    coord_polar(clip = "off") +
    scale_y_continuous(breaks = seq(0, 1, 0.05), labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(. ~ country) +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
    labs(title = "Monthly timing of the peak RSV cases by Americas country & COVID-19 phase", x = "", y = "") + 
    theme(legend.position = c(0.90, 0.05), legend.justification = c(1, 0), strip.background = element_rect(fill = "light green")) +
    guides(color = guide_legend(title = "Reporting period")) +
    theme(aspect.ratio = 1)
)
