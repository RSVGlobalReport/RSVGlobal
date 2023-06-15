#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#TIME SERIES OF RSV DYNAMICS BY EACH COUNTRY GLOBALLY
#====================================================================

#time series of RSV cases
country_ts <-
rsv_all %>%
  dplyr::group_by(country, date, wk) %>%
  dplyr::summarise(cases = mean(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cases = round(zoo::rollmean(cases, k = 3, fill = 0, align = 'right'))) %>%
  dplyr::ungroup()

for (i in c("Central African Republic", "Ivory Coast", "Madagascar", "South Africa",
            "India",
            "Australia", "Japan", "Mongolia", "Malaysia",
            "Oman", "Qatar",
            "France", "Germany", "Netherlands", "Spain", "Portugal", "Iceland", "Ireland", "Denmark", "Sweden", "England", "Northern Ireland", "Scotland", "Bulgaria", "Hungary", "Slovakia",
            "Argentina", "Belize", "Bolivia", "Brazil", "Canada", "Colombia", "Costa Rica", "Dominica", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Uruguay", "United States")) {

plot1 = plotly::ggplotly(
  country_ts %>%
    dplyr::filter(country == i) %>%
    dplyr::mutate(newDate = max(date, na.rm = TRUE), 
           newCases = cases[which.max(date == newDate)]) %>%
    
    ggplot(aes(x = date, y = cases)) +
    geom_line() + 
    geom_point(aes(x = newDate, y = newCases), color = "red", size = 2.5) +
    scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
    theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) +
    labs(title = paste0("14-days rolling average RSV cases, 2017+ in ", i), x = "Reporting date", y = "RSV cases")) %>%
  layout(hovermode = "x unified")

htmlwidgets::saveWidget(as_widget(plot1), here("output", "timeseries_each_country", file = paste0("timeseries_", i,".html")))
unlink(paste0(here("output", "timeseries_each_country", paste0("timeseries_",i,"_files"))), recursive = TRUE) #delete metadata
}

#====================================================================
#WEEKLY RSV DYNAMICS BY EACH COUNTRY GLOBALLLY
#====================================================================

#weekly seasonal RSV dynamics for each year with typical calendar time (seasonality)

country_wk <- 
  rsv_all %>% 
  dplyr::left_join(climate %>% select(country, wk_scale)) %>%
  dplyr::filter(wk_scale == "normal")

for (i in c("Argentina", "Australia", "Belize", "Bolivia", "Central African Republic", "Colombia", "Costa Rica", "Dominica", 
            "El Salvador", "India", "Ivory Coast", "Japan", "Malaysia", "Nicaragua", "Paraguay", "Peru", "South Africa", "Uruguay")) {
  plot2 = plotly::ggplotly(
    country_wk %>%
      dplyr::filter(country == i) %>%
      dplyr::mutate(yr = as.factor(yr), 
                    cases = round(cases, digits = 0),
                    newDate = max(date, na.rm = TRUE),
                    newWk = wk[which.max(date == newDate)],
                    newCases = cases[which.max(date == newDate)]) %>%
      
      ggplot(aes(x = wk, y = cases, color = yr)) +
      geom_line(size = 1) +
      geom_point(aes(x = newWk, y = newCases, color = yr), size = 2) +
      scale_colour_brewer(palette = 7, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i, " by year"), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow")))
  
  htmlwidgets::saveWidget(as_widget(plot2), here("output", "weekly_each_country", file = paste0("weekly_", i,".html")))
  unlink(paste0(here("output", "weekly_each_country", paste0("weekly_",i,"_files"))), recursive = TRUE) #delete metadata
}


#weekly seasonal RSV dynamics for each year with modified calendar time (seasonality)
wkno1 = 1:23
wkno2 = 24:53
wkno = c(24:53, 1:23)

country_wk <-
  rsv_all %>%
  dplyr::group_by(country, yr) %>% 
  dplyr::mutate(seas = case_when(wk %in% wkno2 & yr == 2017 ~ "2017/18",
                          wk %in% wkno1 & yr == 2018 ~ "2017/18",
                          wk %in% wkno2 & yr == 2018 ~ "2018/19",
                          wk %in% wkno1 & yr == 2019 ~ "2018/19",
                          wk %in% wkno2 & yr == 2019 ~ "2019/20",
                          wk %in% wkno1 & yr == 2020 ~ "2019/20",
                          wk %in% wkno2 & yr == 2020 ~ "2020/21",
                          wk %in% wkno1 & yr == 2021 ~ "2020/21",
                          wk %in% wkno2 & yr == 2021 ~ "2021/22",
                          wk %in% wkno1 & yr == 2022 ~ "2021/22",
                          wk %in% wkno2 & yr == 2022 ~ "2022/23",
                          wk %in% wkno1 & yr == 2023 ~ "2022/23",
                          TRUE ~ NA_character_))

for (i in c("Brazil", "Bulgaria", "Canada", "Denmark", "Ecuador", "England", "France", "Germany", "Guatemala", "Honduras", "Hungary", "Iceland", "Ireland", "Madagascar", 
            "Mexico", "Mongolia", "Netherlands", "Northern Ireland", "Oman", "Panama", "Portugal", "Qatar",  "Scotland",  "Slovakia", "Spain", "Sweden", "United States")) {
  plot3 = plotly::ggplotly(
    country_wk %>%
      group_by(country) %>%
      dplyr::mutate(yr = as.factor(yr), 
                    wk = factor(wk, levels(factor(wk))[c(wkno)]),
                    cases = round(cases, digits = 0),
                    newDate = max(date, na.rm = TRUE),
                    newWk = wk[which.max(date == newDate)],
                    newCases = cases[which.max(date == newDate)]) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(seas), country == i) %>%
      
      ggplot(aes(x = wk, y = cases,  group = seas, color = seas)) +
      geom_line(size = 1) +
      geom_point(aes(x = newWk, y = newCases, color = seas), size = 2) +
      scale_colour_brewer(palette = 7, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i, " by year"), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_discrete(breaks = seq(1, 52, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow")))
  
  htmlwidgets::saveWidget(as_widget(plot3), here("output", "weekly_each_country", file = paste0("weekly_", i,".html")))
  unlink(paste0(here("output", "weekly_each_country", paste0("weekly_",i,"_files"))), recursive = TRUE) #delete metadata
}


#====================================================================
#COVID-19 IMPACT ON RSV DYNAMICS BY EACH COUNTRY IN AFRICA
#====================================================================

#weekly seasonal RSV dynamics for each year with typical calendar time (seasonality) before and after COVID-19 aggregated across preCOVID-19
for (i in c("Argentina", "Australia", "Belize", "Bolivia", "Central African Republic", "Colombia", "Costa Rica", "Dominica", 
            "El Salvador", "India", "Ivory Coast", "Japan", "Malaysia", "Nicaragua", "Paraguay", "Peru", "South Africa", "Uruguay")) {
  plot4 = plotly::ggplotly(
    rsv_all %>%
      dplyr::mutate(covid = if_else(date < "2020-01-01", " PreCOVID (2017-19)", 
                                    if_else(year(date) == 2020, NA_character_, 
                                            if_else(year(date) == 2021, "2021",
                                                    if_else(year(date) == 2022, "2022",
                                                            if_else(year(date) == 2023, "2023",
                                                                    if_else(year(date) == 2024, "2024",
                                                                            if_else(year(date) == 2025, "2025",
                                                                                    if_else(year(date) == 2026, "2026", "2027"))))))))) %>%
      dplyr::filter(!is.na(covid), country == i) %>%
      dplyr::group_by(country, wk, covid) %>%
      dplyr::mutate(cases = round(mean(cases, rm.na = TRUE), digits = 0)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(newDate = max(date, na.rm = TRUE),
             newWk = wk[which.max(date == newDate)],
             newCases = cases[which.max(date == newDate)]) %>%
      dplyr::ungroup() %>%
      
      ggplot(aes(x = wk, y = cases, color = covid)) +
      geom_line(size = 1) + 
      geom_point(aes(x = newWk, y = newCases, color = covid), size = 2) +
      scale_colour_brewer(palette = 7, direction = 1) + 
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      labs(title = paste0("(Mean) weekly seasonal RSV cases in ", i , " by year"), x = "Epi week", y = "RSV cases") +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow")) +
      guides(color = guide_legend(title = "")))
  
  htmlwidgets::saveWidget(as_widget(plot4), here("output", "covidimpact_each_country", file = paste0("covidimpact_", i,".html")))
  unlink(paste0(here("output", "covidimpact_each_country", paste0("covidimpact_",i,"_files"))), recursive = TRUE) #delete metadata
}


#weekly seasonal RSV dynamics for each year with modified calendar time (seasonality) before and after COVID-19 aggregated across preCOVID-19
wkno1 = 1:23
wkno2 = 24:53
wkno = c(24:53, 1:23)

country_cv <-
  rsv_all %>%
  dplyr::group_by(region, yr) %>% 
  dplyr::mutate(seas = case_when(wk %in% wkno2 & yr == 2017 ~ "2017/18",
                          wk %in% wkno1 & yr == 2018 ~ "2017/18",
                          wk %in% wkno2 & yr == 2018 ~ "2018/19",
                          wk %in% wkno1 & yr == 2019 ~ "2018/19",
                          wk %in% wkno2 & yr == 2019 ~ "2019/20",
                          wk %in% wkno1 & yr == 2020 ~ "2019/20",
                          wk %in% wkno2 & yr == 2020 ~ "2020/21",
                          wk %in% wkno1 & yr == 2021 ~ "2020/21",
                          wk %in% wkno2 & yr == 2021 ~ "2021/22",
                          wk %in% wkno1 & yr == 2022 ~ "2021/22",
                          wk %in% wkno2 & yr == 2022 ~ "2022/23",
                          wk %in% wkno1 & yr == 2023 ~ "2022/23",
                          TRUE ~ NA_character_)) %>%
  
  dplyr::mutate(covid = if_else(seas == "2017/18" | seas == "2018/19" | seas == "2019/20", " PreCOVID (2017-19)",
                                if_else(seas == "2021/22", "2021/22", 
                                        if_else(seas == "2022/23", "2022/23", NA_character_)))) %>%
  dplyr::filter(!is.na(covid)) %>%
  dplyr::group_by(country, wk, covid) %>%
  dplyr::mutate(cases = mean(cases, rm.na = TRUE)) %>%
  dplyr::ungroup()

for (i in c("Brazil", "Bulgaria", "Canada", "Denmark", "Ecuador", "England", "France", "Germany", "Guatemala", "Honduras", "Hungary", "Iceland", "Ireland", "Madagascar", 
            "Mexico", "Mongolia", "Netherlands", "Northern Ireland", "Oman", "Panama", "Portugal", "Qatar",  "Scotland",  "Slovakia", "Spain", "Sweden", "United States")) {
  plot5 = plotly::ggplotly(
    country_cv %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(yr = as.factor(yr), 
                    wk = factor(wk, levels(factor(wk))[c(wkno)]),
                    cases = round(cases, digits = 0),
                    newDate = max(date, na.rm = TRUE),
                    newWk = wk[which.max(date == newDate)],
                    newCases = cases[which.max(date == newDate)]) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(seas), country == i) %>%
      
      ggplot(aes(x = wk, y = cases, group = covid, color = covid)) +
      geom_line(size = 1) +
      geom_point(aes(x = newWk, y = newCases, color = covid), size = 2) +
      scale_colour_brewer(palette = 7, direction = 1) + 
      labs(title = paste0("(Mean) weekly seasonal RSV cases in ", i, " by year"), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_discrete(breaks = seq(1, 52, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow")))
  
  htmlwidgets::saveWidget(as_widget(plot5), here("output", "covidimpact_each_country", file = paste0("covidimpact_", i,".html")))
  unlink(paste0(here("output", "covidimpact_each_country", paste0("covidimpact_",i,"_files"))), recursive = TRUE) #delete metadata
}
