#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#TIME SERIES OF RSV DYNAMICS BY EACH COUNTRY IN EUROPE
#====================================================================

#time series of RSV cases
country_year <-
  rsv_euro %>%
  dplyr::group_by(country, date, wk) %>%
  dplyr::summarise(cases = mean(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cases = round(zoo::rollmean(cases, k = 3, fill = 0, align = 'right'))) %>%
  dplyr::ungroup()

for (i in c("France", "Germany", "Netherlands", "Spain", "Portugal", "Iceland", "Ireland", "Denmark", 
            "Sweden", "England", "Northern Ireland", "Scotland", "Bulgaria", "Hungary", "Slovakia")) {
  plot28 = plotly::ggplotly(
    country_year %>%
      filter(country == i) %>%
      ggplot(aes(x = date, y = cases)) +
      geom_line() + 
      scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
      theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) +
      labs(title = paste0("14-days rolling average RSV cases, 2017+ in ", i), x = "Reporting date", y = "RSV cases"))
  
  htmlwidgets::saveWidget(as_widget(plot28), here("output", "timeseries_each_country", file = paste0("timeseries_", i,".html")))
  unlink(paste0(here("output", "timeseries_each_country", paste0("timeseries_",i,"_files"))), recursive = TRUE) #delete metadata
}


#====================================================================
#WEEKLY RSV DYNAMICS BY EACH COUNTRY IN EUROPE
#====================================================================

#weekly seasonal RSV dynamics for each year
wkno1 = 1:23
wkno2 = 24:53
wkno = c(24:53, 1:23)

rsv_all_p <-
  rsv_euro %>%
  dplyr::group_by(country, yr) %>% 
  mutate(seas = case_when(wk %in% wkno2 & yr == 2017 ~ "2017/18",
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

#weekly seasonal RSV dynamics for each year
for (i in c("France", "Germany", "Netherlands", "Spain", "Portugal", "Iceland", "Ireland", "Denmark", 
            "Sweden", "England", "Northern Ireland", "Scotland", "Bulgaria", "Hungary", "Slovakia")) {
  plot29 = plotly::ggplotly(
    rsv_all_p %>%
      group_by(country) %>%
      dplyr::mutate(yr = as.factor(yr), 
                    wk = factor(wk, levels(factor(wk))[c(wkno)]),
                    cases = round(cases, digits = 0),
                    newDate = max(date, na.rm = TRUE),
                    newWk = wk[which.max(date == newDate)],
                    newCases = cases[which.max(date == newDate)]) %>%
      ungroup() %>%
      filter(!is.na(seas), country == i) %>%
      
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
  
  htmlwidgets::saveWidget(as_widget(plot29), here("output", "weekly_each_country", file = paste0("weekly_", i,".html")))
  unlink(paste0(here("output", "weekly_each_country", paste0("weekly_",i,"_files"))), recursive = TRUE) #delete metadata
}


#====================================================================
#COVID-19 IMPACT ON RSV DYNAMICS BY EACH COUNTRY IN EUROPE
#====================================================================

#weekly seasonal RSV dynamics for each year
wkno1 = 1:23
wkno2 = 24:53
wkno = c(24:53, 1:23)

#weekly seasonal RSV dynamics before/after COVID-19 by regions aggregated across years
rsv_all_q <-
  rsv_euro %>%
  dplyr::group_by(region, yr) %>% 
  mutate(seas = case_when(wk %in% wkno2 & yr == 2017 ~ "2017/18",
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
  
  #compute the cases by covid period
  dplyr::mutate(covid = if_else(seas == "2017/18" | seas == "2018/19" | seas == "2019/20", " PreCOVID (2017-19)",
                                if_else(seas == "2021/22", "2021/22", 
                                        if_else(seas == "2022/23", "2022/23", NA_character_)))) %>%
  dplyr::filter(!is.na(covid)) %>%
  dplyr::group_by(country, wk, covid) %>%
  dplyr::mutate(cases = mean(cases, rm.na = TRUE)) %>%
  dplyr::ungroup()

#weekly seasonal RSV dynamics for each year
for (i in c("France", "Germany", "Netherlands", "Spain", "Portugal", "Iceland", "Ireland", "Denmark", 
            "Sweden", "England", "Northern Ireland", "Scotland", "Bulgaria", "Hungary", "Slovakia")) {
  plot30 = plotly::ggplotly(
    rsv_all_q %>%
      group_by(country) %>%
      dplyr::mutate(yr = as.factor(yr), 
                    wk = factor(wk, levels(factor(wk))[c(wkno)]),
                    cases = round(cases, digits = 0),
                    newDate = max(date, na.rm = TRUE),
                    newWk = wk[which.max(date == newDate)],
                    newCases = cases[which.max(date == newDate)]) %>%
      ungroup() %>%
      filter(!is.na(seas), country == i) %>%
      
      ggplot(aes(x = wk, y = cases, group = covid, color = covid)) +
      geom_line(size = 1) +
      geom_point(aes(x = newWk, y = newCases, color = covid), size = 2) +
      scale_colour_brewer(palette = 7, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i, " by year"), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_discrete(breaks = seq(1, 52, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow")))
  
  htmlwidgets::saveWidget(as_widget(plot30), here("output", "covidimpact_each_country", file = paste0("covidimpact_", i,".html")))
  unlink(paste0(here("output", "covidimpact_each_country", paste0("covidimpact_",i,"_files"))), recursive = TRUE) #delete metadata
}
