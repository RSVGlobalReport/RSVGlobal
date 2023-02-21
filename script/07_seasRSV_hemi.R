#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#TIME SERIES RSV DYNAMICS BY HEMISPHERE
#====================================================================

#time series of RSV cases
hemi_year <- 
  rsv_all %>% #(remember to filter for USA regions)
  dplyr::group_by(hemi, date, wk) %>%
  dplyr::summarise(cases = mean(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(hemi) %>%
  dplyr::mutate(cases = round(zoo::rollmean(cases, k = 3, fill = 0, align = 'right'))) %>%
  dplyr::ungroup()

#by each hemisphere
for (i in c("Northern hemisphere", "Southern hemisphere")) {
  
  plot8 = plotly::ggplotly(
    hemi_year %>%
      group_by(hemi) %>%
      mutate(newDate = max(date, na.rm = TRUE),
             newCases = cases[which.max(date == newDate)]) %>%
      ungroup() %>%
      filter(hemi == i) %>%
      ggplot(aes(x = date, y = cases)) +
      geom_line() + 
      geom_point(aes(x = newDate, y = newCases), color = "red", size = 2.5) +
      scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
      theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) +
      labs(title = paste0("14-days rolling average RSV cases, 2017+ in ", i), x = "Reporting date", y = "RSV cases")) %>%
    layout(hovermode = "x unified")
  
  htmlwidgets::saveWidget(as_widget(plot8), here("output", "timeseries_each_hemisphere", file = paste0("timeseries_", i,".html")))
  unlink(paste0(here("output", "timeseries_each_hemisphere", paste0("timeseries_",i,"_files"))), recursive = TRUE) #delete metadata
  
}


#====================================================================
#WEEKLY RSV DYNAMICS BY EACH HEMISPHERE
#====================================================================

#weekly seasonal RSV dynamics for each year
wkno1 = 1:23
wkno2 = 24:53
wkno = c(24:53, 1:23)

rsv_all_p <-
  rsv_all %>%
  dplyr::group_by(hemi, yr, wk) %>%
  dplyr::mutate(cases = mean(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(hemi, yr) %>% 
  mutate(seas = case_when(hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2017 ~ "2017/18",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2018 ~ "2017/18",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2018 ~ "2018/19",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2019 ~ "2018/19",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2019 ~ "2019/20",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2020 ~ "2019/20",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2020 ~ "2020/21",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2021 ~ "2020/21",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2021 ~ "2021/22",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2022 ~ "2021/22",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2022 ~ "2022/23",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2023 ~ "2022/23",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2023 ~ "2023/24", 
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2024 ~ "2023/24", 
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2024 ~ "2024/25",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2025 ~ "2024/25",
                          TRUE ~ NA_character_))

for (i in c("Southern hemisphere")) {
  
  plot9 = plotly::ggplotly(
    rsv_all_p %>%
      dplyr::mutate(yr = as.factor(yr), cases = round(cases, digits = 0)) %>%
      group_by(hemi) %>%
      mutate(newDate = max(date, na.rm = TRUE),
             newWk = wk[which.max(date == newDate)],
             newCases = cases[which.max(date == newDate)]) %>%
      ungroup() %>%
      dplyr::filter(hemi == i) %>% 
      
      ggplot(aes(x = wk, y = cases, color = yr)) +
      geom_line(size = 1) +
      geom_point(aes(x = newWk, y = newCases, color = yr), size = 2.5) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Mean weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
  htmlwidgets::saveWidget(as_widget(plot9), here("output", "weekly_each_hemisphere", file = paste0("weekly_", i,".html")))
  unlink(paste0(here("output", "weekly_each_hemisphere", paste0("weekly_",i,"_files"))), recursive = TRUE) #delete metadata
}

for (i in c("Northern hemisphere")) {
  
  plot10 = plotly::ggplotly(
    rsv_all_p %>%
      dplyr::mutate(yr = as.factor(yr), cases = round(cases, digits = 0), wk = factor(wk, levels(factor(wk))[c(wkno)])) %>%
      group_by(hemi) %>%
      mutate(newDate = max(date, na.rm = TRUE),
             newWk = wk[which.max(date == newDate)],
             newCases = cases[which.max(date == newDate)]) %>%
      ungroup() %>%
      dplyr::filter(hemi == i, !is.na(seas)) %>% 
      
      ggplot(aes(x = wk, y = cases, group = seas, color = seas)) +
      geom_line(size = 1) +
      geom_point(aes(x = newWk, y = newCases, color = seas), size = 2.5) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Mean weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_discrete(breaks = seq(1, 52, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
  htmlwidgets::saveWidget(as_widget(plot10), here("output", "weekly_each_hemisphere", file = paste0("weekly_", i,".html")))
  unlink(paste0(here("output", "weekly_each_hemisphere", paste0("weekly_",i,"_files"))), recursive = TRUE) #delete metadata
}


#====================================================================
#COVID-19 IMPACT ON RSV DYNAMICS BY HEMISPHERE
#====================================================================

#weekly seasonal RSV dynamics before/after COVID-19 by regions aggregated across years
rsv_all_q <-
  rsv_all %>%
  dplyr::group_by(hemi, yr) %>% 
  mutate(seas = case_when(hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2017 ~ "2017/18",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2018 ~ "2017/18",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2018 ~ "2018/19",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2019 ~ "2018/19",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2019 ~ "2019/20",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2020 ~ "2019/20",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2020 ~ "2020/21",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2021 ~ "2020/21",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2021 ~ "2021/22",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2022 ~ "2021/22",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2022 ~ "2022/23",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2023 ~ "2022/23",
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2023 ~ "2023/24", 
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2024 ~ "2023/24", 
                          hemi == "Northern hemisphere" & wk %in% wkno2 & yr == 2024 ~ "2024/25",
                          hemi == "Northern hemisphere" & wk %in% wkno1 & yr == 2025 ~ "2024/25",
                          TRUE ~ NA_character_)) %>%
  
  dplyr::mutate(covidS = if_else(date < "2020-01-01", " PreCOVID (2017-19)", 
                                 if_else(year(date) == 2021, "2021", 
                                         if_else(year(date) == 2022, "2022",
                                                 if_else(year(date) == 2023, "2023", 
                                                         if_else(year(date) == 2024, "2024",
                                                                 if_else(year(date) == 2025, "2025", NA_character_)))))),
                                         
                covidN = if_else(seas == "2017/18" | seas == "2018/19" | seas == "2019/20", " PreCOVID (2017-19)",
                                 if_else(seas == "2021/22", "2021/22", 
                                         if_else(seas == "2022/23", "2022/23", 
                                                 if_else(seas == "2023/24", "2023/24",
                                                         if_else(seas == "2024/25", "2024/25", NA_character_)))))
  )

rsv_all_q1 <-
  rsv_all_q %>%
  dplyr::filter(!is.na(covidS)) %>%
  dplyr::group_by(hemi, wk, covidS) %>%
  dplyr::mutate(mcases = mean(cases, rm.na = TRUE)) %>%
  dplyr::ungroup()

rsv_all_q2 <-
  rsv_all_q %>%
  dplyr::filter(!is.na(covidN)) %>%
  dplyr::group_by(hemi, wk, covidN) %>%
  dplyr::mutate(mcases = mean(cases, rm.na = TRUE)) %>%
  dplyr::ungroup()

for (i in c("Southern hemisphere")) {
  
  plot11 = plotly::ggplotly(
    rsv_all_q1 %>%
      dplyr::mutate(period = as.factor(covidS), cases = round(mcases, digits = 0)) %>%
      dplyr::filter(hemi == i, !is.na(period)) %>% 
      group_by(hemi) %>%
      mutate(newDate = max(date, na.rm = TRUE),
             newWk = wk[which.max(date == newDate)],
             newCases = cases[which.max(date == newDate)]) %>%
      ungroup() %>%
      
      ggplot(aes(x = wk, y = cases, color = period, group = period)) +
      geom_line(size = 1) +
      geom_point(aes(x = newWk, y = newCases, color = period), size = 2.5) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Mean weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
  htmlwidgets::saveWidget(as_widget(plot11), here("output", "covidimpact_each_hemisphere", file = paste0("covidimpact_", i,".html")))
  unlink(paste0(here("output", "covidimpact_each_hemisphere", paste0("covidimpact_",i,"_files"))), recursive = TRUE) #delete metadata
}

for (i in c("Northern hemisphere")) {
  
  plot12 = plotly::ggplotly(
    rsv_all_q2 %>%
      dplyr::mutate(period = as.factor(covidN), cases = round(mcases, digits = 0), wk = factor(wk, levels(factor(wk))[c(wkno)])) %>%
      dplyr::filter(hemi == i, !is.na(period)) %>% 
      group_by(hemi) %>%
      mutate(newDate = max(date, na.rm = TRUE),
             newWk = wk[which.max(date == newDate)],
             newCases = cases[which.max(date == newDate)]) %>%
      ungroup() %>%
      
      ggplot(aes(x = wk, y = cases, group = period, color = period)) +
      geom_line(size = 1) +
      geom_point(aes(x = newWk, y = newCases, color = period), size = 2.5) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Mean weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_discrete(breaks = seq(1, 52, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
  htmlwidgets::saveWidget(as_widget(plot12), here("output", "covidimpact_each_hemisphere", file = paste0("covidimpact_", i,".html")))
  unlink(paste0(here("output", "covidimpact_each_hemisphere", paste0("covidimpact_",i,"_files"))), recursive = TRUE) #delete metadata
}
