#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#YEAR ON YEAR RSV DYNAMICS BY HEMISPHERE
#====================================================================

#weekly year on year RSV cases
hemi_year <- 
  rsv_all %>%
  dplyr::group_by(hemi, date, wk) %>%
  dplyr::summarise(cases = mean(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(hemi) %>%
  dplyr::mutate(cases = round(zoo::rollmean(cases, k = 3, fill = 0, align = 'right'))) %>%
  dplyr::ungroup()

#both hemispheres
plot7 <-
  plotly::ggplotly(
    hemi_year %>%
      ggplot(aes(x = date, y = cases)) +
      geom_rect(aes(xmin = date(now())-30.44, xmax = date(now()), ymin = 0, ymax = Inf), fill = "grey70", alpha = 0.8) +
      geom_line() + 
      facet_wrap(. ~ hemi, scales = "free_y") +
      scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
      theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) +
      theme(plot.title = element_text(size = 14)) +
      theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 16)) +
      labs(title = "14-days rolling average RSV cases, 2017+", x = "Reporting date", y = "RSV cases"))

htmlwidgets::saveWidget(as_widget(plot7), here("output", "yearOnyear_all_hemisphere", file = "all_hemisphere_yearOnyear.html"))


#by each hemisphere
for (i in c("Northern hemisphere", "Southern hemisphere")) {
  
  plot8 = plotly::ggplotly(
    hemi_year %>%
      filter(hemi == i) %>%
      ggplot(aes(x = date, y = cases)) +
      geom_line() + 
      scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
      theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) +
      labs(title = paste0("14-days rolling average RSV cases, 2017+ in ", i), x = "Reporting date", y = "RSV cases"))
  
  htmlwidgets::saveWidget(as_widget(plot8), here("output", "yearOnyear_each_hemisphere", file = paste0("HemisphereYearly_", i,".html")))
}


#====================================================================
#WEEKLY RSV DYNAMICS BY HEMISPHERE
#====================================================================

#weekly seasonal RSV dynamics for each year
wkno1 = 1:23
wkno2 = 24:53
wkno = c(24:53, 1:23)

rsv_all_p <-
  rsv_all %>%
  dplyr::group_by(hemi, yr, wk) %>%
  dplyr::summarise(cases = mean(cases, na.rm = TRUE)) %>%
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
                          TRUE ~ NA_character_))

for (i in c("Southern hemisphere")) {
  
  plot9 = plotly::ggplotly(
    rsv_all_p %>%
      dplyr::mutate(yr = as.factor(yr), cases = round(cases, digits = 0)) %>%
      dplyr::filter(hemi == i) %>% 
      ggplot(aes(x = wk, y = cases, color = yr)) +
      geom_line(size = 1) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
  htmlwidgets::saveWidget(as_widget(plot9), here("output", "weekly_each_hemisphere", file = paste0("HemisphereWeekly_", i,".html")))
}

for (i in c("Northern hemisphere")) {
  
  plot10 = plotly::ggplotly(
    rsv_all_p %>%
      dplyr::mutate(yr = as.factor(yr), cases = round(cases, digits = 0), wk = factor(wk, levels(factor(wk))[c(wkno)])) %>%
      dplyr::filter(hemi == i, !is.na(seas)) %>% 
      ggplot(aes(x = wk, y = cases, group = seas, color = seas)) +
      geom_line(size = 1) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_discrete(breaks = seq(1, 52, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
  htmlwidgets::saveWidget(as_widget(plot10), here("output", "weekly_each_hemisphere", file = paste0("HemisphereWeekly_", i,".html")))
}


#====================================================================
#BEFORE AND AFTER COVID-19 RSV DYNAMICS BY HEMISPHERE
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
                          TRUE ~ NA_character_)) %>%
  
  dplyr::mutate(covidS = if_else(date < "2020-01-01", " PreCOVID (2017-19)", 
                                 if_else(year(date) == 2021, "2021", "2022")),
                covidN = if_else(seas == "2017/18" | seas == "2018/19" | seas == "2019/20", " PreCOVID (2017-19)",
                                 if_else(seas == "2021/22", "2021/22", 
                                         if_else(seas == "2022/23", "2022/23", NA_character_)))
  )

rsv_all_q1 <-
  rsv_all_q %>%
  dplyr::filter(!is.na(covidS)) %>%
  dplyr::group_by(hemi, wk, covidS) %>%
  dplyr::summarise(mcases = mean(cases, rm.na = TRUE)) %>%
  dplyr::ungroup()

rsv_all_q2 <-
  rsv_all_q %>%
  dplyr::filter(!is.na(covidN)) %>%
  dplyr::group_by(hemi, wk, covidN) %>%
  dplyr::summarise(mcases = mean(cases, rm.na = TRUE)) %>%
  dplyr::ungroup()

for (i in c("Southern hemisphere")) {
  
  plot11 = plotly::ggplotly(
    rsv_all_q1 %>%
      dplyr::mutate(covid = as.factor(covidS), cases = round(mcases, digits = 0)) %>%
      dplyr::filter(hemi == i, !is.na(covid)) %>% 
      ggplot(aes(x = wk, y = cases, color = covid)) +
      geom_line(size = 1) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
  htmlwidgets::saveWidget(as_widget(plot11), here("output", "covid_each_hemisphere", file = paste0("HemisphereCovid_", i,".html")))
}

for (i in c("Northern hemisphere")) {
  
  plot12 = plotly::ggplotly(
    rsv_all_q2 %>%
      dplyr::mutate(covid = as.factor(covidN), cases = round(mcases, digits = 0), wk = factor(wk, levels(factor(wk))[c(wkno)])) %>%
      dplyr::filter(hemi == i, !is.na(covidN)) %>% 
      ggplot(aes(x = wk, y = cases, group = covidN, color = covidN)) +
      geom_line(size = 1) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_discrete(breaks = seq(1, 52, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
  htmlwidgets::saveWidget(as_widget(plot12), here("output", "covid_each_hemisphere", file = paste0("HemisphereCovid_", i,".html")))
}
