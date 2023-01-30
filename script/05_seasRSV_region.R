#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak


#====================================================================
#YEAR ON YEAR RSV DYNAMICS BY REGION
#====================================================================

#weekly year on year RSV cases
reg_year <- 
rsv_all %>%
  dplyr::group_by(region, date, wk) %>%
  dplyr::summarise(cases = mean(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(cases = round(zoo::rollmean(cases, k = 3, fill = 0, align = 'right'))) %>%
  dplyr::ungroup()

#all WHO regions
plot1 <-
plotly::ggplotly(
reg_year %>%
  ggplot(aes(x = date, y = cases)) +
  geom_rect(aes(xmin = date(now())-30.44, xmax = date(now()), ymin = 0, ymax = Inf), fill = "grey70", alpha = 0.8) +
  geom_line() + 
  facet_wrap(. ~ region, scales = "free_y") +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
  theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) +
  theme(plot.title = element_text(size = 14)) +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 16)) +
  labs(title = "14-days rolling average RSV cases, 2017+", x = "Reporting date", y = "RSV cases"))

htmlwidgets::saveWidget(as_widget(plot1), here("output", "yearOnyear_all_region", file = "all_region_yearOnyear.html"))


#by each region
for (i in c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South East Asia", "Western Pacific")) {
  
  plot2 = plotly::ggplotly(
    reg_year %>%
      filter(region == i) %>%
      ggplot(aes(x = date, y = cases)) +
      geom_line() + 
      scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
      theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) +
      labs(title = paste0("14-days rolling average RSV cases, 2017+ in ", i), x = "Reporting date", y = "RSV cases"))
  
  htmlwidgets::saveWidget(as_widget(plot2), here("output", "yearOnyear_each_region", file = paste0("RegionYearly_", i,".html")))
}


#====================================================================
#WEEKLY RSV DYNAMICS BY REGION
#====================================================================

#weekly seasonal RSV dynamics for each year
wkno1 = 1:23
wkno2 = 24:53
wkno = c(24:53, 1:23)

rsv_all_p <-
  rsv_all %>%
  dplyr::group_by(region, yr, wk) %>%
  dplyr::summarise(cases = mean(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(region, yr) %>% 
  mutate(seas = case_when((region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2017 ~ "2017/18",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2018 ~ "2017/18",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2018 ~ "2018/19",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2019 ~ "2018/19",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2019 ~ "2019/20",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2020 ~ "2019/20",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2020 ~ "2020/21",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2021 ~ "2020/21",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2021 ~ "2021/22",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2022 ~ "2021/22",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2022 ~ "2022/23",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2023 ~ "2022/23",
                          TRUE ~ NA_character_))

for (i in c("Africa", "Americas", "South East Asia", "Western Pacific")) {
  
  plot3 = plotly::ggplotly(
    rsv_all_p %>%
      dplyr::mutate(yr = as.factor(yr), cases = round(cases, digits = 0)) %>%
      dplyr::filter(region == i) %>% 
      ggplot(aes(x = wk, y = cases, color = yr)) +
      geom_line(size = 1) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
htmlwidgets::saveWidget(as_widget(plot3), here("output", "weekly_each_region", file = paste0("RegionWeekly_", i,".html")))
}

for (i in c("Europe", "Eastern Mediterranean")) {
  
  plot4 = plotly::ggplotly(
    rsv_all_p %>%
      dplyr::mutate(yr = as.factor(yr), cases = round(cases, digits = 0), wk = factor(wk, levels(factor(wk))[c(wkno)])) %>%
      dplyr::filter(region == i, !is.na(seas)) %>% 
      ggplot(aes(x = wk, y = cases, group = seas, color = seas)) +
      geom_line(size = 1) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_discrete(breaks = seq(1, 52, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
  htmlwidgets::saveWidget(as_widget(plot4), here("output", "weekly_each_region", file = paste0("RegionWeekly_", i,".html")))
}
 
#====================================================================
#BEFORE AND AFTER COVID-19 RSV DYNAMICS BY REGION
#====================================================================
 
#weekly seasonal RSV dynamics before/after COVID-19 by regions aggregated across years
rsv_all_q <-
  rsv_all %>%
  dplyr::group_by(region, yr) %>% 
  mutate(seas = case_when((region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2017 ~ "2017/18",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2018 ~ "2017/18",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2018 ~ "2018/19",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2019 ~ "2018/19",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2019 ~ "2019/20",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2020 ~ "2019/20",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2020 ~ "2020/21",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2021 ~ "2020/21",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2021 ~ "2021/22",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2022 ~ "2021/22",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno2 & yr == 2022 ~ "2022/23",
                          (region == "Europe" | region == "Eastern Mediterranean") & wk %in% wkno1 & yr == 2023 ~ "2022/23",
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
   dplyr::group_by(region, wk, covidS) %>%
   dplyr::summarise(mcases = mean(cases, rm.na = TRUE)) %>%
   dplyr::ungroup()

rsv_all_q2 <-
  rsv_all_q %>%
  dplyr::filter(!is.na(covidN)) %>%
  dplyr::group_by(region, wk, covidN) %>%
  dplyr::summarise(mcases = mean(cases, rm.na = TRUE)) %>%
  dplyr::ungroup()

for (i in c("Africa", "Americas", "South East Asia", "Western Pacific")) {
  
  plot5 = plotly::ggplotly(
    rsv_all_q1 %>%
      dplyr::mutate(covid = as.factor(covidS), cases = round(mcases, digits = 0)) %>%
      dplyr::filter(region == i, !is.na(covid)) %>% 
      ggplot(aes(x = wk, y = cases, color = covid)) +
      geom_line(size = 1) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
  htmlwidgets::saveWidget(as_widget(plot5), here("output", "covid_each_region", file = paste0("RegionCovid_", i,".html")))
}

for (i in c("Europe", "Eastern Mediterranean")) {
  
  plot6 = plotly::ggplotly(
    rsv_all_q2 %>%
      dplyr::mutate(covid = as.factor(covidN), cases = round(mcases, digits = 0), wk = factor(wk, levels(factor(wk))[c(wkno)])) %>%
      dplyr::filter(region == i, !is.na(covidN)) %>% 
      ggplot(aes(x = wk, y = cases, group = covidN, color = covidN)) +
      geom_line(size = 1) +
      scale_colour_brewer(palette = 1, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_discrete(breaks = seq(1, 52, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "white")))
  
  htmlwidgets::saveWidget(as_widget(plot6), here("output", "covid_each_region", file = paste0("RegionCovid_", i,".html")))
}
