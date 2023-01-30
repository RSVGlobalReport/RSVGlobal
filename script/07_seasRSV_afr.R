#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#YEAR ON YEAR RSV DYNAMICS IN AFRICA
#====================================================================

#weekly year on year RSV cases
plot13 = plotly::ggplotly(
  rsv_afr %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(cases = zoo::rollmean(cases, k = 3, fill = 0, align = 'right')) %>%
    dplyr::ungroup() %>%
    
    ggplot(aes(x = date, y = cases)) +
    geom_line() + 
    facet_wrap(. ~ country, ncol = 2, scales = "free_y") +
    scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
    theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
    theme(strip.background = element_rect(fill = "white")) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
    labs(title = "14-days rolling average RSV cases, 2017-2022", x = "Date", y = "RSV cases"))

htmlwidgets::saveWidget(as_widget(plot13), here("output", "yearOnyear_all_africa", file = "all_africa_yearOnyear.html"))


#====================================================================
#WEEKLY RSV DYNAMICS BY COUNTRY
#====================================================================

#weekly seasonal RSV dynamics for each year
for (i in c("Central African Republic", "Côte d'Ivoire", "Madagascar", "South Africa")) {
  plot14 = plotly::ggplotly(
    rsv_afr %>%
      dplyr::mutate(yr = as.factor(yr), cases = round(cases, digits = 0)) %>%
      filter(country == i) %>%
      ggplot(aes(x = wk, y = cases, color = yr)) +
      geom_line(size = 1) +
      scale_colour_brewer(palette = 7, direction = 1) + 
      labs(title = paste0("Weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      guides(color = guide_legend(title = "")) +
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow")))
  
  htmlwidgets::saveWidget(as_widget(plot14), here("output", "weekly_each_country", file = paste0("CountryWeekly_", i,".html")))
  
}


#====================================================================
#BEFORE AND AFTER COVID-19 RSV DYNAMICS BY COUNTRY
#====================================================================

#weekly seasonal RSV dynamics before and after COVID-19 by regions aggregated across all years

for (i in c("Central African Republic", "Côte d'Ivoire", "Madagascar", "South Africa")) {
  plot15 = plotly::ggplotly(
    rsv_afr %>%
      dplyr::mutate(covid = if_else(date < "2020-01-01", " PreCOVID (2017-19)", 
                                    if_else(year(date) == 2021, "2021", "2022"))) %>%
      filter(!is.na(covid), country == i) %>%
      group_by(country, wk, covid) %>%
      summarise(mcases = mean(cases, rm.na = TRUE)) %>%
      ungroup() %>%
      ggplot(aes(x = wk, y = mcases, group = covid, color = covid)) +
      geom_line(size = 1) + 
      scale_colour_brewer(palette = 7, direction = 1) + 
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
      labs(title = paste0("Mean weekly seasonal RSV cases in ", i), x = "Epi week", y = "RSV cases") +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow")) +
      guides(color = guide_legend(title = "")))
  
  htmlwidgets::saveWidget(as_widget(plot15), here("output", "covid_each_country", file = paste0("CountryCovid_", i,".html")))
  
}
  