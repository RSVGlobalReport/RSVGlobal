#Deus & Dan (code adapted from Gigi - https://github.com/Gigi112/RSV_tutorials/blob/main/2_RSV_timing_explain/RSV_timing_tutorial.Rmd)
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#====================================================================

#merge the RSV onset datasets
rsv_onset <-
rows_append(rsv_onset_temp, rsv_onset_trop) %>% 
  left_join(climate %>% select(country, clim_zone)) %>%
  left_join(rsv_all %>% select(country, hemi, region) %>% distinct(.keep_all = TRUE)) %>%
  mutate(covper = if_else(covper == "2021/22", "y2021",
                          if_else(covper == "2022/23", "y2022",
                                  if_else(covper == "2021", "y2021",
                                          if_else(covper == "2022", "y2022", 
                                                  if_else(covper == "preCOVID-19", "precov", NA_character_))))))

#only consider countries with all the data e.g., preCovid, 2021, and 2022
rsv_onset <- 
  rsv_onset %>%
  left_join(
    rsv_onset %>%
      group_by(country) %>%
      tally()) %>%
  filter(n == 3)

#====================================================================
#RSV ONSET BY HEMISPHERE
#====================================================================

#loop in the specified vector content
for (i in c("Northern hemisphere", "Southern hemisphere")) {
  
  rsv_onset_hemi <-  
    rsv_onset %>% 
    filter(hemi == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    fill(precov, .direction = "up")

#reshape the onset datasets for scatter plotting
scatterXY <-
  left_join(
    left_join(
        rsv_onset_hemi %>%
        filter(is.na(y2021), is.na(y2022)) %>%
        select(country, precov, l_epiwk, u_epiwk) %>%
        rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
      
        rsv_onset_hemi %>%
        select(country, y2021, l_epiwk, u_epiwk) %>%
        filter(!is.na(y2021)) %>%
        rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
    
      rsv_onset_hemi %>%
      select(country, y2022, l_epiwk, u_epiwk) %>%
      filter(!is.na(y2022)) %>%
      rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk")) %>% 
  
  #compute accuracy (100%-mean absolute percentage error) for 2021/22 and 2022/23 seasons against preCovid
  mutate(A2021 = (1-round(MAPE(y2021, precov), digits = 3))*100,
         A2022 = (1-round(MAPE(y2022, precov), digits = 3))*100)

plot1 = plotly::ggplotly(
  scatterXY %>%
    mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
    ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
    geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
    geom_text(aes(x = 10, y = 50, label = paste0("α = ", A2021, "%")), color = "black", size = 6, fontface = "bold") +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    theme_bw(base_size = 14, base_family = 'Lato') +
    labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2021", title = paste0("RSV onset in the ", i)) +
    theme(legend.position = "bottom", legend.title = element_blank()))

htmlwidgets::saveWidget(as_widget(plot1), here("output", "onset_each_hemisphere", file = paste0(i,"_preCovid_vs_2021_22.html")))
unlink(paste0(here("output", "onset_each_hemisphere", paste0(i,"_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata


plot2 = plotly::ggplotly(
  scatterXY %>%
  mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
  geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_text(aes(x = 10, y = 50, label = paste0("α = ", A2022, "%")), color = "black", size = 6, fontface = "bold") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2022", title = paste0("RSV onset in the ", i)) +
  theme(legend.position = "bottom", legend.title = element_blank()))

htmlwidgets::saveWidget(as_widget(plot2), here("output", "onset_each_hemisphere", file = paste0(i,"_preCovid_vs_2022_23.html")))
unlink(paste0(here("output", "onset_each_hemisphere", paste0(i,"_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata

}


#====================================================================
#RSV ONSET BY WHO REGION
#====================================================================

#loop in the specified vector content
for (i in c("Africa", "North Americas", "South Americas", "Eastern Mediterranean", "Europe", "South East Asia", "Western Pacific")) {
  
  rsv_onset_reg <-  
    rsv_onset %>% 
    filter(region == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    fill(precov, .direction = "up")
  
  #reshape the onset datasets for scatter plotting
  scatterXY <-
    left_join(
      left_join(
        rsv_onset_reg %>%
          filter(is.na(y2021), is.na(y2022)) %>%
          select(country, precov, l_epiwk, u_epiwk) %>%
          rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        rsv_onset_reg %>%
          select(country, y2021, l_epiwk, u_epiwk) %>%
          filter(!is.na(y2021)) %>%
          rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      rsv_onset_reg %>%
        select(country, y2022, l_epiwk, u_epiwk) %>%
        filter(!is.na(y2022)) %>%
        rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk")) %>% 
    
    #compute accuracy (100%-mean absolute percentage error) for 2021/22 and 2022/23 seasons against preCovid
    mutate(A2021 = (1-round(MAPE(y2021, precov), digits = 3))*100,
           A2022 = (1-round(MAPE(y2022, precov), digits = 3))*100)
  
  plot1 = plotly::ggplotly(
    scatterXY %>%
      mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 10, y = 50, label = paste0("α = ", A2021, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2021", title = paste0("RSV onset in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()))
  
  htmlwidgets::saveWidget(as_widget(plot1), here("output", "onset_each_region", file = paste0(i,"_region_preCovid_vs_2021_22.html")))
  unlink(paste0(here("output", "onset_each_region", paste0(i,"_region_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata
  
  plot2 = plotly::ggplotly(
    scatterXY %>%
      mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 10, y = 50, label = paste0("α = ", A2022, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2022", title = paste0("RSV onset in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()))
  
  htmlwidgets::saveWidget(as_widget(plot2), here("output", "onset_each_region", file = paste0(i,"_region_preCovid_vs_2022_23.html")))
  unlink(paste0(here("output", "onset_each_region", paste0(i,"_region_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata
  
}


#====================================================================
#RSV ONSET BY CLIMATE ZONES
#====================================================================

#there are 5 classifications of climatic zones according to Köppen-Geiger climate classification system
#tropical, dry, temperate, continental, and polar
#countries here are classified as tropical (tropical, dry), temperate (continental, polar), and subtropical (tropical, temperate) to check if climate zones align with onset of RSV cases
#countries data can be found here (https://www.worlddata.info)

#loop in the specified vector content
for (i in c("Tropical", "Temperate", "Sub-tropical")) {
  
  rsv_onset_cz <-  
    rsv_onset %>% 
    filter(clim_zone == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    fill(precov, .direction = "up")
  
  #reshape the onset datasets for scatter plotting
  scatterXY <-
    left_join(
      left_join(
        rsv_onset_cz %>%
          filter(is.na(y2021), is.na(y2022)) %>%
          select(country, precov, l_epiwk, u_epiwk) %>%
          rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        rsv_onset_cz %>%
          select(country, y2021, l_epiwk, u_epiwk) %>%
          filter(!is.na(y2021)) %>%
          rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      rsv_onset_cz %>%
        select(country, y2022, l_epiwk, u_epiwk) %>%
        filter(!is.na(y2022)) %>%
        rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk"))  %>% 
    
    #compute accuracy (100%-mean absolute percentage error) for 2021/22 and 2022/23 seasons against preCovid
    mutate(A2021 = (1-round(MAPE(y2021, precov), digits = 3))*100,
           A2022 = (1-round(MAPE(y2022, precov), digits = 3))*100)
  
  plot1 = plotly::ggplotly(
    scatterXY %>%
      mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 10, y = 50, label = paste0("α = ", A2021, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2021", title = paste0("RSV onset in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()))
  
  htmlwidgets::saveWidget(as_widget(plot1), here("output", "onset_each_climatezone", file = paste0(i,"_climazone_preCovid_vs_2021_22.html")))
  unlink(paste0(here("output", "onset_each_climatezone", paste0(i,"_climazone_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata
  
  
  plot2 = plotly::ggplotly(
    scatterXY %>%
      mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 10, y = 50, label = paste0("α = ", A2022, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2022", title = paste0("RSV onset in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()))
  
  htmlwidgets::saveWidget(as_widget(plot2), here("output", "onset_each_climatezone", file = paste0(i,"_climazone_preCovid_vs_2022_23.html")))
  unlink(paste0(here("output", "onset_each_climatezone", paste0(i,"_climazone_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata
  
}


#====================================================================
#RSV ONSET BY UNITED STATES REGION
#====================================================================

#loop in the specified vector content
for (i in c("United States")) {
  
  rsv_onset_us <-  
    rsv_onset %>% 
    filter(region == "North Americas", (country == "United States" | country == "United States North East" | country == "United States South" | country == "United States West" | country == "United States Mid West")) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    fill(precov, .direction = "up")
  
  #reshape the onset datasets for scatter plotting
  scatterXY <-
    left_join(
      left_join(
        rsv_onset_us %>%
          filter(is.na(y2021), is.na(y2022)) %>%
          select(country, precov, l_epiwk, u_epiwk) %>%
          rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        rsv_onset_us %>%
          select(country, y2021, l_epiwk, u_epiwk) %>%
          filter(!is.na(y2021)) %>%
          rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      rsv_onset_us %>%
        select(country, y2022, l_epiwk, u_epiwk) %>%
        filter(!is.na(y2022)) %>%
        rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk")) %>% 
    
    #compute accuracy (100%-mean absolute percentage error) for 2021/22 and 2022/23 seasons against preCovid
    mutate(A2021 = (1-round(MAPE(y2021, precov), digits = 3))*100,
           A2022 = (1-round(MAPE(y2022, precov), digits = 3))*100)
  
  plot1 = plotly::ggplotly(
    scatterXY %>%
      mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 10, y = 50, label = paste0("α = ", A2021, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2021", title = paste0("RSV onset in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(title = "US region")))
  
  htmlwidgets::saveWidget(as_widget(plot1), here("output", "onset_each_USregion", file = paste0(i,"_USregion_preCovid_vs_2021_22.html")))
  unlink(paste0(here("output", "onset_each_USregion", paste0(i,"_USregion_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata
  
  
  plot2 = plotly::ggplotly(
    scatterXY %>%
      mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 10, y = 50, label = paste0("α = ", A2022, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2022", title = paste0("RSV onset in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(title = "US region")))
  
  htmlwidgets::saveWidget(as_widget(plot2), here("output", "onset_each_USregion", file = paste0(i,"_USregion_preCovid_vs_2022_23.html")))
  unlink(paste0(here("output", "onset_each_USregion", paste0(i,"_USregion_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata
  
}
