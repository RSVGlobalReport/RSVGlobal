#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#====================================================================

#merge the RSV onset datasets
rsv_onset <-
  dplyr::rows_append(rsv_onset_temp, rsv_onset_trop) %>% 
  dplyr::left_join(climate %>% dplyr::select(country, clim_zone)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi, region) %>% distinct(.keep_all = TRUE)) %>%
  dplyr::mutate(covper = if_else(covper == "2021/22", "y2021",
                          if_else(covper == "2022/23", "y2022",
                                  if_else(covper == "2021", "y2021",
                                          if_else(covper == "2022", "y2022",
                                                  if_else(covper == "preCOVID-19", "precov", NA_character_))))))


#====================================================================
#RSV ONSET BY OVERALL
#====================================================================

#loop in the specified vector content
rsv_onset_all <-
  rsv_onset %>% 
  dplyr::filter(!(country %in% c("United States North East", "United States South", "United States West", "United States Mid West"))) %>%
  dplyr::mutate(row = row_number()) %>%
  pivot_wider(names_from = covper, values_from = epiwk) %>%
  dplyr::select(everything(), -row)
  
  #reshape the onset datasets for scatter plotting
  scatterXY <-
    dplyr::left_join(
      dplyr::left_join(
        rsv_onset_all %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
          dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        rsv_onset_all %>%
          dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      rsv_onset_all %>%
        dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk"))  %>%
    
    dplyr::mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
           y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
           y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1]), digits = 3)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1], digits = 3)),
                  corr = abs(round((cor.circular(y2021x, y2022x))[1], digits = 3)))

#====================================================================
  
  plot1 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
      #geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr2021)), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2021", title = "RSV onset in all countries "))

  htmlwidgets::saveWidget(as_widget(plot1), here("output", "onset_all_countries", file = paste0("all_countries_preCovid_vs_2021_22.html")))
  unlink(paste0(here("output", "onset_all_countries", paste0("all_countries_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata
  
  plot2 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr2022)), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2022", title = "RSV onset in all countries"))

  htmlwidgets::saveWidget(as_widget(plot2), here("output", "onset_all_countries", file = paste0("all_countries_preCovid_vs_2022_23.html")))
  unlink(paste0(here("output", "onset_all_countries", paste0("all_countries_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata
  
  plot3 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(y2021 = round(y2021, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = y2021, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr)), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "RSV onset in 2021", y = "RSV onset in 2022", title = "RSV onset in all countries"))

  htmlwidgets::saveWidget(as_widget(plot3), here("output", "onset_all_countries", file = paste0("all_countries_2021_22_vs_2022_23.html")))
  unlink(paste0(here("output", "onset_all_countries", paste0("all_countries_2021_22_vs_2022_23_files"))), recursive = TRUE) #delete metadata
  

#====================================================================
#RSV ONSET BY HEMISPHERE
#====================================================================

#loop in the specified vector content
for (i in c("Northern hemisphere", "Southern hemisphere")) {
  
  rsv_onset_hemi <-  
    rsv_onset %>% 
    dplyr::filter(hemi == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    dplyr::select(everything(), -row)

#reshape the onset datasets for scatter plotting
scatterXY <-
  dplyr::left_join(
    dplyr::left_join(
        rsv_onset_hemi %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
          dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
      
        rsv_onset_hemi %>%
          dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
    
      rsv_onset_hemi %>%
      dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
      dplyr::filter(!is.na(y2022)) %>%
      dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk"))  %>%
  
  dplyr::mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
         y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
         y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
  
  dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1]), digits = 3)),
                corr2022 = abs(round((cor.circular(precovx, y2022x))[1], digits = 3)),
                corr = abs(round((cor.circular(y2021x, y2022x))[1], digits = 3)))

#====================================================================

plot1 = plotly::ggplotly(
  scatterXY %>%
    dplyr::mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
    ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
    geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
    #geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
    geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr2021)), color = "black", size = 6, fontface = "bold") +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    theme_bw(base_size = 14, base_family = 'Lato') +
    labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2021", title = paste0("RSV onset in the ", i)))

htmlwidgets::saveWidget(as_widget(plot1), here("output", "onset_each_hemisphere", file = paste0(i,"_preCovid_vs_2021_22.html")))
unlink(paste0(here("output", "onset_each_hemisphere", paste0(i,"_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata

plot2 = plotly::ggplotly(
  scatterXY %>%
    dplyr::mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
  #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr2022)), color = "black", size = 6, fontface = "bold") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2022", title = paste0("RSV onset in the ", i)))

htmlwidgets::saveWidget(as_widget(plot2), here("output", "onset_each_hemisphere", file = paste0(i,"_preCovid_vs_2022_23.html")))
unlink(paste0(here("output", "onset_each_hemisphere", paste0(i,"_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata

plot3 = plotly::ggplotly(
  scatterXY %>%
    dplyr::mutate(y2021 = round(y2021, digits = 1), y2022 = round(y2022, digits = 1)) %>%
    ggplot(aes(x = y2021, y = y2022, color = country), position = position_dodge(width = 0.5)) +
    geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
    #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
    geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr)), color = "black", size = 6, fontface = "bold") +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    theme_bw(base_size = 14, base_family = 'Lato') +
    labs(x = "RSV onset in 2021", y = "RSV onset in 2022", title = paste0("RSV onset in the ", i)))

htmlwidgets::saveWidget(as_widget(plot3), here("output", "onset_each_hemisphere", file = paste0(i,"_2021_22_vs_2022_23.html")))
unlink(paste0(here("output", "onset_each_hemisphere", paste0(i,"_2021_22_vs_2022_23_files"))), recursive = TRUE) #delete metadata
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
    dplyr::filter(clim_zone == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    dplyr::select(everything(), -row)

  #reshape the onset datasets for scatter plotting
  scatterXY <-
    dplyr::left_join(
      dplyr::left_join(
        rsv_onset_cz %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
          dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        rsv_onset_cz %>%
          dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      rsv_onset_cz %>%
        dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk"))  %>%
    
    dplyr::mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
           y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
           y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1]), digits = 3)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1], digits = 3)),
                  corr = abs(round((cor.circular(y2021x, y2022x))[1], digits = 3)))

  #====================================================================
  
  plot1 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
      #geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr2021)), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2021", title = paste0("RSV onset in the ", i)))

  htmlwidgets::saveWidget(as_widget(plot1), here("output", "onset_each_climatezone", file = paste0(i,"_climazone_preCovid_vs_2021_22.html")))
  unlink(paste0(here("output", "onset_each_climatezone", paste0(i,"_climazone_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata
  
  
  plot2 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr2022)), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2022", title = paste0("RSV onset in the ", i)))

  htmlwidgets::saveWidget(as_widget(plot2), here("output", "onset_each_climatezone", file = paste0(i,"_climazone_preCovid_vs_2022_23.html")))
  unlink(paste0(here("output", "onset_each_climatezone", paste0(i,"_climazone_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata
 
  
  plot3 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(y2021 = round(y2021, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = y2021, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr)), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "RSV onset in 2021", y = "RSV onset in 2022", title = paste0("RSV onset in the ", i)))
  
  htmlwidgets::saveWidget(as_widget(plot3), here("output", "onset_each_climatezone", file = paste0(i,"_climazone_2021_22_vs_2022_23.html")))
  unlink(paste0(here("output", "onset_each_climatezone", paste0(i,"_climazone_2021_22_vs_2022_23_files"))), recursive = TRUE) #delete metadata 
}


#====================================================================
#RSV ONSET BY UNITED STATES REGION
#====================================================================

#loop in the specified vector content
for (i in c("United States")) {
  
  rsv_onset_us <-  
    rsv_onset %>% 
    dplyr::filter(region == "North Americas", (country == "United States North East" | country == "United States South" | country == "United States West" | country == "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    dplyr::select(everything(), -row)
    
  #reshape the onset datasets for scatter plotting
  scatterXY <-
    dplyr::left_join(
      dplyr::left_join(
        rsv_onset_us %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
          dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        rsv_onset_us %>%
          dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      rsv_onset_us %>%
        dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk"))  %>%
    
    dplyr::mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
           y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
           y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1]), digits = 3)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1], digits = 3)),
                  corr = abs(round((cor.circular(y2021x, y2022x))[1], digits = 3)))

  #====================================================================
  
  plot1 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
      #geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr2021)), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2021", title = paste0("RSV onset in the ", i)))
  
  htmlwidgets::saveWidget(as_widget(plot1), here("output", "onset_each_USregion", file = paste0(i,"_USregion_preCovid_vs_2021_22.html")))
  unlink(paste0(here("output", "onset_each_USregion", paste0(i,"_USregion_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata
  
  
  plot2 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr2022)), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean onset", y = "RSV onset in 2022", title = paste0("RSV onset in the ", i)))
  
  htmlwidgets::saveWidget(as_widget(plot2), here("output", "onset_each_USregion", file = paste0(i,"_USregion_preCovid_vs_2022_23.html")))
  unlink(paste0(here("output", "onset_each_USregion", paste0(i,"_USregion_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata
  
  
  plot3 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(y2021 = round(y2021, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = y2021, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 46, y = 8, label = paste0("c = ", corr)), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "RSV onset in 2021", y = "RSV onset in 2022", title = paste0("RSV onset in the ", i)))
  
  htmlwidgets::saveWidget(as_widget(plot3), here("output", "onset_each_USregion", file = paste0(i,"_USregion_2021_22_vs_2022_23.html")))
  unlink(paste0(here("output", "onset_each_USregion", paste0(i,"_USregion_2021_22_vs_2022_23_files"))), recursive = TRUE) #delete metadata 
}
