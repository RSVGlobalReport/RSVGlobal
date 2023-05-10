#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#RSV peak BY OVERALL
#====================================================================

#loop in the specified vector content
rsv_peak_all <-
  rsv_peak2 %>% 
  dplyr::filter(!(country %in% c("United States North East", "United States South", "United States West", "United States Mid West"))) %>%
  dplyr::mutate(row = row_number()) %>%
  pivot_wider(names_from = covper, values_from = epiwk) %>%
  dplyr::select(everything(), -row)
  
  #reshape the peak datasets for scatter plotting
  scatterXY <-
    dplyr::left_join(
      dplyr::left_join(
        rsv_peak_all %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
          dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        rsv_peak_all %>%
          dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      rsv_peak_all %>%
        dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk"))  %>%
    
    dplyr::mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
                  y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
                  y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0)),
                  corr = abs(round((cor.circular(y2021x, y2022x))[1]*100, digits = 0)))

#====================================================================
  
  plot1 = plotly::ggplotly(
    scatterXY %>%
      mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
      #geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr2021, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean peak", y = "RSV peak in 2021", title = "RSV peak in all countries ") +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(title = "Countries")))
  
  htmlwidgets::saveWidget(as_widget(plot1), here("output", "peak_all_countries", file = paste0("all_countries_preCovid_vs_2021_22.html")))
  unlink(paste0(here("output", "peak_all_countries", paste0("all_countries_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata
  
  plot2 = plotly::ggplotly(
    scatterXY %>%
      mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr2022, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean peak", y = "RSV peak in 2022", title = "RSV peak in all countries") +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(title = "Countries")))
  
  htmlwidgets::saveWidget(as_widget(plot2), here("output", "peak_all_countries", file = paste0("all_countries_preCovid_vs_2022_23.html")))
  unlink(paste0(here("output", "peak_all_countries", paste0("all_countries_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata
  
  plot3 = plotly::ggplotly(
    scatterXY %>%
      mutate(y2021 = round(y2021, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = y2021, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "RSV peak in 2021", y = "RSV peak in 2022", title = "RSV peak in all countries") +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(title = "Countries")))
  
  htmlwidgets::saveWidget(as_widget(plot3), here("output", "peak_all_countries", file = paste0("all_countries_2021_22_vs_2022_23.html")))
  unlink(paste0(here("output", "peak_all_countries", paste0("all_countries_2021_22_vs_2022_23_files"))), recursive = TRUE) #delete metadata
  

#====================================================================
#RSV peak BY HEMISPHERE
#====================================================================

#loop in the specified vector content
for (i in c("Northern hemisphere", "Southern hemisphere")) {
  
  rsv_peak_hemi <-  
    rsv_peak2 %>% 
    dplyr::filter(hemi == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    dplyr::select(everything(), -row)

#reshape the peak datasets for scatter plotting
scatterXY <-
  dplyr::left_join(
    dplyr::left_join(
        rsv_peak_hemi %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
          dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
      
        rsv_peak_hemi %>%
          dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
    
      rsv_peak_hemi %>%
      dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
      dplyr::filter(!is.na(y2022)) %>%
      dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk"))  %>%
  
  dplyr::mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
                y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
                y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
  
  dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0)),
                corr = abs(round((cor.circular(y2021x, y2022x))[1]*100, digits = 0)))

#====================================================================

plot1 = plotly::ggplotly(
  scatterXY %>%
    dplyr::mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
    ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
    geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
    #geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
    geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr2021, "%")), color = "black", size = 6, fontface = "bold") +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    theme_bw(base_size = 14, base_family = 'Lato') +
    labs(x = "PreCOVID-19 mean peak", y = "RSV peak in 2021", title = paste0("RSV peak in the ", i)) +
    theme(legend.position = "bottom", legend.title = element_blank()))

htmlwidgets::saveWidget(as_widget(plot1), here("output", "peak_each_hemisphere", file = paste0(i,"_preCovid_vs_2021_22.html")))
unlink(paste0(here("output", "peak_each_hemisphere", paste0(i,"_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata

plot2 = plotly::ggplotly(
  scatterXY %>%
    dplyr::mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
    ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
    geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
    #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
    geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr2022, "%")), color = "black", size = 6, fontface = "bold") +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    theme_bw(base_size = 14, base_family = 'Lato') +
    labs(x = "PreCOVID-19 mean peak", y = "RSV peak in 2022", title = paste0("RSV peak in the ", i)) +
    theme(legend.position = "bottom", legend.title = element_blank()))

htmlwidgets::saveWidget(as_widget(plot2), here("output", "peak_each_hemisphere", file = paste0(i,"_preCovid_vs_2022_23.html")))
unlink(paste0(here("output", "peak_each_hemisphere", paste0(i,"_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata

plot3 = plotly::ggplotly(
  scatterXY %>%
    dplyr::mutate(y2021 = round(y2021, digits = 1), y2022 = round(y2022, digits = 1)) %>%
    ggplot(aes(x = y2021, y = y2022, color = country), position = position_dodge(width = 0.5)) +
    geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
    #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
    geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr, "%")), color = "black", size = 6, fontface = "bold") +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
    theme_bw(base_size = 14, base_family = 'Lato') +
    labs(x = "RSV peak in 2021", y = "RSV peak in 2022", title = paste0("RSV peak in the ", i)) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(color = guide_legend(title = "Countries")))

htmlwidgets::saveWidget(as_widget(plot3), here("output", "peak_each_hemisphere", file = paste0(i,"_2021_22_vs_2022_23.html")))
unlink(paste0(here("output", "peak_each_hemisphere", paste0(i,"_2021_22_vs_2022_23_files"))), recursive = TRUE) #delete metadata
}

  
#====================================================================
#RSV peak BY WHO REGION
#====================================================================

#loop in the specified vector content
for (i in c("Africa", "North Americas", "South Americas", "Eastern Mediterranean", "Europe", "South East Asia", "Western Pacific")) {
  
  rsv_peak_reg <-  
    rsv_peak2 %>% 
    dplyr::filter(region == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    dplyr::select(everything(), -row)
  
  #reshape the peak datasets for scatter plotting
  scatterXY <-
    dplyr::left_join(
      dplyr::left_join(
        rsv_peak_reg %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
          dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        rsv_peak_reg %>%
          dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      rsv_peak_reg %>%
        dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk")) %>%
    
    dplyr::mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
           y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
           y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0)),
                  corr = abs(round((cor.circular(y2021x, y2022x))[1]*100, digits = 0)))

  #====================================================================
  
  plot1 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
      #geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr2021, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean peak", y = "RSV peak in 2021", title = paste0("RSV peak in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()))
  
  htmlwidgets::saveWidget(as_widget(plot1), here("output", "peak_each_region", file = paste0(i,"_region_preCovid_vs_2021_22.html")))
  unlink(paste0(here("output", "peak_each_region", paste0(i,"_region_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata
  
  plot2 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr2022, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean peak", y = "RSV peak in 2022", title = paste0("RSV peak in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()))
  
  htmlwidgets::saveWidget(as_widget(plot2), here("output", "peak_each_region", file = paste0(i,"_region_preCovid_vs_2022_23.html")))
  unlink(paste0(here("output", "peak_each_region", paste0(i,"_region_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata
  
  plot3 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(y2021 = round(y2021, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = y2021, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "RSV peak in 2021", y = "RSV peak in 2022", title = paste0("RSV peak in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(title = "Countries")))
  
  htmlwidgets::saveWidget(as_widget(plot3), here("output", "peak_each_region", file = paste0(i,"_region_2021_22_vs_2022_23.html")))
  unlink(paste0(here("output", "peak_each_region", paste0(i,"_region_2021_22_vs_2022_23_files"))), recursive = TRUE) #delete metadata
  }

#====================================================================
#RSV peak BY CLIMATE ZONES
#====================================================================

#there are 5 classifications of climatic zones according to Köppen-Geiger climate classification system
#tropical, dry, temperate, continental, and polar
#countries here are classified as tropical (tropical, dry), temperate (continental, polar), and subtropical (tropical, temperate) to check if climate zones align with peak of RSV cases
#countries data can be found here (https://www.worlddata.info)

#loop in the specified vector content
for (i in c("Tropical", "Temperate", "Sub-tropical")) {
  
  rsv_peak_cz <-  
    rsv_peak2 %>% 
    dplyr::filter(clim_zone == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    dplyr::select(everything(), -row)

  #reshape the peak datasets for scatter plotting
  scatterXY <-
    dplyr::left_join(
      dplyr::left_join(
        rsv_peak_cz %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
          dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        rsv_peak_cz %>%
          dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      rsv_peak_cz %>%
        dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk"))  %>%
    
    dplyr::mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
           y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
           y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0)),
                  corr = abs(round((cor.circular(y2021x, y2022x))[1]*100, digits = 0)))

  #====================================================================
  
  plot1 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
      #geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr2021, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean peak", y = "RSV peak in 2021", title = paste0("RSV peak in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()))
  
  htmlwidgets::saveWidget(as_widget(plot1), here("output", "peak_each_climatezone", file = paste0(i,"_climazone_preCovid_vs_2021_22.html")))
  unlink(paste0(here("output", "peak_each_climatezone", paste0(i,"_climazone_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata
  
  
  plot2 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr2022, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean peak", y = "RSV peak in 2022", title = paste0("RSV peak in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()))
  
  htmlwidgets::saveWidget(as_widget(plot2), here("output", "peak_each_climatezone", file = paste0(i,"_climazone_preCovid_vs_2022_23.html")))
  unlink(paste0(here("output", "peak_each_climatezone", paste0(i,"_climazone_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata
 
  
  plot3 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(y2021 = round(y2021, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = y2021, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "RSV peak in 2021", y = "RSV peak in 2022", title = paste0("RSV peak in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(title = "Countries")))
  
  htmlwidgets::saveWidget(as_widget(plot3), here("output", "peak_each_climatezone", file = paste0(i,"_climazone_2021_22_vs_2022_23.html")))
  unlink(paste0(here("output", "peak_each_climatezone", paste0(i,"_climazone_2021_22_vs_2022_23_files"))), recursive = TRUE) #delete metadata 
}


#====================================================================
#RSV peak BY UNITED STATES REGION
#====================================================================

#loop in the specified vector content
for (i in c("United States")) {
  
  rsv_peak_us <-  
    rsv_peak2 %>% 
    dplyr::filter(region == "North Americas", (country == "United States North East" | country == "United States South" | country == "United States West" | country == "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    dplyr::select(everything(), -row)
    
  #reshape the peak datasets for scatter plotting
  scatterXY <-
    dplyr::left_join(
      dplyr::left_join(
        rsv_peak_us %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
          dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        rsv_peak_us %>%
          dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      rsv_peak_us %>%
        dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk"))  %>%
    
    dplyr::mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
           y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
           y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0)),
                  corr = abs(round((cor.circular(y2021x, y2022x))[1]*100, digits = 0)))

  #====================================================================
  
  plot1 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2021 = round(y2021, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, shape = 4, stroke = 1, position = position_dodge(width = 0.5)) +
      #geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr2021, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean peak", y = "RSV peak in 2021", title = paste0("RSV peak in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(title = "US region")))
  
  htmlwidgets::saveWidget(as_widget(plot1), here("output", "peak_each_USregion", file = paste0(i,"_USregion_preCovid_vs_2021_22.html")))
  unlink(paste0(here("output", "peak_each_USregion", paste0(i,"_USregion_preCovid_vs_2021_22_files"))), recursive = TRUE) #delete metadata
  
  
  plot2 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(precov = round(precov, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr2022, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "PreCOVID-19 mean peak", y = "RSV peak in 2022", title = paste0("RSV peak in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(title = "US region")))
  
  htmlwidgets::saveWidget(as_widget(plot2), here("output", "peak_each_USregion", file = paste0(i,"_USregion_preCovid_vs_2022_23.html")))
  unlink(paste0(here("output", "peak_each_USregion", paste0(i,"_USregion_preCovid_vs_2022_23_files"))), recursive = TRUE) #delete metadata
  
  
  plot3 = plotly::ggplotly(
    scatterXY %>%
      dplyr::mutate(y2021 = round(y2021, digits = 1), y2022 = round(y2022, digits = 1)) %>%
      ggplot(aes(x = y2021, y = y2022, color = country), position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 1) +
      #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
      geom_text(aes(x = 50, y = 10, label = paste0("r = ", corr, "%")), color = "black", size = 6, fontface = "bold") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      scale_y_continuous(breaks = seq(0, 52, 4), limits = c(0,52)) +
      theme_bw(base_size = 14, base_family = 'Lato') +
      labs(x = "RSV peak in 2021", y = "RSV peak in 2022", title = paste0("RSV peak in the ", i)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(title = "Countries")))
  
  htmlwidgets::saveWidget(as_widget(plot3), here("output", "peak_each_USregion", file = paste0(i,"_USregion_2021_22_vs_2022_23.html")))
  unlink(paste0(here("output", "peak_each_USregion", paste0(i,"_USregion_2021_22_vs_2022_23_files"))), recursive = TRUE) #delete metadata 
}
