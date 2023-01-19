#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#====================================================================

#weekly year on year RSV cases in the US
print(
  rsv_usa_nat %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(cases = zoo::rollmean(cases, k = 3, fill = 0, align = 'right')) %>%
    dplyr::ungroup() %>%
    
    ggplot(aes(x = date, y = cases)) +
    geom_line() + 
    facet_wrap(. ~ country, ncol = 4, scales = "free_y") +
    scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
    theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
    theme(strip.background = element_rect(fill = "light yellow")) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
    labs(title = "14-days rolling average RSV cases, 2017-2022", subtitle = "(Stratified by European country)", x = "Date", y = "RSV cases")
)

#====================================================================
#====================================================================

#weekly seasonal RSV dynamics for each year (nationala and regions)
wkno1 = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
wkno2 = c(24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53)
wkno = c(24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)

rsv_usa_nat <-
  rsv_usa_nat %>%
  dplyr::group_by(country, yr) %>% 
  dplyr::mutate(seas = if_else(wk %in% wkno2 & yr == 2017, "2017/18",
                               if_else( wk %in% wkno1  & yr == 2018, "2017/18",
                                       if_else(wk %in% wkno2 & yr == 2018, "2018/19",
                                               if_else(wk %in% wkno1 & yr == 2019, "2018/19",
                                                       if_else(wk %in% wkno2 & yr == 2019, "2019/20",
                                                               if_else(wk %in% wkno1 & yr == 2020, "2019/20",
                                                                       if_else(wk %in% wkno2 & yr == 2020, "2020/21",
                                                                               if_else(wk %in% wkno1 & yr == 2021, "2020/21",
                                                                                       if_else(wk %in% wkno2 & yr == 2021, "2021/22",
                                                                                               if_else(wk %in% wkno1 & yr == 2022, "2021/22",
                                                                                                       if_else(wk %in% wkno2 & yr == 2022, "2022/23",
                                                                                                               if_else(wk %in% wkno1 & yr == 2023, "2022/23", NA_character_))))))))))))
  ) 

rsv_usa_reg <-
  rsv_usa_reg %>%
  dplyr::group_by(country, yr) %>% 
  dplyr::mutate(seas = if_else(wk %in% wkno2 & yr == 2017, "2017/18",
                               if_else(wk %in% wkno1  & yr == 2018, "2017/18",
                                       if_else(wk %in% wkno2 & yr == 2018, "2018/19",
                                               if_else(wk %in% wkno1 & yr == 2019, "2018/19",
                                                       if_else(wk %in% wkno2 & yr == 2019, "2019/20",
                                                               if_else(wk %in% wkno1 & yr == 2020, "2019/20",
                                                                       if_else(wk %in% wkno2 & yr == 2020, "2020/21",
                                                                               if_else(wk %in% wkno1 & yr == 2021, "2020/21",
                                                                                       if_else(wk %in% wkno2 & yr == 2021, "2021/22",
                                                                                               if_else(wk %in% wkno1 & yr == 2022, "2021/22",
                                                                                                       if_else(wk %in% wkno2 & yr == 2022, "2022/23",
                                                                                                               if_else(wk %in% wkno1 & yr == 2023, "2022/23", NA_character_))))))))))))
  )

#weekly seasonal RSV dynamics for each year
print(
  rbind(rsv_usa_nat, rsv_usa_reg) %>%
  dplyr::mutate(regionUS = factor(regionUS, levels = c("National", "North East", "Mid West", "West", "Florida", "South"))) %>%
  ggplot(aes(x = factor(wk, levels(factor(wk))[c(wkno)]), y = cases, group = seas, color = factor(seas))) +
  geom_line(size = 1) +
  facet_wrap(. ~ regionUS, ncol = 3, scales = "free_y") +
  labs(title = "Weekly seasonal RSV cases", subtitle = "(Stratified by US region & year)", x = "Week", y = "RSV cases")  +
  guides(color = guide_legend(title = "")) +
  scale_x_discrete(breaks = seq(1, 52, 4)) +
  theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow"))
)

#====================================================================
#====================================================================

#weekly seasonal RSV dynamics before and after COVID-19 by regions aggregated across all years
print(
  rbind(rsv_usa_nat, rsv_usa_reg) %>%
    mutate(regionUS = factor(regionUS, levels = c("National", "North East", "Mid West", "West", "South", "Florida"))) %>%

    mutate(covid = if_else(date < "2020-01-01", "Pre-C19 (2017-19)", if_else(date >= "2021-01-01" , "Post-C19 (2021-22)", NA_character_))) %>%
    filter(!is.na(covid), !is.na(seas)) %>%
    group_by(regionUS, wk, covid) %>%
    summarise(mcases = mean(cases, rm.na = TRUE)) %>%
    ungroup() %>%
    
    ggplot(aes(x = factor(wk, levels(factor(wk))[c(wkno)]), y = mcases, group = covid, color = covid)) +
    geom_line(size = 1) + 
    scale_x_discrete(breaks = seq(1, 52, 4)) +
    facet_wrap(. ~ regionUS, ncol = 3, scales = "free_y") +
    theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
    labs(title = "Mean weekly seasonal RSV cases", subtitle = "(Stratified by US region & Covid-19 phase)", x = "Week", y = "RSV cases") + 
    theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow")) +
    guides(color = guide_legend(title = ""))
)
