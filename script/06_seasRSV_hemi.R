#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#====================================================================

#weekly year on year RSV cases
print(
  rsv_all %>%
    dplyr::group_by(hemi, date, wk) %>%
    dplyr::summarise(cases = mean(cases, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(hemi) %>%
    dplyr::mutate(cases = zoo::rollmean(cases, k = 3, fill = 0, align = 'right')) %>%
    dplyr::ungroup() %>%
    
    ggplot(aes(x = date, y = cases)) +
    geom_line() + 
    facet_wrap(. ~ hemi, scales = "free_y") +
    scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
    theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
    theme(strip.background = element_rect(fill = "light yellow")) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
    labs(title = "14-days rolling average RSV cases, 2017-2022 (*see countries included)", subtitle = "(Stratified by hemisphere)", x = "Date", y = "RSV cases")
)

#====================================================================
#====================================================================

#weekly seasonal RSV dynamics for each year
wkno1 = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
wkno2 = c(24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53)
wkno = c(24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)

rsv_all_h <-
  rsv_all %>%
  dplyr::group_by(hemi, yr, wk) %>%
  dplyr::summarise(cases = mean(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(hemi, yr) %>% 
  dplyr::mutate(seas = if_else((hemi == "NH") & wk %in% wkno2 & yr == 2017, "2017/18",
                               if_else((hemi == "NH") & wk %in% wkno1  & yr == 2018, "2017/18",
                                       if_else((hemi == "NH") & wk %in% wkno2 & yr == 2018, "2018/19",
                                               if_else((hemi == "NH") & wk %in% wkno1 & yr == 2019, "2018/19",
                                                       if_else((hemi == "NH") & wk %in% wkno2 & yr == 2019, "2019/20",
                                                               if_else((hemi == "NH") & wk %in% wkno1 & yr == 2020, "2019/20",
                                                                       if_else((hemi == "NH") & wk %in% wkno2 & yr == 2020, "2020/21",
                                                                               if_else((hemi == "NH") & wk %in% wkno1 & yr == 2021, "2020/21",
                                                                                       if_else((hemi == "NH") & wk %in% wkno2 & yr == 2021, "2021/22",
                                                                                               if_else((hemi == "NH") & wk %in% wkno1 & yr == 2022, "2021/22",
                                                                                                       if_else((hemi == "NH") & wk %in% wkno2 & yr == 2022, "2022/23",
                                                                                                               if_else((hemi == "NH") & wk %in% wkno1 & yr == 2023, "2022/23", NA_character_))))))))))))
  )

r1 <-
  rsv_all_h %>%
  dplyr::filter(hemi == "SH") %>% 
  ggplot(aes(x = wk, y = cases, group = yr, color = factor(yr))) +
  geom_line(size = 1) +
  facet_wrap(. ~ hemi, ncol = 1, scales = "free_y") +
  labs(title = "Mean weekly seasonal RSV cases", subtitle = "(Stratified by hemisphere & year)", x = "Epi week", y = "RSV cases")  +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = seq(1, 53, 4)) +
  theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow"))

r2 <-
  rsv_all_h %>%
  dplyr::filter(!is.na(seas)) %>%
  filter(hemi == "NH") %>% 
  ggplot(aes(x = factor(wk, levels(factor(wk))[c(wkno)]), y = cases, group = seas, color = factor(seas))) +
  geom_line(size = 1) +
  facet_wrap(. ~ hemi, ncol = 1, scales = "free_y") +
  labs(title = "", x = "Epi week", y = "")  +
  guides(color = guide_legend(title = "")) +
  scale_x_discrete(breaks = seq(1, 52, 4)) +
  theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow"))


print(r1 + r2)

#====================================================================
#====================================================================

#weekly seasonal RSV dynamics before/after COVID-19 by regions aggregated across years
rsv_all_i <-
  rsv_all %>%
  dplyr::mutate(covid = if_else(date < "2020-01-01", " Pre-C19 (2017-19)", if_else(year(date) == 2021, "2021", "2022"))) %>%
  dplyr::filter(!is.na(covid)) %>%
  dplyr::group_by(hemi, wk, covid) %>%
  dplyr::summarise(mcases = mean(cases, rm.na = TRUE)) %>%
  dplyr::ungroup()

s1 <- 
  rsv_all_i %>%
  dplyr::filter(hemi == "SH") %>% 
  ggplot(aes(x = wk, y = mcases, group = covid, color = covid)) +
  geom_line(size = 1) + 
  scale_x_continuous(breaks = seq(1, 53, 4)) +
  facet_wrap(. ~ hemi, ncol = 1, scales = "free_y") +
  theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(title = "Mean weekly seasonal RSV cases", subtitle = "(Stratified by hemisphere & COVID-19 phase)", x = "Epi week", y = "RSV cases") + 
  theme(legend.position = "none", strip.background = element_rect(fill = "light yellow")) +
  guides(color = guide_legend(title = ""))

s2 <- 
  rsv_all_i %>%
  dplyr::filter(hemi == "NH") %>% 
  ggplot(aes(x = factor(wk, levels(factor(wk))[c(wkno)]), y = mcases, group = covid, color = covid)) +
  geom_line(size = 1) + 
  scale_x_discrete(breaks = seq(1, 52, 4)) +
  facet_wrap(. ~ hemi, ncol = 1, scales = "free_y") +
  theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  labs(title = "", x = "Epi week", y = "") + 
  theme(legend.position = "right", strip.background = element_rect(fill = "light yellow")) +
  guides(color = guide_legend(title = ""))

print(s1 + s2)

