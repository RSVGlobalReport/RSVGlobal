#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#====================================================================

#set strings as factors to false globally
options(stringsAsFactors = FALSE)

#read the CDC RSV update file/dataset into R (US regional data from 2021 onwards)
#source (https://www.cdc.gov/surveillance/nrevss/rsv/region.html)
rsv_usaNE <- runIfExpired('usa_rsv/rsv2021NE+', maxage = 168, ~readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg1.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 1(5 week Average)"]]
rsv_usaMW <- runIfExpired('usa_rsv/rsv2021MW+', maxage = 168, ~readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg2.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 2(5 week Average)"]]
rsv_usaSo <- runIfExpired('usa_rsv/rsv2021So+', maxage = 168, ~readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg3.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 3(5 week Average)"]]
rsv_usaWe <- runIfExpired('usa_rsv/rsv2021We+', maxage = 168, ~readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg4.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 4(5 week Average)"]]
rsv_usaFl <- runIfExpired('usa_rsv/rsv2021Fl+', maxage = 168, ~readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_FL.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Data for Florida(5 week Average)"]]

#data wrangling
rsv_usa_reg <-
  rbind(
    rsv_usaNE %>% 
      dplyr::select(RepWeekDate, `PCR Detections`) %>% #we are reading Antigen test results only but its incorrectly labeled as PCR from source data
      dplyr::rename("date" = "RepWeekDate", cases = `PCR Detections`) %>%
      dplyr::mutate(regionUS = "North East"),

    rsv_usaMW %>% 
      dplyr::select(RepWeekDate, `PCR Detections`) %>% #we are reading Antigen test results only but its incorrectly labeled as PCR from source data
      dplyr::rename("date" = "RepWeekDate", cases = `PCR Detections`) %>%
      dplyr::mutate(regionUS = "Mid West"),

    rsv_usaSo %>% 
      dplyr::select(RepWeekDate, `PCR Detections`) %>% #we are reading Antigen test results only but its incorrectly labeled as PCR from source data
      dplyr::rename("date" = "RepWeekDate", cases = `PCR Detections`) %>%
      dplyr::mutate(regionUS = "South"),

    rsv_usaWe %>% 
      dplyr::select(RepWeekDate, `PCR Detections`) %>% #we are reading Antigen test results only but its incorrectly labeled as PCR from source data
      dplyr::rename("date" = "RepWeekDate", cases = `PCR Detections`) %>%
      dplyr::mutate(regionUS = "West"),

    rsv_usaFl %>% 
      dplyr::select(RepWeekDate, `PCR Detections`) %>% #we are reading Antigen test results only but its incorrectly labeled as PCR from source data
      dplyr::rename("date" = "RepWeekDate", cases = `PCR Detections`) %>%
      dplyr::mutate(regionUS = "Florida")
    ) %>%
  
dplyr::mutate(cases = as.integer(cases),
              date = lubridate::mdy(date),
              hemi = "NH",
              region = "AMR",
              country = "United States") %>%
  dplyr::select(hemi, region, country, regionUS, date, cases)

#view structure of data
str(rsv_usa_reg)

#expand the dataset to have single case per row
rsv_usa_reg <- 
  rsv_usa_reg %>% 
  type.convert(as.is = TRUE) %>% 
  uncount(cases)

#view structure of data
str(rsv_usa_reg)

#assign data types to variables
rsv_usa_reg <-
  rsv_usa_reg %>%
  dplyr::mutate(hemi = factor(hemi),
                region = factor(region),
                country = country,
                region = region,
                date = lubridate::ymd(date))

# view structure of data
str(rsv_usa_reg)

#properly index by week to have 0 or some observed number of cases in sequential weeks
rsv_usa_reg <- 
  rsv_usa_reg %>% 
  tidyr::drop_na(date) %>%
  dplyr::group_by(region, hemi, country, regionUS, date) %>%
  dplyr::mutate(wkcases = floor_date(date, unit = "week")) %>% 
  dplyr::count(wkcases) %>% 
  dplyr::ungroup() %>%
  dplyr::arrange(country, regionUS, date) %>%
  tidyr::complete(wkcases = seq.Date(from = min(wkcases), to = max(wkcases), by = "week"), nesting(region, hemi, country), fill = list(n = 0L)) %>%
  dplyr::select(hemi, region, country, regionUS, wkcases, n) %>%
  dplyr::rename("date" = "wkcases", "cases" = "n") %>%
  dplyr::mutate(yr = lubridate::year(date),
                mo = lubridate::month(date),
                wk = lubridate::week(date))

#check for duplicates by country and date
rsv_usa_reg %>% 
  janitor::get_dupes(regionUS, date)

rsv_usa_reg <- 
  rsv_usa_reg %>% 
  dplyr::filter(yr >= 2017) %>%
  dplyr::distinct(regionUS, date, .keep_all = TRUE) %>%
  select(hemi, region, country, date, cases, yr, mo, wk, regionUS)


#====================================================================
#====================================================================

#read the CDC RSV update file/dataset into R (US national data from 2021 onwards)
#source (https://www.cdc.gov/surveillance/nrevss/rsv/region.html)
rsv_usa1 <- runIfExpired('usa_rsv/rsv2021+', maxage = 168, ~readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14Num_Nat.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for the US"]]

#data wrangling
rsv_usa1 <- 
rsv_usa1 %>% 
  dplyr::select(RepWeekDate, `PCR Detections`) %>% #we are reading Antigen test results only but its incorrectly labeled as PCR from source data
  dplyr::rename("date" = "RepWeekDate", cases = `PCR Detections`) %>%
  dplyr::mutate(cases = as.integer(cases),
                date = lubridate::mdy(date),
                hemi = "NH",
                region = "AMR",
                country = "United States") %>%
  dplyr::select(hemi, region, country, date, cases)

#view structure of data
str(rsv_usa1)

#expand the dataset to have single case per row
rsv_usa1 <- 
  rsv_usa1 %>% 
  type.convert(as.is = TRUE) %>% 
  uncount(cases)

#view structure of data
str(rsv_usa1)

#assign data types to variables
rsv_usa1 <-
  rsv_usa1 %>%
  dplyr::mutate(hemi = factor(hemi),
                region = factor(region),
                country = country,
                date = lubridate::ymd(date))

# view structure of data
str(rsv_usa1)

#====================================================================
#====================================================================

#read the CDC RSV update file/dataset into R (US national data from 2020 backwards)
#source (https://healthdata.gov/dataset/Respiratory-Syncytial-Virus-Laboratory-Data-NREVSS/7zgq-bp9w)
rsv_usa2 <- runIfExpired('usa_rsv/rsv2020-', maxage = 168, ~read.csv(curl("https://data.cdc.gov/api/views/52kb-ccu2/rows.csv?accessType=DOWNLOAD")))

#get the required variables
rsv_usa2 <-
  rsv_usa2 %>%
  dplyr::filter(!is.na(RSV.Detections)) %>%
  dplyr::select(Week.ending.Date, RSV.Detections) %>%
  dplyr::mutate(hemi = "NH",
                region = "AMR",
                country = "United States",
                Week.ending.Date = lubridate::dmy(Week.ending.Date)) %>%
  dplyr::rename("date"= Week.ending.Date,
                "cases" = RSV.Detections) %>%
  dplyr::select(hemi, region, country, date, cases)

#view structure of data
str(rsv_usa2)

#expand the dataset to have single case per row
rsv_usa2 <- 
  rsv_usa2 %>% 
  type.convert(as.is = TRUE) %>% 
  uncount(cases)

#view structure of data
str(rsv_usa2)

#assign data types to variables
rsv_usa2 <-
  rsv_usa2 %>%
  dplyr::mutate(hemi = factor(hemi),
                region = factor(region),
                country = country,
                date = lubridate::ymd(date))

# view structure of data
str(rsv_usa2)

#combine dataset from 2020 backwards (rsv_usa2) and 2021 onwards (rsv_usa1)
rsv_usa_nat <-
  rbind(
    rsv_usa1,
    rsv_usa2
    )

#properly index by week to have 0 or some observed number of cases in sequential weeks
rsv_usa_nat <- 
  rsv_usa_nat %>% 
  tidyr::drop_na(date) %>%
  dplyr::group_by(region, hemi, country, date) %>%
  dplyr::mutate(wkcases = floor_date(date, unit = "week")) %>% 
  dplyr::count(wkcases) %>% 
  dplyr::ungroup() %>%
  dplyr::arrange(country, date) %>%
  tidyr::complete(wkcases = seq.Date(from = min(wkcases), to = max(wkcases), by = "week"), nesting(region, hemi, country), fill = list(n = 0L)) %>%
  dplyr::select(hemi, region, country, wkcases, n) %>%
  dplyr::rename("date" = "wkcases", "cases" = "n") %>%
  dplyr::mutate(yr = lubridate::year(date),
                mo = lubridate::month(date),
                wk = lubridate::week(date))

#check for duplicates by country and date
rsv_usa_nat %>% 
  janitor::get_dupes(country, date)

rsv_usa_nat <- 
  rsv_usa_nat %>% 
  dplyr::filter(yr >= 2017) %>%
  dplyr::distinct(country, date, .keep_all = TRUE) %>%
  mutate(regionUS = "National")


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
