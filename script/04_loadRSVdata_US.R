#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================
#====================================================================

#set strings as factors to false globally
options(stringsAsFactors = FALSE)

#read the CDC RSV update file/dataset into R (US regional data from 2021 onwards)
#source (https://www.cdc.gov/surveillance/nrevss/rsv/region.html)
rsv_usaNE <- runIfExpired('usa_rsv/rsv2021NE+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg1.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 1(5 week Average)"]]
rsv_usaMW <- runIfExpired('usa_rsv/rsv2021MW+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg2.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 2(5 week Average)"]]
rsv_usaSo <- runIfExpired('usa_rsv/rsv2021So+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg3.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 3(5 week Average)"]]
rsv_usaWe <- runIfExpired('usa_rsv/rsv2021We+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg4.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 4(5 week Average)"]]
rsv_usaFl <- runIfExpired('usa_rsv/rsv2021Fl+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_FL.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Data for Florida(5 week Average)"]]

#data wrangling
rsv_usa_reg1 <-
  base::rbind(
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
      dplyr::mutate(regionUS = "South")
  ) %>%
  
  dplyr::mutate(cases = as.integer(cases),
                date = lubridate::mdy(date),
                hemi = "NH",
                region = "AMR",
                country = "United States") %>%
  dplyr::select(hemi, region, country, regionUS, date, cases)

base::rm(rsv_usaNE, rsv_usaMW, rsv_usaSo, rsv_usaWe, rsv_usaFl)

#view structure of data
utils::str(rsv_usa_reg1)

#expand the dataset to have single case per row
rsv_usa_reg1 <- 
  rsv_usa_reg1 %>% 
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(cases)

#view structure of data
utils::str(rsv_usa_reg1)

#assign data types to variables
rsv_usa_reg1 <-
  rsv_usa_reg1 %>%
  dplyr::mutate(hemi = factor(hemi),
                region = factor(region),
                country = country,
                regionUS = regionUS,
                date = lubridate::ymd(date))

# view structure of data
utils::str(rsv_usa_reg1)

#====================================================================
#====================================================================

#read the CDC RSV update file/dataset into R (US regional data from 2020 backwards)
#source (https://healthdata.gov/dataset/Respiratory-Syncytial-Virus-Laboratory-Data-NREVSS/7zgq-bp9w)
rsv_usa_reg2 <- runIfExpired('usa_rsv/rsv2020-', maxage = 168, ~read.csv(curl("https://data.cdc.gov/api/views/52kb-ccu2/rows.csv?accessType=DOWNLOAD")))

#get the required variables
rsv_usa_reg2 <-
  rsv_usa_reg2 %>%
  dplyr::filter(!is.na(RSV.Detections)) %>%
  dplyr::select(Week.ending.Date, HHS.region, RSV.Detections) %>%
  dplyr::mutate(hemi = "NH",
                region = "AMR",
                country = "United States",
                Week.ending.Date = lubridate::dmy(Week.ending.Date),
                regionUS = if_else(HHS.region == 1 | HHS.region == 2, "North East", #collapse HHS regions into census regions (as only census regional data are scrappable post 2020)
                                   if_else(HHS.region == 3 | HHS.region == 4 | HHS.region == 6, "South",
                                           if_else(HHS.region == 5 | HHS.region == 7, "Mid West", "West")))) %>%
  dplyr::rename("date"= Week.ending.Date,
                "cases" = RSV.Detections) %>%
  dplyr::select(hemi, region, country, regionUS, date, cases)
  
#view structure of data
utils::str(rsv_usa_reg2)

#expand the dataset to have single case per row
rsv_usa_reg2 <- 
  rsv_usa_reg2 %>% 
  utils::type.convert(as.is = TRUE) %>% 
  tidyr:: uncount(cases)

#view structure of data
utils::str(rsv_usa_reg2)

#assign data types to variables
rsv_usa_reg2 <-
  rsv_usa_reg2 %>%
  dplyr::mutate(hemi = factor(hemi),
                region = factor(region),
                country = country,
                regionUS = regionUS,
                date = lubridate::ymd(date))

# view structure of data
utils::str(rsv_usa_reg2)

#====================================================================
#====================================================================

#combine dataset from 2020 backwards (rsv_usa2) and 2021 onwards (rsv_usa1)
rsv_usa_reg <-
  dplyr::rows_append(
    rsv_usa_reg1,
    rsv_usa_reg2)
base::rm(rsv_usa_reg1, rsv_usa_reg2)

#properly index by week to have 0 or some observed number of cases in sequential weeks
rsv_usa_reg <- 
  rsv_usa_reg %>% 
  tidyr::drop_na(date) %>%
  dplyr::group_by(region, hemi, country, regionUS, date) %>%
  dplyr::mutate(wkcases = floor_date(date, unit = "week")) %>% 
  dplyr::count(wkcases) %>% 
  dplyr::ungroup() %>%
  dplyr::arrange(country, regionUS, date) %>%
  tidyr::complete(wkcases = seq.Date(from = min(wkcases), to = max(wkcases), by = "week"), nesting(region, hemi, country, regionUS), fill = list(n = 0L)) %>%
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
  dplyr::select(hemi, region, country, date, cases, yr, mo, wk, regionUS)


#====================================================================
#====================================================================

#read the CDC RSV update file/dataset into R (US national data from 2021 onwards)
#source (https://www.cdc.gov/surveillance/nrevss/rsv/region.html)
rsv_usa_nat1 <- runIfExpired('usa_rsv/rsv2021+', maxage = 168, ~readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14Num_Nat.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for the US"]]

#data wrangling
rsv_usa_nat1 <- 
  rsv_usa_nat1 %>% 
  dplyr::select(RepWeekDate, `PCR Detections`) %>% #we are reading Antigen test results only but its incorrectly labeled as PCR from source data
  dplyr::rename("date" = "RepWeekDate", cases = `PCR Detections`) %>%
  dplyr::mutate(cases = as.integer(cases),
                date = lubridate::mdy(date),
                hemi = "NH",
                region = "AMR",
                country = "United States") %>%
  dplyr::select(hemi, region, country, date, cases)

#view structure of data
str(rsv_usa_nat1)

#expand the dataset to have single case per row
rsv_usa_nat1 <- 
  rsv_usa_nat1 %>% 
  type.convert(as.is = TRUE) %>% 
  uncount(cases)

#view structure of data
str(rsv_usa_nat1)

#assign data types to variables
rsv_usa_nat1 <-
  rsv_usa_nat1 %>%
  dplyr::mutate(hemi = factor(hemi),
                region = factor(region),
                country = country,
                date = lubridate::ymd(date))

# view structure of data
str(rsv_usa_nat1)

#====================================================================
#====================================================================

#read the CDC RSV update file/dataset into R (US national data from 2020 backwards)
#source (https://healthdata.gov/dataset/Respiratory-Syncytial-Virus-Laboratory-Data-NREVSS/7zgq-bp9w)
rsv_usa_nat2 <- runIfExpired('usa_rsv/rsv2020-', maxage = 168, ~read.csv(curl("https://data.cdc.gov/api/views/52kb-ccu2/rows.csv?accessType=DOWNLOAD")))

#get the required variables
rsv_usa_nat2 <-
  rsv_usa_nat2 %>%
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
str(rsv_usa_nat2)

#expand the dataset to have single case per row
rsv_usa_nat2 <- 
  rsv_usa_nat2 %>% 
  type.convert(as.is = TRUE) %>% 
  uncount(cases)

#view structure of data
str(rsv_usa_nat2)

#assign data types to variables
rsv_usa_nat2 <-
  rsv_usa_nat2 %>%
  dplyr::mutate(hemi = factor(hemi),
                region = factor(region),
                country = country,
                date = lubridate::ymd(date))

# view structure of data
str(rsv_usa_nat2)

#====================================================================
#====================================================================

#combine dataset from 2020 backwards (rsv_usa2) and 2021 onwards (rsv_usa1)
rsv_usa_nat <-
  rows_append(
    rsv_usa_nat1,
    rsv_usa_nat2)
rm(rsv_usa_nat1, rsv_usa_nat2)

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

#combine dataset for the national and region
#rsv_usa <- dplyr::rows_append(rsv_usa_nat, rsv_usa_reg)

#====================================================================
#====================================================================

#alternatively ignore the independent National US dataset and
#add up all regional datasets to make national then combine with each region in single dataset
rsv_usa <-
rsv_usa_reg %>% 
  dplyr::select(everything(), -regionUS) %>% 
  dplyr::group_by(hemi, region, country, date, yr, mo, wk) %>% 
  dplyr::summarise(cases = sum(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(regionUS = "National") %>%
  dplyr::select(hemi:date, cases, yr, mo, wk, regionUS) %>%
  dplyr::rows_append(rsv_usa_reg)
