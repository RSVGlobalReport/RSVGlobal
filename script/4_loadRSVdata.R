#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================

#read the WHO RSV update file into R
rsv <- runIfExpired('who_rsv', maxage = 168, ~read.csv(curl("https://frontdoor-l4uikgap6gz3m.azurefd.net/FLUMART/VIW_FNT?$format=csv")))

#set strings as factors to false globally
options(stringsAsFactors = FALSE)

#get the required variables
rsvds <-
  rsv %>%
  dplyr::filter(!is.na(RSV)) %>%
  dplyr::select(WHOREGION, FLUSEASON, HEMISPHERE, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE, ORIGIN_SOURCE, RSV) %>%
  dplyr::arrange(WHOREGION, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE) %>%
  dplyr::rename("region" = WHOREGION,
                "fluseas" = FLUSEASON,
                "hemi" = HEMISPHERE,
                "country" = COUNTRY_AREA_TERRITORY,
                "date"= MMWR_WEEKSTARTDATE,
                "sentin" = ORIGIN_SOURCE,
                "cases" = RSV)

readr::write_csv(x = rsvds, file = here("data", "RSVglobal.csv"))

#view structure of data
str(rsvds)

#expand the dataset to have single case per row
rsvds <- 
  rsvds %>% 
  type.convert(as.is = TRUE) %>% 
  uncount(cases)

#view structure of data
str(rsvds)

#assign data types to variables
rsvds <-
  rsvds %>%
  dplyr::mutate(date = lubridate::ymd(date),
                wk = lubridate::week(date),
                mon = lubridate::month(date),
                yr = lubridate::year(date),
                sentin = factor(sentin),
                hemi = factor(hemi),
                fluseas = factor(fluseas))

# view structure of data
str(rsvds)

#====================================================================
#====================================================================
  
#filter to only have these countries included from African, South East Asia, Western pacific and Middle East
#filter to have times series from 2018 on-wards
rsv_asmw <- 
  rsvds %>%
  dplyr::filter(country %in% c("Cameroon", "Central African Republic", "Côte d'Ivoire", "Madagascar", "South Africa", #African
                               "India", #South East Asia
                               "Australia", "Japan", "Mongolia", "Malaysia", # Western pacific
                               "Oman", "Qatar"), #Middle East
                yr >= 2018) 

#'Not defined' may include sentinel or non-sentinel data
#'properly index by week to have 0 or some observed number of cases in sequential weeks
#combine sentinel and non-sentinel data or use undefined source of RSV cases
rsv_asmw <- 
  rsv_asmw %>% 
  tidyr::drop_na(date) %>%
  dplyr::group_by(region, country, fluseas, hemi, date, wk, mon, yr) %>%
  dplyr::mutate(wkcases = floor_date(date, unit = "week")) %>% 
  dplyr::count(wkcases) %>%           
  tidyr::complete(wkcases = seq.Date(from = min(wkcases), to = max(wkcases), by = "week"), fill = list(n = 0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(yrwk = tsibble::yearweek(wkcases),
                yrmo = tsibble::yearmonth(wkcases)) %>%
  dplyr::select(region, country, fluseas, hemi, wkcases, yr, mon, wk, yrwk, yrmo, n) %>%
  dplyr::rename("date" = "wkcases", "cases" = "n")
  

#check for duplicates by country and date
rsv_asmw %>% 
  janitor::get_dupes(country, date)

rsv_asmw <- 
  rsv_asmw %>% 
  dplyr::distinct(country, date, .keep_all = TRUE)

#====================================================================
#====================================================================

#filter to only have these countries included from Europe
#filter to have times series from 2018 on-wards
rsv_euro <- 
  rsvds %>%
  dplyr::filter(country %in% c("France", "Germany", "Netherlands", "Spain", "Portugal", "Iceland",
                               "Ireland", "Denmark", "Finland", "Sweden", "United Kingdom, England", 
                               "United Kingdom, Northern Ireland", "United Kingdom, Scotland",
                               "Bulgaria", "Belarus", "Russian Federation", "Hungary", "Poland", "Slovakia"),
                yr >= 2018) %>%
  dplyr::mutate(country = if_else(country == "United Kingdom, Northern Ireland", "NIreland",
                                  if_else(country == "United Kingdom, Scotland", "Scotland",
                                          if_else(country == "United Kingdom, England", "England",
                                                  if_else(country == "Russian Federation","Russia", country)))))

#'Not defined' may include sentinel or non-sentinel data
#properly index by week to have 0 or some observed number of cases in sequential weeks
rsv_euro <- 
  rsv_euro %>% 
  tidyr::drop_na(date) %>%
  dplyr::group_by(region, country, fluseas, hemi, sentin, date, wk, mon, yr) %>%
  dplyr::mutate(wkcases = floor_date(date, unit = "week")) %>% 
  dplyr::count(wkcases) %>%           
  tidyr::complete(wkcases = seq.Date(from = min(wkcases), to = max(wkcases), by = "week"), fill = list(n = 0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(yrwk = tsibble::yearweek(wkcases),
                yrmo = tsibble::yearmonth(wkcases)) %>%
  dplyr::select(region, country, fluseas, hemi, sentin, wkcases, yr, mon, wk, yrwk, yrmo, n) %>%
  dplyr::rename("date" = "wkcases", "cases" = "n") %>%

#combine sentinel and non-sentinel data
  dplyr::filter(sentin != "NOTDEFINED") %>%
  dplyr::group_by(region, country, fluseas, hemi, date, yr, mon, wk, yrwk, yrmo) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::ungroup()

#check for duplicates by country and date
rsv_euro %>% 
  janitor::get_dupes(country, date)

rsv_euro <- 
  rsv_euro %>% 
  dplyr::distinct(country, date, .keep_all = TRUE)

#====================================================================
#====================================================================

# filter to only have these countries included from Americas
#filter to have times series from 2018 on-wards
rsv_amer <- 
  rsvds %>%
  dplyr::filter(country %in% c("Argentina", "Belize", "Bolivia (Plurinational State of)", "Brazil", "Canada", "Colombia", "Costa Rica",
                               "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico",
                               "Nicaragua", "Panama", "Paraguay", "Peru", "Uruguay"),
                yr >= 2018) %>%
  dplyr:: mutate(country = if_else(country == "Bolivia (Plurinational State of)", "Bolivia",
                                   if_else(country == "Dominican Republic", "Dominica", country)))

#'Not defined' may include sentinel or non-sentinel data
#'properly index by week to have 0 or some number of cases in sequential weeks
#combine sentinel and non-sentinel data or use undefined source of RSV cases
rsv_amer <- 
  rsv_amer %>% 
  tidyr::drop_na(date) %>%
  dplyr::group_by(region, country, fluseas, hemi, date, wk, mon, yr) %>%
  dplyr::mutate(wkcases = floor_date(date, unit = "week")) %>% 
  dplyr::count(wkcases) %>%           
  tidyr::complete(wkcases = seq.Date(from = min(wkcases), to = max(wkcases), by = "week"), fill = list(n = 0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(yrwk = tsibble::yearweek(wkcases),
                yrmo = tsibble::yearmonth(wkcases)) %>%
  dplyr::select(region, country, fluseas, hemi, wkcases, yr, mon, wk, yrwk, yrmo, n) %>%
  dplyr::rename("date" = "wkcases", "cases" = "n")

#check for duplicates by country and date
rsv_amer %>% 
  janitor::get_dupes(country, date)

rsv_amer <- 
  rsv_amer %>% 
  dplyr::distinct(country, date, .keep_all = TRUE)

#====================================================================
#====================================================================

#combine all the datasets for regional seasonal dynamics plotting
rsv_regn <- 
  rsv_asmw %>% 
  dplyr::bind_rows(rsv_euro) %>%
  dplyr::bind_rows(rsv_amer)

#aggregate 
rsv_regn <- 
  rsv_regn %>% 
  dplyr::group_by(region, date, wk, mon, yr, yrwk, yrmo) %>%
  dplyr::summarise(cases = sum(cases))

#check for duplicates by country and date
rsv_regn %>% 
  janitor::get_dupes(region, date)

#rename the regions in full
rsv_regn <- 
  rsv_regn %>% 
  dplyr::distinct(region, date, .keep_all = TRUE) %>%
  dplyr::mutate(region = if_else(region == "AFR", "AFRICA",
                                 if_else(region == "AMR", "AMERICAS",
                                         if_else(region == "EUR", "EUROPE",
                                                 if_else(region == "SEAR", "SOUTH EAST ASIA", "WESTERN PACIFIC")))))
