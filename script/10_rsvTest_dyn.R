#====================================================================
# UNITED STATES DATA
#====================================================================

#read the CDC RSV update file/dataset into R, both case detection and testing (US regional data from 2021 onwards)
#source (https://www.cdc.gov/surveillance/nrevss/rsv/region.html)
rsv_usaNE <- runIfExpired('usa_rsv/rsv2021NE+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg1.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 1(5 week Average)"]]
rsv_usaNEp <- runIfExpired('usa_rsv/rsv2021NEp+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV124PP_Reg1.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Data for Census Region 1"]]

%>%
  left_join(bind_rows(rsv_usaNEp %>% select(RepWeekDate, `PCR Detection`) %>% rename("date" = "RepWeekDate", "test" = `PCR Detection`))) %>%
  mutate(cases = as.numeric(cases), test = as.numeric(test), test = 100*cases/test)


rsv_usaMW <- runIfExpired('usa_rsv/rsv2021MW+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg2.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 2(5 week Average)"]]
rsv_usaMWp <- runIfExpired('usa_rsv/rsv2021MWp+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV124PP_Reg2.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Data for Census Region 2"]]

%>%
  left_join(bind_rows(rsv_usaMWp %>% select(RepWeekDate, `PCR Detection`) %>% rename("date" = "RepWeekDate", "test" = `PCR Detection`))) %>%
  mutate(cases = as.numeric(cases), test = as.numeric(test), test = 100*cases/test)

rsv_usaSo <- runIfExpired('usa_rsv/rsv2021So+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg3.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 3(5 week Average)"]]
rsv_usaSop <- runIfExpired('usa_rsv/rsv2021Sop+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV124PP_Reg3.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Data for Census Region 3"]]

%>%
  left_join(bind_rows(rsv_usaSop %>% select(RepWeekDate, `PCR Detection`) %>% rename("date" = "RepWeekDate", "test" = `PCR Detection`))) %>%
  mutate(cases = as.numeric(cases), test = as.numeric(test), test = 100*cases/test)

rsv_usaWe <- runIfExpired('usa_rsv/rsv2021We+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_Reg4.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Numerator Data for Census Region 4(5 week Average)"]]
rsv_usaWep <- runIfExpired('usa_rsv/rsv2021Wep+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV124PP_Reg4.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Data for Census Region 4"]]

%>%
  left_join(bind_rows(rsv_usaWep %>% select(RepWeekDate, `PCR Detection`) %>% rename("date" = "RepWeekDate", "test" = `PCR Detection`))) %>%
  mutate(cases = as.numeric(cases), test = as.numeric(test), test = 100*cases/test)

rsv_usaFl <- runIfExpired('usa_rsv/rsv2021Fl+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV14NumCent5AVG_FL.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Data for Florida(5 week Average)"]]
rsv_usaFlp <- runIfExpired('usa_rsv/rsv2021Flp+', maxage = 168, ~XML::readHTMLTable(getURL("https://www.cdc.gov/surveillance/nrevss/images/trend_images/RSV124PP_FL.htm",.opts = list(ssl.verifypeer = FALSE)), header = TRUE))[["RSV Data for Florida"]]

%>%
  left_join(bind_rows(rsv_usaFlp %>% select(RepWeekDate, `PCR Detection`) %>% rename("date" = "RepWeekDate", "test" = `PCR Detection`))) %>%
  mutate(cases = as.numeric(cases), test = as.numeric(test), test = 100*cases/test)



#read the WHO RSV update file into R
#source (https://www.who.int/teams/global-influenza-programme/surveillance-and-monitoring/influenza-surveillance-outputs)
rsv <- runIfExpired('who_rsv', maxage = 168, ~read.csv(curl("https://frontdoor-l4uikgap6gz3m.azurefd.net/FLUMART/VIW_FNT?$format=csv")))

#set strings as factors to false globally
base::options(stringsAsFactors = FALSE)

#get the required variables
rsvds <-
  rsv %>%
  dplyr::filter(!is.na(RSV)) %>%
  dplyr::select(HEMISPHERE, WHOREGION, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE, ORIGIN_SOURCE, SPEC_PROCESSED_NB, RSV) %>%
  dplyr::rename("hemi" = HEMISPHERE,
                "region" = WHOREGION,
                "country" = COUNTRY_AREA_TERRITORY,
                "date"= MMWR_WEEKSTARTDATE,
                "sentin" = ORIGIN_SOURCE,
                "test" = SPEC_PROCESSED_NB,
                "cases" = RSV)

#view structure of data
utils::str(rsvds)
