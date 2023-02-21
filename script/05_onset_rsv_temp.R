#Deus & Dan (code adapted from Gigi - https://github.com/Gigi112/RSV_tutorials/blob/main/2_RSV_timing_explain/RSV_timing_tutorial.Rmd)
#18/11/2022
#global reemergence of RSV onset, duration and peak

#disease onset calculations and intuition
#When the first derivative is positive, the original curve is increasing. 
#In epidemic, this corresponds to the early stage of an outbreak as cases are increasing. 
#When the second derivative reach its maximum in the segment that the first derivative is positive, it means that the growth rate of the increasing trend reach its maximum. 
#This fits for the definition of the starting point of an disease outbreak.   

#====================================================================
#====================================================================

#split the dataset by country each having country name, yr, wk (up to 52 weeks) and cases from 2017-2019
#we restrict weeks to 52 (may also take mean cases of week 52 and 53 and assign to week 52)
#, rsv_usa_nat %>% select(hemi:date, yr, mo, wk, cases)

#drop country and rename within US regions as country to adapt the code below
W <-
  left_join(rsv_all, climate) %>%
  filter(wk_scale == "inverted") %>%
  dplyr::select(country, yr, wk, cases) %>%
  dplyr::arrange(country, yr, wk) %>%
  dplyr::filter(wk <=52) %>%
  dplyr::mutate(yr = case_when((wk %in% 24:52 & yr == 2017) | (wk %in% 1:23 & yr == 2018) ~ "2017/18",
                               (wk %in% 24:52 & yr == 2018) | (wk %in% 1:23 & yr == 2019) ~ "2018/19",
                               (wk %in% 24:52 & yr == 2019) | (wk %in% 1:23 & yr == 2020) ~ "2019/20",
                               (wk %in% 24:52 & yr == 2020) | (wk %in% 1:23 & yr == 2021) ~ "2020/21",
                               (wk %in% 24:52 & yr == 2021) | (wk %in% 1:23 & yr == 2022) ~ "2021/22",
                               (wk %in% 24:52 & yr == 2022) | (wk %in% 1:23 & yr == 2023) ~ "2022/23",
                               (wk %in% 24:52 & yr == 2023) | (wk %in% 1:23 & yr == 2024) ~ "2023/24",
                               (wk %in% 24:52 & yr == 2024) | (wk %in% 1:23 & yr == 2025) ~ "2024/25",
                               (wk %in% 24:52 & yr == 2025) | (wk %in% 1:23 & yr == 2026) ~ "2025/26",
                               (wk %in% 24:52 & yr == 2026) | (wk %in% 1:23 & yr == 2027) ~ "2026/27",
                               TRUE ~ NA_character_)) %>%
  dplyr::filter(!is.na(yr)) %>%
  
  #for temperate countries, ensure seasonality starts from week 24 and ensure annual cases are 100+ only
  dplyr::group_by(country, yr) %>%
  mutate(wk = seq.int(from = 24, by = 1, length.out = n()),
         tcases = sum(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(yr != "2020/21",  tcases >250) %>% #filter out covid period
  dplyr::select(country, yr, wk, cases)


#split the dataset by country/regionUS and year to form list of datasets
X <- 
  W %>%  
  split(list(.$country, .$yr))

#delete empty country.yr data frames from list X (they have less than threshold total cases [tcases] throughout the year)
X <- X[unlist(lapply(X, nrow) != 0)]

#function to calculate derivative and assist derivative calculation
deriv <- function(x, y) diff(y) / diff(x) 
middle_pts <- function(x) x[-1] - diff(x) / 2 

#generate time sequence
t = seq(0.5, 52.5, 0.01)
dtime = seq(0.5, 52.5, 0.01)

#create empty list to store GAM models
Gmodels <- list()

#set seed
set.seed = 1988

#run the GAM models where smoothing parameter/knots are automatically selected via a cross validation method
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = wk, bs = "ps"),
                      family = poisson,
                      method = "REML",
                      control = list(maxit =100000),
                      data = X[[i]]
  )
}

#uncertainty interval of RSV trajectory and onset timing
#create list to store case and onset samples for 2017
cases.samples <- list()
onset.samples <- list()

#uns simulations on an outbreak GAM to estimate time series outbreak outcomes 
#and returns estimated time series outcomes for each simulation.
for (i in names(X)){
  cases.samples[[i]] = pspline.sample.timeseries(Gmodels[[i]], 
                                                 data.frame(wk = t), 
                                                 pspline.outbreak.cases, 
                                                 samples = 100)
}

#iterate for each country and year, compute first and second derivative
for (i in names(X)){
  onset.samples[[i]] = cases.samples[[i]] %>% 
    group_by(pspline.sample) %>% # for each sample do the following
    do((function(data){
      deriv.pred = data.frame(deriv = diff(data$cases)/diff(data$wk), # calculate the first derivative
                              wk = c(1:length(diff(t)))) 
      
      second_d = data.frame(second.deriv = deriv(middle_pts(dtime), deriv(dtime, data$cases)), # calculate the second derivative
                            wk = c(1:(length(diff(t))-1))) 
      
      indicator = deriv.pred[which(deriv.pred$deriv > 0),]  # only look at second derivatives in the increasing segment (first derivative > 0 )
      second_d_test <- second_d[second_d$wk%in%indicator$wk,]
      
      onset = dtime[second_d_test$wk[second_d_test$second.deriv == max(second_d_test$second.deriv)]] #find when second derivative of smooth functions reached its maximum 
      
      data.frame(
        pspline.sample = tail(data$pspline.sample, 1),
        onset = onset,
        cases = data$cases[which(data$wk == onset)]) #find the case number when the second derivative reach its maximum
    })(.)) %>%
    ungroup()
}

#compute mean, low and upper 95%CI for onset by country and preCOVID-19 vs each year after COVID-19 year
rsv_onset_temp <-
  bind_rows(onset.samples, .id = "id") %>%
  mutate(yr = str_sub(id, -7, -1),
         country = word(id, 1, sep = "\\.")) %>%
  select(everything(), -id) %>%
  mutate(covper = if_else(yr == "2017/18" | yr == "2018/19" | yr == "2019/20", "preCOVID-19",
                          if_else(yr == "2021/22", "2021/22",
                                  if_else(yr == "2022/23", "2022/23",
                                          if_else(yr == "2023/24", "2023/24",
                                                  if_else(yr == "2024/25", "2024/25",
                                                          if_else(yr == "2025/26", "2025/26", NA_character_))))))) %>%
  dplyr::filter(!is.na(covper)) %>%
  group_by(country, covper) %>%
  summarise(epiwk = mean(onset, 0.025),
            l_epiwk = quantile(onset, 0.025),
            u_epiwk = quantile(onset, 0.975)) %>%
  ungroup()
