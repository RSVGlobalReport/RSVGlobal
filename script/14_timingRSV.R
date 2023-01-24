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
X <- rbind(rsv_afr, rsv_wpr, rsv_sear, rsv_emr, rsv_amer, rsv_euro) %>%
  select(country, yr, wk, cases) %>%
  group_by(country, yr) %>%
  mutate(tcases = sum(cases, na.rm = TRUE)) %>%
  filter(wk <=52, yr != 2020, yr !=2023, tcases >100) %>% 
  select(country, yr, wk, cases) %>%
  arrange(country, yr, wk, cases) %>%
  split(list(.$country, .$yr))

#delete empty country.yr dataframes from list X (they have <10 total cases [tcases] throughout the year)
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
for (i in 1:length(X)) {
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
for (i in 1:length(X)){
  cases.samples[[i]] = pspline.sample.timeseries(Gmodels[[i]], 
                                                 data.frame(wk = t), 
                                                 pspline.outbreak.cases, 
                                                 samples = 100)
  }

#iterate for each country, compute first and second derivative
for (i in 1:length(X)){
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

#create an empty matrix to hold onset timing data
rsv_onset <- matrix(data = NA, nrow = length(X), ncol = 4)
colnames(rsv_onset) <- c("epiwk","l_epiwk","u_epiwk","ncountry")

#update the matrix with estimated values and convert to data frame
for (i in 1:length(X)){
  rsv_onset[i,1] <- mean(onset.samples[[i]]$onset)
  rsv_onset[i,2] <- quantile(onset.samples[[i]]$onset,0.025)
  rsv_onset[i,3] <- quantile(onset.samples[[i]]$onset,0.975) 
  rsv_onset[i,4] <- i}
rsv_onset <- as.data.frame(rsv_onset)

#extract country and yr from original dataset to merge with onset dataset
rsv_onset <- 
  rsv_onset %>%
  left_join(
    rbind(rsv_afr, rsv_wpr, rsv_sear, rsv_emr, rsv_amer, rsv_euro) %>%
      select(country, yr, wk, cases) %>%
      group_by(country, yr) %>%
      mutate(tcases = sum(cases, na.rm = TRUE)) %>%
      filter(wk <=52, yr != 2020, yr !=2023, tcases >100) %>% 
      summarise(tot_cases = sum(cases)) %>%
      ungroup() %>%
      mutate(ncountry = 1:n()) %>%
      select(ncountry, country, yr)
)

#====================================================================
#====================================================================

#combine datasets for plotting
rsv_onset %>%
  
  ggplot(aes(x = epiwk, y = country, color = country)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin = l_epiwk, xmax = u_epiwk), width = 0, size = 0.3) +
  theme_bw() +
  facet_wrap(.~yr, ncol = 5) +
  labs(x = "Epidemiological week", y = "country") +
  theme(legend.position = "none", strip.background = element_rect(fill = "white"))
 
  