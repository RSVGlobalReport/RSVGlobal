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
X <- rsv_afr %>%
  select(country, yr, wk, cases) %>%
  filter(wk <=52, yr != 2020) %>% #alternatively can also take mean cases of week 52 and 53 and assign to week 52
  #mutate(yr = yr, wk = wk) %>%
  pivot_wider(names_from = wk, values_from = cases, names_sort = TRUE) %>%
  split(list(.$country))

#array names of rows, columns and countries
r.names = as.character(c(2017, 2018, 2019, 2021, 2022))
c.names = as.character(c(1:52))
m.names = c ("Central African Republic", "Côte d'Ivoire", "Madagascar", "South Africa")
countries = c ("Central African Republic", "Côte d'Ivoire", "Madagascar", "South Africa")

#generate an array using datasets represented by countries
afr_data2017_19 <- array(data = c(unlist(X[["Central African Republic"]]),  
                                  unlist(X[["Côte d'Ivoire"]]), 
                                  unlist(X[["Madagascar"]]), 
                                  unlist(X[["South Africa"]])), 
                         dim = c(5, 54, 4))

#subset array to remove unnecessary country and year columns 1:2
afr_data2017_19 <- afr_data2017_19[1:5, 3:54, ]

#convert array into numeric and assign row, column and country names
afr_data2017_19 <- array(as.numeric(afr_data2017_19), dim = c(5, 52, 4), dimnames = list (r.names, c.names, m.names))

#set seed
set.seed = 1988

#====================================================================
#2017
#====================================================================

#function to calculate derivative
#function to assist derivative calculation
deriv <- function(x, y) diff(y) / diff(x) 
middle_pts <- function(x) x[-1] - diff(x) / 2 

#generate time sequence
t = seq(0.5, 52.5, 0.01)
dtime = seq(0.5, 52.5, 0.01)

#create models stored in list
afr_model2017 <- list()

# k is the smoothing parameter that controls the wiggliness of the curve
# Here it's estimated via algorithm in the "method" e.g. REML or GCV.Cp
afr_model2017 <- lapply(countries, function(country){gam(cases ~ s(x = time, bs = "ps"), 
                                                    family = poisson, 
                                                    method = "GCV.Cp", 
                                                    data = data.frame(time = seq(1:52), cases = afr_data2017_19['2017',,country])
                                                    )
  })

#uncertainty interval of RSV trajectory and onset timing
#create list to store case and onset samples for 2017
cases.samples.2017 <- list()
onset.samples.2017 <- list()

#iterate for each country, compute first and second derivative
for (i in 1:length(countries)){ #sample the timing of RSV epidemics in 2017
  cases.samples.2017[[i]] = pspline.sample.timeseries(afr_model2017[[i]], 
                                                      data.frame(time = t), 
                                                      pspline.outbreak.cases, 
                                                      samples = 200)
  }

for (i in 1:length(countries)){
  onset.samples.2017[[i]] = cases.samples.2017[[i]] %>% 
    group_by(pspline.sample) %>% # for each sample do the following
    do((function(data){
      deriv.pred = data.frame(deriv = diff(data$cases)/diff(data$time), # calculate the first derivative
                              time = c(1:length(diff(t)))) 
      
      second_d = data.frame(second.deriv = deriv(middle_pts(dtime), deriv(dtime, data$cases)), # calculate the second derivative
                            time = c(1:(length(diff(t))-1))) 
      
      indicator = deriv.pred[which(deriv.pred$deriv > 0),]  # only look at second derivatives in the increasing segment (first derivative > 0 )
      second_d_test <- second_d[second_d$time%in%indicator$time,]
      
      onset = dtime[second_d_test$time[second_d_test$second.deriv == max(second_d_test$second.deriv)]] #find when second derivative of smooth functions reached its maximum 
      
      data.frame(
        pspline.sample = tail(data$pspline.sample, 1),
        onset = onset,
        cases = data$cases[which(data$time == onset)]) #find the case number when the second derivative reach its maximum
    })(.)) %>%
    ungroup()
  }

#create an empty matrix to hold onset timing data
onset.2017 <- matrix(data = NA, nrow = length(countries), ncol = 4)
colnames(onset.2017) <- c("mean","lower","upper","country")

for (i in 1:length(countries)){
  onset.2017[i,1] <- mean(onset.samples.2017[[i]]$onset)
  onset.2017[i,2] <- quantile(onset.samples.2017[[i]]$onset,0.025)
  onset.2017[i,3] <- quantile(onset.samples.2017[[i]]$onset,0.975) 
}

#convert onset timing data matrix into data frame and assign countries
onset.2017 <- as.data.frame(onset.2017)
onset.2017[,4] <- countries
onset.2017 <- onset.2017 %>% mutate(yr = 2017)

#====================================================================
#2018
#====================================================================

#function to calculate derivative
#function to assist derivative calculation
deriv <- function(x, y) diff(y) / diff(x) 
middle_pts <- function(x) x[-1] - diff(x) / 2 

#generate time sequence
t = seq(0.5, 52.5, 0.01)
dtime = seq(0.5, 52.5, 0.01)

#create models stored in list
afr_model2018 <- list()

# k is the smoothing parameter that controls the wiggliness of the curve
# Here it's estimated via algorithm in the "method" e.g. REML or GCV.Cp
afr_model2018 <- lapply(countries, function(country){gam(cases ~ s(x = time, bs = "ps"), 
                                                    family = poisson, 
                                                    method = "GCV.Cp", 
                                                    data = data.frame(time = seq(1:52), cases = afr_data2017_19['2018',,country])
)
})

#uncertainty interval of RSV trajectory and onset timing
#create list to store case and onset samples for 2018
cases.samples.2018 <- list()
onset.samples.2018 <- list()

#iterate for each country, compute first and second derivative
for (i in 1:length(countries)){ #sample the timing of RSV epidemics in 2018
  cases.samples.2018[[i]] = pspline.sample.timeseries(afr_model2018[[i]], 
                                                      data.frame(time = t), 
                                                      pspline.outbreak.cases, 
                                                      samples = 200)
}

for (i in 1:length(countries)){
  onset.samples.2018[[i]] = cases.samples.2018[[i]] %>% 
    group_by(pspline.sample) %>% # for each sample do the following
    do((function(data){
      deriv.pred = data.frame(deriv = diff(data$cases)/diff(data$time), # calculate the first derivative
                              time = c(1:length(diff(t)))) 
      
      second_d = data.frame(second.deriv = deriv(middle_pts(dtime), deriv(dtime, data$cases)), # calculate the second derivative
                            time = c(1:(length(diff(t))-1))) 
      
      indicator = deriv.pred[which(deriv.pred$deriv > 0),]  # only look at second derivatives in the increasing segment (first derivative > 0 )
      second_d_test <- second_d[second_d$time%in%indicator$time,]
      
      onset = dtime[second_d_test$time[second_d_test$second.deriv == max(second_d_test$second.deriv)]] #find when second derivative of smooth functions reached its maximum 
      
      data.frame(
        pspline.sample = tail(data$pspline.sample, 1),
        onset = onset,
        cases = data$cases[which(data$time == onset)]) #find the case number when the second derivative reach its maximum
    })(.)) %>%
    ungroup()
}

#create an empty matrix to hold onset timing data
onset.2018 <- matrix(data = NA, nrow = length(countries), ncol = 4)
colnames(onset.2018) <- c("mean","lower","upper","country")

for (i in 1:length(countries)){
  onset.2018[i,1] <- mean(onset.samples.2018[[i]]$onset)
  onset.2018[i,2] <- quantile(onset.samples.2018[[i]]$onset,0.025)
  onset.2018[i,3] <- quantile(onset.samples.2018[[i]]$onset,0.975) 
}

#convert onset timing data matrix into data frame and assign countries
onset.2018 <- as.data.frame(onset.2018)
onset.2018[,4] <- countries
onset.2018 <- onset.2018 %>% mutate(yr = 2018)

#====================================================================
#2019
#====================================================================

#function to calculate derivative
#function to assist derivative calculation
deriv <- function(x, y) diff(y) / diff(x) 
middle_pts <- function(x) x[-1] - diff(x) / 2 

#generate time sequence
t = seq(0.5, 52.5, 0.01)
dtime = seq(0.5, 52.5, 0.01)

#create models stored in list
afr_model2019 <- list()

# k is the smoothing parameter that controls the wiggliness of the curve
# Here it's estimated via algorithm in the "method" e.g. REML or GCV.Cp
afr_model2019 <- lapply(countries, function(country){gam(cases ~ s(x = time, bs = "ps"), 
                                                    family = poisson, 
                                                    method = "GCV.Cp", 
                                                    data = data.frame(time = seq(1:52), cases = afr_data2017_19['2019',,country])
)
})

#uncertainty interval of RSV trajectory and onset timing
#create list to store case and onset samples for 2018
cases.samples.2019 <- list()
onset.samples.2019 <- list()

#iterate for each country, compute first and second derivative
for (i in 1:length(countries)){ #sample the timing of RSV epidemics in 2019
  cases.samples.2019[[i]] = pspline.sample.timeseries(afr_model2019[[i]], 
                                                      data.frame(time = t), 
                                                      pspline.outbreak.cases, 
                                                      samples = 200)
}

for (i in 1:length(countries)){
  onset.samples.2019[[i]] = cases.samples.2019[[i]] %>% 
    group_by(pspline.sample) %>% # for each sample do the following
    do((function(data){
      deriv.pred = data.frame(deriv = diff(data$cases)/diff(data$time), # calculate the first derivative
                              time = c(1:length(diff(t)))) 
      
      second_d = data.frame(second.deriv = deriv(middle_pts(dtime), deriv(dtime, data$cases)), # calculate the second derivative
                            time = c(1:(length(diff(t))-1))) 
      
      indicator = deriv.pred[which(deriv.pred$deriv > 0),]  # only look at second derivatives in the increasing segment (first derivative > 0 )
      second_d_test <- second_d[second_d$time%in%indicator$time,]
      
      onset = dtime[second_d_test$time[second_d_test$second.deriv == max(second_d_test$second.deriv)]] #find when second derivative of smooth functions reached its maximum 
      
      data.frame(
        pspline.sample = tail(data$pspline.sample, 1),
        onset = onset,
        cases = data$cases[which(data$time == onset)]) #find the case number when the second derivative reach its maximum
    })(.)) %>%
    ungroup()
}

#create an empty matrix to hold onset timing data
onset.2019 <- matrix(data = NA, nrow = length(countries), ncol = 4)
colnames(onset.2019) <- c("mean","lower","upper","country")

for (i in 1:length(countries)){
  onset.2019[i,1] <- mean(onset.samples.2019[[i]]$onset)
  onset.2019[i,2] <- quantile(onset.samples.2019[[i]]$onset,0.025)
  onset.2019[i,3] <- quantile(onset.samples.2019[[i]]$onset,0.975) 
}

#convert onset timing data matrix into data frame and assign countries
onset.2019 <- as.data.frame(onset.2019)
onset.2019[,4] <- countries
onset.2019 <- onset.2019 %>% mutate(yr = 2019)

#====================================================================
#2021
#====================================================================

#function to calculate derivative
#function to assist derivative calculation
deriv <- function(x, y) diff(y) / diff(x) 
middle_pts <- function(x) x[-1] - diff(x) / 2 

#generate time sequence
t = seq(0.5, 52.5, 0.01)
dtime = seq(0.5, 52.5, 0.01)

#create models stored in list
afr_model2021 <- list()

# k is the smoothing parameter that controls the wiggliness of the curve
# Here it's estimated via algorithm in the "method" e.g. REML or GCV.Cp
afr_model2021 <- lapply(countries, function(country){gam(cases ~ s(x = time, bs = "ps"), 
                                                         family = poisson, 
                                                         method = "GCV.Cp", 
                                                         data = data.frame(time = seq(1:52), cases = afr_data2017_19['2021',,country])
)
})

#uncertainty interval of RSV trajectory and onset timing
#create list to store case and onset samples for 2018
cases.samples.2021 <- list()
onset.samples.2021 <- list()

#iterate for each country, compute first and second derivative
for (i in 1:length(countries)){ #sample the timing of RSV epidemics in 2021
  cases.samples.2021[[i]] = pspline.sample.timeseries(afr_model2021[[i]], 
                                                      data.frame(time = t), 
                                                      pspline.outbreak.cases, 
                                                      samples = 200)
}

for (i in 1:length(countries)){
  onset.samples.2021[[i]] = cases.samples.2021[[i]] %>% 
    group_by(pspline.sample) %>% # for each sample do the following
    do((function(data){
      deriv.pred = data.frame(deriv = diff(data$cases)/diff(data$time), # calculate the first derivative
                              time = c(1:length(diff(t)))) 
      
      second_d = data.frame(second.deriv = deriv(middle_pts(dtime), deriv(dtime, data$cases)), # calculate the second derivative
                            time = c(1:(length(diff(t))-1))) 
      
      indicator = deriv.pred[which(deriv.pred$deriv > 0),]  # only look at second derivatives in the increasing segment (first derivative > 0 )
      second_d_test <- second_d[second_d$time%in%indicator$time,]
      
      onset = dtime[second_d_test$time[second_d_test$second.deriv == max(second_d_test$second.deriv)]] #find when second derivative of smooth functions reached its maximum 
      
      data.frame(
        pspline.sample = tail(data$pspline.sample, 1),
        onset = onset,
        cases = data$cases[which(data$time == onset)]) #find the case number when the second derivative reach its maximum
    })(.)) %>%
    ungroup()
}

#create an empty matrix to hold onset timing data
onset.2021 <- matrix(data = NA, nrow = length(countries), ncol = 4)
colnames(onset.2021) <- c("mean","lower","upper","country")

for (i in 1:length(countries)){
  onset.2021[i,1] <- mean(onset.samples.2021[[i]]$onset)
  onset.2021[i,2] <- quantile(onset.samples.2021[[i]]$onset,0.025)
  onset.2021[i,3] <- quantile(onset.samples.2021[[i]]$onset,0.975) 
}

#convert onset timing data matrix into data frame and assign countries
onset.2021 <- as.data.frame(onset.2021)
onset.2021[,4] <- countries
onset.2021 <- onset.2021 %>% mutate(yr = 2021)

#====================================================================
#2022
#====================================================================

#function to calculate derivative
#function to assist derivative calculation
deriv <- function(x, y) diff(y) / diff(x) 
middle_pts <- function(x) x[-1] - diff(x) / 2 

#generate time sequence
t = seq(0.5, 52.5, 0.01)
dtime = seq(0.5, 52.5, 0.01)

#create models stored in list
afr_model2022 <- list()

# k is the smoothing parameter that controls the wiggliness of the curve
# Here it's estimated via algorithm in the "method" e.g. REML or GCV.Cp
afr_model2022 <- lapply(countries, function(country){gam(cases ~ s(x = time, bs = "ps"), 
                                                         family = poisson, 
                                                         method = "GCV.Cp", 
                                                         data = data.frame(time = seq(1:52), cases = afr_data2017_19['2022',,country])
)
})

#uncertainty interval of RSV trajectory and onset timing
#create list to store case and onset samples for 2018
cases.samples.2022 <- list()
onset.samples.2022 <- list()

#iterate for each country, compute first and second derivative
for (i in 1:length(countries)){ #sample the timing of RSV epidemics in 2019
  cases.samples.2022[[i]] = pspline.sample.timeseries(afr_model2022[[i]], 
                                                      data.frame(time = t), 
                                                      pspline.outbreak.cases, 
                                                      samples = 200)
}

for (i in 1:length(countries)){
  onset.samples.2022[[i]] = cases.samples.2022[[i]] %>% 
    group_by(pspline.sample) %>% # for each sample do the following
    do((function(data){
      deriv.pred = data.frame(deriv = diff(data$cases)/diff(data$time), # calculate the first derivative
                              time = c(1:length(diff(t)))) 
      
      second_d = data.frame(second.deriv = deriv(middle_pts(dtime), deriv(dtime, data$cases)), # calculate the second derivative
                            time = c(1:(length(diff(t))-1))) 
      
      indicator = deriv.pred[which(deriv.pred$deriv > 0),]  # only look at second derivatives in the increasing segment (first derivative > 0 )
      second_d_test <- second_d[second_d$time%in%indicator$time,]
      
      onset = dtime[second_d_test$time[second_d_test$second.deriv == max(second_d_test$second.deriv)]] #find when second derivative of smooth functions reached its maximum 
      
      data.frame(
        pspline.sample = tail(data$pspline.sample, 1),
        onset = onset,
        cases = data$cases[which(data$time == onset)]) #find the case number when the second derivative reach its maximum
    })(.)) %>%
    ungroup()
}

#create an empty matrix to hold onset timing data
onset.2022 <- matrix(data = NA, nrow = length(countries), ncol = 4)
colnames(onset.2022) <- c("mean","lower","upper","country")

for (i in 1:length(countries)){
  onset.2022[i,1] <- mean(onset.samples.2022[[i]]$onset)
  onset.2022[i,2] <- quantile(onset.samples.2022[[i]]$onset,0.025)
  onset.2022[i,3] <- quantile(onset.samples.2022[[i]]$onset,0.975) 
}

#convert onset timing data matrix into data frame and assign countries
onset.2022 <- as.data.frame(onset.2022)
onset.2022[,4] <- countries
onset.2022 <- onset.2022 %>% mutate(yr = 2022)

#====================================================================
#====================================================================

#combine datasets for plotting
rbind(onset.2017, onset.2018, onset.2019) %>%
  
  ggplot(aes(x = mean, y = country, color = country)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, size = 0.3) +
  theme_bw() +
  facet_wrap(.~yr) +
  theme(legend.position = "none", strip.background = element_rect(fill = "white"))
  
  









