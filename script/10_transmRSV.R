#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================

#specify the serial interval (distribution delays between symptom onset in primary and secondary cases)
rsv_config <- 
  make_config(
  mean_si = 7.0,
  std_si = 2.2
)

#====================================================================

#specify the dataset of interest - AFRICA
epi_afr <- 
  rsv_regn %>%
  ungroup() %>%
  filter(region == "AFRICA") %>%
  select(date, cases) %>% 
  arrange(date, cases)
  #tidyr::complete(date = seq.Date(from = min(date, na.rm = T), to = max(date, na.rm=T), by = "day"), fill = list(cases = 0))
str(epi_afr)

#estimate Rt using the EpiEstim method
est_afr <- estimate_R(
  incid = epi_afr$cases,
  method = "parametric_si",
  config = rsv_config
)


#plot RSV Incidence and Rt estimations
plot(est_afr)

#------------------------------------

## define a vector of dates starting on June 1st
start_dates <- seq.Date(
  as.Date("2018-01-14"),
  max(epi_afr$date) - 7,
  by = 7
) %>%
  ## subtract the starting date to convert to numeric
  `-`(min(epi_afr$date)) %>%
  ## convert to integer
  as.integer()

## add six days for a one week sliding window
end_dates <- start_dates + 6

## make config
config_partial <- make_config(
  mean_si = 7.0,
  std_si = 2.2,
  t_start = start_dates,
  t_end = end_dates
)

## run epiestim
epiestim_res_partial <- estimate_R(
  incid = epi_afr$cases,
  method = "parametric_si",
  config = config_partial
)

## plot outputs
plot(epiestim_res_partial)

#====================================================================

#specify the dataset of interest - AFRICA
epi_euro <- 
  rsv_regn %>%
  ungroup() %>%
  filter(region == "EUROPE") %>%
  select(date, cases) %>% 
  arrange(date, cases) %>%
  tidyr::complete(date = seq.Date(from = min(date, na.rm = T), to = max(date, na.rm=T), by = "day"), fill = list(cases = 0))
str(epi_euro)

#estimate Rt using the EpiEstim method
est_euro <- estimate_R(
  incid = epi_euro$cases,
  method = "parametric_si",
  config = rsv_config
)

#plot RSV Incidence and Rt estimations
plot(est_euro)

#====================================================================

