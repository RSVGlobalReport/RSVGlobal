#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================

#load a package "pacman" used for for installing and loading other packages
if(!require(pacman)) install.packages("pacman")

#use pacman to load packages for analysis
pacman::p_load(char = c("lubridate", "tidyverse", "dplyr", "tidyr", "broom", "rio", "scales", "boot", "magrittr",  "mvtnorm", "zoo", "stringr", "survminer",
                        "patchwork", "PropCIs", "reshape2","purrr", "minqa", "ggridges", "timetk", "ggbreak", "ggpubr", "gridExtra", "readr", "survival",
                        "curl", "archive", "jsonlite", "janitor", "ggh4x", "distcrete", "epitrix", "mgcv", "pspline.inference", "RCurl", "XML", "psych", "ie2misc", 
                        "rlist", "tsibble", "htmlwidgets", "plotly", "utils", "MLmetrics", "circular", "gsignal", "moderndive", "knitr", "Smisc", "here"))

#set seed for entire session to ensure reproducibility using a task call
addTaskCallback(function(...) {set.seed(12345); TRUE})

#turn off the task call for set seed if needed
#removeTaskCallback(1)

#====================================================================

#set caching code
source("script/02_fileCache.R")

#set archiving path for downloaded datasets
source("script/03_runIfExpired.R")

#load RSV dynamics datasets
source("script/04_rsvLoad_cases.R")

#compute RSV onset
source("script/05_rsvCompute_onset.R")

#plot RSV onset
source("script/05_rsvPlot_onset.R")

#compute RSV peak
source("script/06_rsvCompute_peak.R")

#plot RSV onset
source("script/06_rsvPlot_peak.R")

#compute RSV growth
source("script/07_rsvCompute_growth.R")

#plot RSV growth
source("script/07_rsvPlot_growth.R")

#compute RSV intensity
source("script/08_rsvCompute_intensity.R")

#plot RSV intensity
source("script/08_rsvPlot_intensity.R")

#run RSV dynamics by hemisphere
source("script/09_rsvDyn_hemi.R")

#run RSV dynamics by region
source("script/10_rsvDyn_region.R")

#run RSV dynamics by country
source("script/11_rsvDyn_country.R")
