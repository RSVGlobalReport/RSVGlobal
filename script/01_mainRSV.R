#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================

#load package for installing and loading other packages
#install.packages("pacman")
#library(pacman)

#use packman to load packages for analysis
pacman::p_load(char = c("lubridate", "tidyverse", "dplyr", "utils", "MLmetrics", "rio", "scales", "boot", "magrittr",  "mvtnorm", "zoo", "stringr",
                        "patchwork", "PropCIs", "reshape2","purrr", "minqa", "ggridges", "timetk", "ggbreak", "ggpubr", "gridExtra", "readr",
                        "curl", "archive", "jsonlite", "janitor", "ggh4x", "EpiEstim", "projections", "distcrete", "epitrix", "mgcv", 
                        "pspline.inference", "incidence2", "RCurl", "XML", "rlist", "tsibble", "htmlwidgets", "plotly", "here"))

#====================================================================

#set caching code
source("script/02_fileCache.R")

#set archiving path for downloaded datasets
source("script/03_runIfExpired.R")

#load RSV dynamics datasets
source("script/04_load_rsv_cases.R")

#compute RSV onset datasets
source("script/05_load_rsv_onset.R")

#make RSV onset plots
source("script/06_plot_rsv_onset.R")

#run RSV dynamics by hemisphere
source("script/07_seasRSV_hemi.R")

#run RSV dynamics by region
source("script/08_seasRSV_region.R")

#run RSV dynamics by country
source("script/09_seasRSV_country.R")
