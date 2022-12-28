#Deus & Dan
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================

pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # Data management + ggplot2 graphics
  epicontacts,  # Analysing transmission networks
  EpiNow2,      # Rt estimation
  EpiEstim,     # Rt estimation
  projections,  # Incidence projections
  incidence2,   # Handling incidence data
  epitrix,      # Useful epi functions
  distcrete     # Discrete delay distributions
)

