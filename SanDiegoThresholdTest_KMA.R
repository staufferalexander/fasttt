setwd("/Users/staufferkm/Desktop/gitrepos/fasttt/")
## Libraries to include
library(assertr)
library(broom)
library(digest)
library(fs)
library(functional)
library(getopt)
library(here)
library(housingData)
library(jsonlite)
library(knitr)
library(lubridate)
library(lazyeval)
library(lutz)
library(magrittr)
library(maps)
library(parallel)
library(purrr)
library(rlang)
library(rmarkdown)
library(rgdal)
library(rstan)
library(scales)
library(sp)
library(splines)
library(suncalc)
library(zoo)
library(lubridate)
library(lutz)
library(tidyverse)

## Load the data
data_path <- "/Users/staufferkm/Desktop/gitrepos/fasttt/DataForThreshold_KMA_formatted.txt"
data_path1 <- "/Users/staufferkm/Desktop/gitrepos/fasttt/sdsddatanew.txt"
data_path2 <- "/Users/staufferkm/Desktop/gitrepos/fasttt/SanDiegoPoliceRipaStopsTrafficOnly_3Race.txt"
stops2 <- read.delim(data_path2) #change this to whichever file path you need
missing = which(is.na(stops2$num_hits))
#stops1 = stops2[-missing,]
stops1 = stops2
zerostops = which(stops1$num_stops == 0)
#stops = stops1[-zerostops,]
stops = stops1
stops$driver_race = as.factor(stops$driver_race)  ##need to make sure driver_race is a column
stops$location_variable = as.factor(stops$location_variable) ###need to make sure location_variable (beat) is a column
#stops$location_variable = as.numeric(as.character(stops$location_variable))
save(stops, file = "/Users/staufferkm/Desktop/gitrepos/fasttt/SanDiegoPoliceRipaStopsTrafficOnly.RData") #change this to new path reflecting the data file you're using
#ok I had to take out '615	Black	17	0	1.0	CA	San_Diego	San_Diego' from data_path2 because I think it's causing the error
#read_in_stop_and_frisk_data()
#add_placebo_column() ###not sure if I need this? -KMA
#make_stop_and_frisk_dataframe(search_decision, 
#                              white_population_counterfactual_perturbation = 1, 
#                              col_to_filter_on = NULL,
#                              filter_fxn = NULL, 
#                              filename = "SanDiegoThresholdTest_TrafficOnlySearchDecision", 
#                              column_to_use_as_placebo = NULL)
run_threshold_test('SanDiegoPoliceRipaStopsTrafficOnly', 'model_mixture.stan') ##change the first entry to your stops RData prefix
load("/Users/staufferkm/Desktop/gitrepos/fasttt/SanDiegoPoliceRipaStopsTrafficOnly/SanDiegoPoliceRipaStopsTrafficOnly_model_mixture.RData")
bl = which(obs$driver_race == "Black")
wh = which(obs$driver_race == "White")
hi = which(obs$driver_race == "Hispanic")
obs.bl = obs[bl,]
obs.wh = obs[wh,]
obs.hi = obs[hi,]
hist(obs.bl$thresholds)
hist(obs.wh$thresholds)  
hist(obs.hi$thresholds)   
obs.df = as.data.frame(obs)
a1 <- aov(obs.df$thresholds ~ obs.df$driver_race) 
summary(a1)
pairwise.t.test(obs.df$thresholds, obs.df$driver_race, p.adj = "bonf")
  
  
