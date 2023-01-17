library(tidyverse)
library(broom)
library(readxl)

#Load parameters file
source("parameters.R")

#load in coefficients file
if (!file.exists("coefs.rda")) {
  source("historyanalysis.R")
} else {
  load("coefs.rda")
}

#script to calculate replacement pitcher and hitter values
source("replacement_level.R")

#load hitter and pitcher projections
if (!file.exists("projections.rda")) {
  source("calculatevalue.R")
} else{
  load("projections.rda")
}

#Build league
source("leaguesetup.R")

#run draft
source("draftpicks.R")

#merge in projections
source("mergeinprojections.R")
  
#calculate standings
source("calculatestandings.R")

#calculate league statistics
source("calc_league_stats.R")

#write to .csv
source("csvwriter.R")

standings

marmaduke <- view_team("marmaduke")
pasadena <- view_team("pasadena")
pkd <- view_team("pkdodgers")
