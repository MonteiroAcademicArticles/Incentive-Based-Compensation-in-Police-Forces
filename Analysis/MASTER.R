#------------------------------------------------------------------------------#

#	 SIM- Master script

#------------------------------------------------------------------------------#

rm(list = ls())

#------------------------------------------------------------------------------#
#### Section switches ####


# Run different sections of analysis

RUN_var_construction = T

# Main analysis
RUN_main_analysis = T
RUN_desc_analysis = T

# Robustness checks
RUN_placebo_analysis = T
RUN_poisson_analysis = T
RUN_spatial_analysis = T

# Settings switches

EXPORT_data = T
EXPORT_plots = F
EXPORT_tables = T

#------------------------------------------------------------------------------#
#### Packages ####

library(tidyverse)
library(magrittr)
library(readstata13)
library(lfe)
library(spdep) # lagsarlm 
library(plm) # Panel data
library(splm) # Spatial panel data
library(Rcpp)
library(Hmisc)

library(stargazer)
library(huxtable)
library(flextable)
library(jtools)

library(rgeos)
library(rgdal)
library(sf)
library(sp)
library(maps)
library(geosphere)
library(viridis)
library(tmap)
library(spdep) # Moran's I
library(spatialreg) # Spatial lag model
library(readstata13)
library(tidyverse)
library(broom)

library(data.table)

#------------------------------------------------------------------------------#
#### Projections ####

RjProj_aze <- CRS("+proj=aeqd +lat_0=-22.911522 +lon_0=-43.397503") 
RjProj_unp <- CRS("+init=epsg:4326") 


#------------------------------------------------------------------------------#
#### File paths ####
HOME_DIR <- "" # <- Change for path on your computer

DATA <- file.path(HOME_DIR, "data")
OUTPUTS <- file.path(HOME_DIR, "Output")
OUTPUTS_final <- OUTPUTS


GIS <- file.path(HOME_DIR, "DATA/geo/GIS")


#------------------------------------------------------------------------------#
#### Function definition ####

source(file.path(HOME_DIR, "Analysis/utils.R"))

#------------------------------------------------------------------------------#
#### Load Data ####

#### Create placebo targets ####

if(RUN_var_construction){
  source(file.path(HOME_DIR, "Analysis/01-construction.R"))
  
}

# Load final data created
if(file.exists(file.path(DATA, "finaL_data_SIM.csv"))){
  final_data <- fread(file = file.path(DATA, "final_data_SIM.csv"),
                      encoding = "UTF-8", dec = ',')
  
}else{
  print("Please, turn RUN_placebo_targets_construction to TRUE and run again.")
}

#------------------------------------------------------------------------------#
#### Globals ####

####  List regression variables 

depVars <- c("violent_death_sim",
             "vehicle_robbery",
             "street_robbery",
             "homicide",
             "dpolice_killing",
             "police_killing",
             "vehicle_theft",
             "street_theft",
             "dbody_found",
             "other_robberies",
             "cargo_robbery",
             'body_bones_found',
             'other_robberies',
             'attempt_homicide',
             'involuntary_homicide',
             'other_homicides',
             'theft',
             'all_violent_deaths')
# Temporario
depVars <- c(depVars,paste0(depVars,'_log'))

names(depVars) <- depVars


indepVars <- c(
  "hit_sem_l",
  "n_precinct",
  "max_prize",
  "population" )

names(indepVars) <- indepVars

FEVars <- c("aisp",
            "month_semester",
            'year', 
            "id_cmt")
names(FEVars) <- FEVars


#-----------------------------#
# Diff in Diff (a.k.a Virada) #

indep_vars_dd <- c(
  "hit_sem_fm_l",
  "month6",
  "last_month",
  "n_precinct",
  "max_prize",
  "population" )

# Since there are only 4 months in this analysis I'm removing month
# fixed effects to avoid collinearity
FE_vars_dd <- c("aisp",
                "month_semester",
                "year",
                "id_cmt")

# Set cluster SE level
cluster_vars_dd= "0"

#------------------------------------------------------------------------------#
#### Sections ####

#------------------------------------------------------------------------------#
#### Main Analysis ####

if(RUN_main_analysis){
  source(file.path(HOME_DIR, "Analysis/02-analysis-main.R"))
}


#------------------------------------------------------------------------------#
#### Descriptive Analysis ####

if(RUN_desc_analysis){
  source(file.path(HOME_DIR, "Analysis/02-analysis-descriptives.R"))
}


#------------------------------------------------------------------------------#
#### Robustuness Analysis ####

if(RUN_poisson_analysis){
  source(file.path(HOME_DIR, "Analysis/03-robustness-poisson.R"))
}

if(RUN_spatial_analysis){
  source(file.path(HOME_DIR, "Analysis/03-robustness-spatial2.R"))
}

if(RUN_placebo_analysis){
  source(file.path(HOME_DIR, "Analysis/03-robustness-placebo.R"))
}
