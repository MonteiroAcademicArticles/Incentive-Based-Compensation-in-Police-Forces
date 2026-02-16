#------------------------------------------------------------------------------#

#	 SIM - Descriptive Stats	

#------------------------------------------------------------------------------#



# This code depends on analysis_main.R to run! So if this option is not selected
# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = F
}


#------------------------------------------------------------------------------#
#### Load data ####
sr <- final_data %>%
  # Removing lower and upper bounds we don't have data on batallion sizes after
  # 2015 and the system begun in the second semmester of 2009
  subset((year*10 + semester > 20091))

# Creating formula to run main regression and keep same sample
rFormula <- paste(indepVars, collapse = " + ") 
FeForumala1 <- paste(FEVars[1:2], collapse = " + ")
clusterVars = c("aisp" )
clusterVars_form <- paste(clusterVars, collapse =  " + ")
config1 <- paste("|", FeForumala1, "| 0 | ", clusterVars_form )
Formulas01_str <- paste(depVars, paste(rFormula, config1), sep = " ~ ")
names(Formulas01_str) <- depVars

# Keep same sample for as regressions
sd <- feRegSim('violent_death_sim') %>% regData(regdf = sr)


#------------------------------------------------------------------------------#
#### Table 1 - General descriptives ####

# Create table variabels

sd$pop1000 <- sd$population/1000

# Select variables
tab1Vars <- c("hit_sem_l",
              depVars[1:3], 
              'homicide',
              'police_killing',
              'dpolice_killing',
              'body_bones_found',
              'attempt_homicide',
              'other_robberies',
              'vehicle_theft',
              'street_theft',
              "max_prize",
              "pop1000",
              'n_precinct')

tab1VarLabels <- c("Eligible",
                   "Violent deaths",
                   "Vehicle robbery",
                   "Street robbery",
                   "Homicides",
                   "Police killing", 
                   "Police killing (dummy)",
                   "Cadavers and Bones Found",
                   "Attempted Murders",
                   "Other robberies not included in the target",
                   "Vehicle theft",
                   "Street theft",
                   "Maximum prize (R$ 1000)",
                   "District population (1000 hab)",
                   "Number of precint")


stargazer(sd %>% dplyr::select(tab1Vars),
          summary.stat = c("n", "mean", "sd", "min", "max"),
          type = "text",
          out = file.path(OUTPUTS_final, "Table 1 - Descriptive statistics.html"),
          covariate.labels = tab1VarLabels
          )

