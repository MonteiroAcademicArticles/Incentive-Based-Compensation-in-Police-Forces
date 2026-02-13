#------------------------------------------------------------------------------#

#	 SIM - Robustness poisson

#------------------------------------------------------------------------------#

# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = F
}


#------------------------------------------------------------------------------#
# Load data

# sr <- raw_data %>%
sr <- final_data %>%
  # Removing lower and upper bounds we don't have data on batallion sizes after
  # 2015 and the system begun in the second semmester of 2009
  subset((year*10 + semester > 20091))

# Create a data set with only target months
dd_df <- sr %>%
  # Keep only regression months
  subset(month %in% c(6,7,12,1)) #%>%


#------------------------------------------------------------------------------#
#### Poisson formulas ####


# Remove max prize for some reason
poisson_indepVars <- indepVars[!indepVars %in% c("max_prize", "population")]

# Fixed effects
sFormulaFE_poi1 <- paste(paste0("factor(",FEVars[-4],")"), collapse = " + ") 
sFormulaFE_poi2 <- paste(paste0("factor(",FEVars,")"), collapse = " + ") 


# Construct right hand side for eq.
rFormula_poi_0 <- paste(poisson_indepVars, collapse = " + ") 

# Add FEs
rFormula_poi_1 <-  paste(rFormula_poi_0, "+", sFormulaFE_poi1)
rFormula_poi_2 <-  paste(rFormula_poi_0, "+", sFormulaFE_poi2)
rFormula_poi_plot <-  paste(c('hit_sem_l + month2 + month3 + month4 + month5 + month6 + n_precinct'), "+", sFormulaFE_poi2)


# Add Exposure variable

# Exposure Variable
exposure_variable <- "population"

rFormula_poi1 <-  paste(rFormula_poi_1,
                        "+", 
                        paste0(" offset(log(", 
                               exposure_variable, 
                               "))"
                        )
)

rFormula_poi2 <-  paste(rFormula_poi_2,
                        "+", 
                        paste0(" offset(log(", 
                               exposure_variable, 
                               "))"
                        )
)


rFormula_poi2_plot <-  paste(rFormula_poi_plot,
                             "+", 
                             paste0(" offset(log(", 
                                    exposure_variable, 
                                    "))"
                             )
)


# Final formula
Formulas_poi_str1 <- paste(depVars, rFormula_poi1, sep = " ~ ")
names(Formulas_poi_str1) <- depVars
Formulas_poi_str2 <- paste(depVars, rFormula_poi2, sep = " ~ ")
names(Formulas_poi_str2) <- depVars
rFormula_poi2_plot <- paste(depVars, rFormula_poi2_plot, sep = " ~ ")
names(rFormula_poi2_plot) <- depVars

#------------------------------------------------------------------------------#
#### Poisson models ####


# Set regressions model formula
RegPoisson <- function(dep_var,
                       model = 1,
                       formula_vector1 = Formulas_poi_str1,
                       formula_vector2 = Formulas_poi_str2,
                       data = sr){
  if (model == 1){
    formula_vector = formula_vector1
  } else {
    formula_vector = formula_vector2
  }
  
  
  form <- as.formula(formula_vector[dep_var])
  model <- glm(form, family = poisson, data = data)
  
  # Return regression object
  return(model)
  
}


p_vd <- RegPoisson("violent_death_sim")

p_vr <- RegPoisson("vehicle_robbery")

p_rr <- RegPoisson("street_robbery")


p_vd2 <- RegPoisson("violent_death_sim", model = 2)

p_vr2 <- RegPoisson("vehicle_robbery", model = 2)

p_rr2 <- RegPoisson("street_robbery", model = 2)

p_vd_plot <- RegPoisson("violent_death_sim", formula_vector1 = rFormula_poi2_plot)

p_vr_plot <- RegPoisson("vehicle_robbery", formula_vector1 = rFormula_poi2_plot)

p_rr_plot <- RegPoisson("street_robbery", formula_vector1 = rFormula_poi2_plot)

#------------------------------------------------------------------------------#
#### Poisson DD ####


poisson_indepVars_dd <- 
  indep_vars_dd[!(indep_vars_dd %in% c("max_prize", "population"))]

# Exposure Variable
exposure_variable <- "population"

rFormula_poi1 <-  paste(rFormula_poi_1,
                        "+", 
                        paste0(" offset(log(", 
                               exposure_variable, 
                               "))")
)



# Function to create formulas
reg_formula <- function(dep_vars,
                        indep_vars,
                        FE_vars,
                        exposure_variable = "population"){
  
  paste_plus <- function(x){
    paste(x, collapse =  " + ")
  }
  
  
  indep_vars <- 
    paste(paste_plus(indep_vars),
          paste0(" offset(log(", exposure_variable, "))"),
          paste(paste0("factor(",FE_vars_dd,")"), collapse = " + "), sep = "+")
  
  # Combine all elements
  final_formula <- 
    paste(dep_vars,indep_vars, sep = " ~ ")
  
  
  # Named vector with the dependent variables
  names(final_formula) <- dep_vars
  
  
  # Return named vector of formulas
  return(final_formula)
}


# Second model with chief FE
dd_formulas_poi <-
  reg_formula(depVars,
              str_replace_all(poisson_indepVars_dd, 'hit_sem_l','hit_sem_l_fm'),
              FE_vars_dd)


#------------------------------------------------------------------------------#
#### Poisson diff in diff ####


# Set regressions model formula
ddPoiSim <- function(dep_var,
                     formula_vector = dd_formulas_poi,
                     data = dd_df){
  
  
  form <- as.formula(formula_vector[dep_var])
  model <- glm(form, family = poisson, data = data)
  
  # Return regression object
  return(model)
  
}


# Tablev 2
p_vd_dd <- ddPoiSim('violent_death_sim')

p_vr_dd <- ddPoiSim("vehicle_robbery")

p_rr_dd <- ddPoiSim("street_robbery")

#------------------------------------------------------------------------------#
#### Export ####

# Poisson model
tab5_regs <-
  list(p_vd2,
       p_vd_plot,
       p_vd_dd,
       p_vr2,
       p_vr_plot,
       p_vr_dd,
       p_rr2,
       p_rr_plot,
       p_rr_dd)


Ymean_row_vector <- c(mean(p_vd2$model$violent_death_sim),
                      mean(p_vd_plot$model$violent_death_sim),
                      mean(p_vd_dd$model$violent_death_sim),
                      mean(p_vr2$model$vehicle_robbery),
                      mean(p_vr_plot$model$vehicle_robbery),
                      mean(p_vr_dd$model$vehicle_robbery),
                      mean(p_rr2$model$street_robbery),
                      mean(p_rr_plot$model$street_robbery),
                      mean(p_rr_dd$model$street_robbery))

tab5_addLines <- list(
  c("Month FE", rep(c("Yes", "Yes", "No"), 3)),
  c("Y mean",Ymean_row_vector %>% round(2)),
  c("Number of aisp", rep("39", 9)))

if(EXPORT_tables){
  
  createTable(reg_list = tab5_regs,
              add_lines_list = tab5_addLines,
              dep_var_labels = c("Violent deaths",
                                 "Vehicle robbery (Carjacking)",
                                 "Street robbery"),
              title = "Table A5 - Robustness: Poisson Regressions",
              outPath = file.path(OUTPUTS_final, "Table A5 - Robustness Poisson Regressions.html"),
              notes = c('Note: This table reports regression results for district-level crime from the second semester of 2009 through the first semester of 2015, using a Poisson Model. Columns 3, 6, and 9 restrict the sample to the last and first month of each semester. The main variables include eligibility for the bonus, its interactions with semester–month indicators, and an interaction with the final month of the semester. Controls in all columns include the maximum bonus, number of civil police precincts, commander fixed effects, and fixed effects for district, month of the semester, and year. Robust standard errors, clustered at the district level, are reported in parentheses. * p<0.1, ** p<0.05, *** p<0.01.'))
}

