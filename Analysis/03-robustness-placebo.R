#------------------------------------------------------------------------------#

#	 SIM - Robustness Placebo

#------------------------------------------------------------------------------#


# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = T
}

#------------------------------------------------------------------------------#
#### Load data ####

# Loading data into a new object to be processed
sr <- final_data

#------------------------------------------------------------------------------#
#### Process data ####

#### Placebo
sr_pl <- sr[sr$year < 2009,]

sr_pl <- sr_pl %>%
  mutate(month1_pla = hit_sem_pla_l*(as.numeric(month %in% c(1,7))),
         month2_pla = hit_sem_pla_l*(as.numeric(month %in% c(2,8))),
         month3_pla = hit_sem_pla_l*(as.numeric(month %in% c(3,9))),
         month4_pla = hit_sem_pla_l*(as.numeric(month %in% c(4,10))),
         month5_pla = hit_sem_pla_l*(as.numeric(month %in% c(5,11))),
         month6_pla = hit_sem_pla_l*(as.numeric(month %in% c(6,12))))

# Create a data set with only target months
dd_df_pla <- sr_pl %>%
  # For table formatting
  mutate(hit_sem_pla_fm_l = hit_sem_pla_l) %>%
  # Keep only regression months
  subset(month %in% c(6,7,12,1)) #%>%

#------------------------------------------------------------------------------#
#### Global objects ####


#------------------------------------------------------------------------------#
#### List regression variables ####

depVars_pla <- c("violent_death_sim",
                 "vehicle_robbery",
                 "street_robbery",
                 "violent_death_sim",
                 "vehicle_robbery",
                 "street_robbery")


indepVars_pla <- c("hit_sem_pla_l",
                   "n_precinct",
                   "population" )

indepVars_pla_dd <- c(
  "hit_sem_pla_fm_l",
  "last_month_on_target_plapre",
  "last_month",
  "n_precinct",
  "population" )


FEVars_pla <- c("aisp",
                "month",
                "year",
                "cmd_name")

FEVars_pla_dd <- c("aisp",
                   "year",
                   "cmd_name")


#------------------------------------------------------------------------------#
### OLS formulas ####
# Right hand side without FE
rFormula <- paste(indepVars, collapse = " + ") 
rFormula_pla <- paste(indepVars_pla, collapse = " + ") 

# Add FE, cluster

clusterVars = c("aisp")
# clusterVars= "0"

clusterVars_form <- paste(clusterVars, collapse =  " + ")

FeForumala1 <- paste(FEVars_pla[1:2], collapse = " + ")
config1_pla <- paste("|", FeForumala1, "| 0 | ", clusterVars_form )

FeForumala2_pla <- paste(FEVars_pla, collapse = " + ") # with cmd FE
config2_pla <- paste("|", FeForumala2_pla, "| 0 |  ", clusterVars_form)


#### Final formulas

# Formulas01_str <- paste(depVars_pla, paste(rFormula, config1), sep = " ~ ")
# Formulas02_str <- paste(depVars_pla, paste(rFormula, config2), sep = " ~ ")

# Placebo
Formulas01_pla_str <- paste(depVars_pla, paste(rFormula_pla, config1_pla), sep = " ~ ")
Formulas02_pla_str <- paste(depVars_pla, paste(rFormula_pla, config2_pla), sep = " ~ ")

### Model 2 whith cmnd FE and monthly coefficients - placebo
#### Construct monthly regression formulas
month_dummies <- c("month2_pla", 
                   "month3_pla",
                   "month4_pla",
                   "month5_pla",
                   "month6_pla")

rFormula_pla_plot <- paste(c('hit_sem_pla_l',
                             month_dummies, 
                             # Remove on_targer as it is already in the interactions
                             indepVars_pla[-1]), 
                           collapse = " + ") 

Formulas02_pla_plot_str <- paste(depVars_pla, paste(rFormula_pla_plot, config2_pla), sep = " ~ ")
names(Formulas02_pla_plot_str) <- depVars_pla

# So it's easier to refernce to elements
names(Formulas01_pla_str) <- depVars_pla
names(Formulas02_pla_str) <- depVars_pla
names(Formulas02_pla_plot_str) <- depVars_pla

#------------------------------------------------------------------------------#
#### End of semster formulas ####

# Function to create formulas
reg_formula <- function(dep_vars,
                        indep_vars,
                        FE_vars,
                        instr_vars = 0,
                        custer_vars = clusterVars){
  
  paste_plus <- function(x){
    paste(x, collapse =  " + ")
  }
  
  # Set regression, FEs, cluster SEs and IV
  paste_config <- function(FE_vars,
                           custer_vars,
                           instr_vars){
    paste(" ", 
          paste_plus(FE_vars), 
          paste_plus(instr_vars), 
          paste_plus(custer_vars),
          sep = " | ")
  }
  
  # Combine all elements
  final_formula <- paste(dep_vars, 
                         paste(paste_plus(indep_vars),
                               paste_config(FE_vars,
                                            custer_vars,
                                            instr_vars)), 
                         sep = " ~ ")
  
  
  # Named vector with the dependent variables
  names(final_formula) <- dep_vars
  
  
  # Return named vector of formulas
  return(final_formula)
}


# Placebo formulas
p_dd_formulas_m2 <-
  reg_formula(depVars_pla,
              indepVars_pla_dd,
              FEVars_pla_dd)


#------------------------------------------------------------------------------#
#### Regression tables ####

# Conditional exporting
export <- function(file,
                   export_global = EXPORT_tables,
                   dir = OUTPUTS_final){
  if (export_global){
    out_path <- file.path(OUTPUTS_final, file)
  } else{
    out_path <- NULL
  }
  
}

# If not exporting print tables
if(EXPORT_tables){
  table_type = 'html'
} else{
  table_type = 'text'
}

# Table function
createTable_pla <- function(reg_list, 
                            add_lines_list = NULL,
                            title = "",
                            dep_var_labels = NULL,
                            col_labels = NULL,
                            outPath = NULL,
                            type = 'html',
                            placebo = T,
                            notes = NULL){
  
  if (placebo){
    keep = c(  "hit_sem_pla_l",
               "last_month_on_target_plapre")
  }else{

    keep = c("hit_sem_pla_l",
             "last_month_on_target_plapre")
    
  }
  
  stargazer(reg_list,
            keep = keep,
            covariate.labels = c("Elegible",
                                 "Elegible * Month 6"),
            dep.var.labels = dep_var_labels,
            title = title,
            dep.var.caption  = "Number  of  occurrences",
            column.labels   = col_labels,
            add.lines = add_lines_list,
            digits = 3,
            omit.stat = c("rsq","ser", "f"),
            out = outPath,
            type = type,
            notes = notes, notes.append = F, notes.align = 'l', notes.label = ''
  )
}

table_fun_pla <- function(crime_vec,
                          out = NULL,
                          title = "",
                          dep_var_labels = NULL,
                          col_labels = NULL,
                          add_lines_list = NULL,
                          outPath = NULL,
                          type = 'html',
                          ols_data = sr_pl,
                          dd_data = dd_df_pla,
                          placebo = T, notes = NULL){
  
  if (placebo){
    # Specification block template
    table_list_fun <- function(crime){
      list(feRegSim(crime, model = Formulas02_pla_str, data = ols_data),
           # feRegSim(crime, model = Formulas02_pla_plot_str, data = ols_data),
           ddRegSim(crime,formula_vector2 = p_dd_formulas_m2,model = 2, data = dd_data))
    }
  } else {
    # Specification block template
    table_list_fun <- function(crime){
      list(feRegSim(crime, model = Formulas02_str, data = ols_data),
           feRegSim(crime, model = Formulas02_plot_str, data = ols_data),
           ddRegSim(crime,formula_vector2 = dd_formulas_alt_m2,model = 2, data = dd_data))
    }
    
  }
  
  
  # Dinamically set the number of blocks based on the number of dep vars
  tab_list <- list()
  for (i in crime_vec){
    tab_list <- append(tab_list, table_list_fun(i))
  }
  
  # Add column labels
  n_blocks <- length(crime_vec)
  
  if (is.null(col_labels)){
    col_labels <- rep(c("OLS", "DD"), n_blocks)
  }
  
  # Obtaining non-log mean
  crime_vec_no <- crime_vec
  tab_list_no <- list()
  for (i in crime_vec_no){
    tab_list_no <- append(tab_list_no, table_list_fun(i))
  }
  
  # Calculate Y means for each model taking into consideration that they have different original data.  
  ymean_dfs_list <- rep(
    list(ols_data, ols_data, dd_data), 
    length(crime_vec_no))
  
  Ymean_row_vector <- c("Y mean")
  
  for (i in 1:length(tab_list_no)){
    i_ymean <- Ymean(tab_list_no[i][[1]], ymean_dfs_list[i][[1]]) %>% round(2)
    i_df <- 
      Ymean_row_vector <- c(Ymean_row_vector, i_ymean)
    
  }
  
  
  # Add lines at the bottom of the table
  if (is.null(add_lines_list)){
    add_lines_list <- 
      list(
        c("Month FE", rep(c('Yes', "No"), n_blocks)),
        Ymean_row_vector,
        c("Number of aisp", rep("39", 4*n_blocks))
      )
  }
  
  
  # Create final table
  tab_list %>% createTable_pla(add_lines_list = add_lines_list,
                               title = title,
                               dep_var_labels = dep_var_labels,
                               col_labels = col_labels,
                               outPath = outPath,
                               type = type,
                               placebo = placebo,
                               notes = notes)
  
}


# Table main
table_fun_pla(c('violent_death_sim',
                'vehicle_robbery',
                'street_robbery'),
              dep_var_labels = c("Violent deaths", 
                                 "Vehicle robbery (Carjacking)",	
                                 "Street robbery"),
              title = "Table A3 - Effect of expectation of receiving bonuses on crime rates (Placebo analysis between 2005 and 2008)",          
              outPath = export("Table A3 - Effect of expectation of receiving bonuses on crime rates (Placebo analysis between 2005 and 2008).html"),
              type = table_type,
              placebo = T,
              notes = c('Note: This table reports regression results for district-level crime from the first semester of 2005 through the second semester of 2008. Columns 3, 6, and 9 restrict the sample to the last and first month of each semester. The main variables include eligibility for the bonus and an interaction with the final month of the semester. Controls in all columns include the number of civil police precincts, commander fixed effects, and fixed effects for district, month of the semester, and year. Robust standard errors, clustered at the district level, are reported in parentheses. All dependent variables are log-transformed. * p<0.1, **p<0.05, *** p<0.01.'))

