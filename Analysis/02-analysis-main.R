#------------------------------------------------------------------------------#

#	 SIM - Main Regressions nova

#------------------------------------------------------------------------------#

# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = T
  EXPORT_plots = F
  EXPORT_tables = T
}


#------------------------------------------------------------------------------#
# Load data

sr <- final_data %>%
  # Removing lower and upper bounds we don't have data on batallion sizes after
  # 2015 and the system begun in the second semmester of 2009
  subset((year*10 + semester > 20091))

placebo_data <- final_data %>%
  # Removing lower and upper bounds we don't have data on batallion sizes after
  # 2015 and the system begun in the second semmester of 2009
  subset((year*10 + semester < 20091))

# Diff in diff
# Create a data set with only target months
dd_df <- sr %>%
  # Keep only regression months
  subset(month %in% c(6,7,12,1))


#------------------------------------------------------------------------------#
#### OLS formulas ####

# right hand side without FE
rFormula <- paste(indepVars, collapse = " + ") 


# Add FE, cluster and instruments

# clusterVars = c("latitude", "longitude" )
clusterVars = c("aisp" )
# clusterVars= "0"

clusterVars_form <- paste(clusterVars, collapse =  " + ")

config0 <- paste("|", 0, "| 0 | ", clusterVars_form )

FeForumala1 <- paste(FEVars[1:2], collapse = " + ")
config1 <- paste("|", FeForumala1, "| 0 | ", clusterVars_form )

FeForumala2 <- paste(FEVars, collapse = " + ") # with cmd FE
config2 <- paste("|", FeForumala2, "| 0 |  ", clusterVars_form)


#### Final formulas

Formulas00_str <- paste(depVars, paste('hit_sem_l', config0), sep = " ~ ")
Formulas01_str <- paste(depVars, paste(rFormula, config1), sep = " ~ ")
Formulas02_str <- paste(depVars, paste(rFormula, config2), sep = " ~ ")

# So it's easier to reference to elements
names(Formulas00_str) <- depVars
names(Formulas01_str) <- depVars
names(Formulas02_str) <- depVars

#### Construct monthly regression formulas
month_dummies <- c("month2", 
                   "month3",
                   "month4",
                   "month5",
                   "month6")

rFormula00_plot <- paste(c('hit_sem_l',
                           month_dummies), 
                         collapse = " + ") 

rFormula_plot <- paste(c('hit_sem_l',
                         month_dummies, 
                         # Remove on_targer as it is already in the interactions
                         indepVars[-1]), 
                       collapse = " + ") 

# Montlhy formulas
Formulas00_plot_str <- paste(depVars, paste(rFormula00_plot, '|month'), sep = " ~ ")
names(Formulas00_plot_str) <- depVars

Formulas02_plot_str <- paste(depVars, paste(rFormula_plot, config2), sep = " ~ ")
names(Formulas02_plot_str) <- depVars
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


# First model without chief FE (differente name for treatment variable for formatting purposes)
dd_formulas_alt_m0 <- 
  paste(depVars, '~',
        paste(c('hit_sem_fm_l', 'month6', 'last_month'), collapse = ' + '))

names(dd_formulas_alt_m0) <- depVars

dd_formulas_m1 <- 
  reg_formula(depVars,
              indep_vars_dd,
              FE_vars_dd[1:3])



# Second model with chief FE
dd_formulas_alt_m2 <- 
  reg_formula(depVars,
              c("hit_sem_fm_l", "month6",
                "last_month",
                "n_precinct",
                "max_prize",
                "population"),
              FE_vars_dd)



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



# Table main
table_fun(c('violent_death_sim',
            'vehicle_robbery',
            'street_robbery'),
          dep_var_labels = c("Violent deaths", 
                             "Vehicle robbery (Carjacking)",	
                             "Street robbery"),
          title = "Table 2 - Main Results: Effect of expectancy of receiving bonuses on crime rates",
          outPath = export("Table 2 - Main Results Effect of expectancy of receiving bonuses on crime rates.html"),
          type = table_type,
          notes = c('Note: This table reports regression results for district-level crime from the second semester of 2009 through the first semester of 2015. Columns 3, 6, and 9 restrict the sample to the last and first month of each semester. The main variables include eligibility for the bonus, its interactions with semester–month indicators, and an interaction with the final month of the semester. Controls in all columns include the maximum bonus, number of civil police precincts, commander fixed effects, and fixed effects for district, month of the semester and year. Robust standard errors, clustered at the district level, are reported in parentheses. All dependent variables are log-transformed. * p<0.1, ** p<0.05, *** p<0.01.'))

# Table deaths
table_fun(c('homicide',
            'police_killing',
            'dpolice_killing'),
          dep_var_labels = c("Homicides",
                             "Police killing", 
                             "Police killing (dummy)"),
          title = "Table 3 – Disagregation of Violent Deaths",
          outPath = export("Table 3 – Disagregation of Violent Deaths.html"),
          type = table_type,
          notes = c('Note: This table reports regression results for district-level crime from the second semester of 2009 through the first semester of 2015. Columns 3, 6, and 9 restrict the sample to the last and first month of each semester. The main variables include eligibility for the bonus, its interactions with semester–month indicators, and an interaction with the final month of the semester. Controls in all columns include the maximum bonus, number of civil police precincts, commander fixed effects, and fixed effects for district, month of the semester and year. Robust standard errors, clustered at the district level, are reported in parentheses. All dependent variables are log-transformed. * p<0.1, ** p<0.05, *** p<0.01.'))



# Table gaming 
table_fun(c('body_bones_found',
            'attempt_homicide',
            'other_robberies',
            'vehicle_theft',
            'street_theft'),
          dep_var_labels = c("Cadavers and Bones Found",
                             "Attempted Murders",
                             "Other robberies not included in the target",
                             "Vehicle theft",
                             "Street theft"),
          title = "Table 5 - Expectancy of receiving bonuses and gaming",
          outPath = export("Table 5 - Expectancy of receiving bonuses and gaming.html"),
          type = table_type,
          notes = c('Note: This table reports regression results for district-level crime from the second semester of 2009 through the first semester of 2015. Columns 3, 6, and 9 restrict the sample to the last and first month of each semester. The main variables include eligibility for the bonus, its interactions with semester–month indicators, and an interaction with the final month of the semester. Controls in all columns include the maximum bonus, number of civil police precincts, commander fixed effects, and fixed effects for district, month of the semester and year. Robust standard errors, clustered at the district level, are reported in parentheses. All dependent variables are log-transformed. * p<0.1, ** p<0.05, *** p<0.01.'))



#------------------------------------------------------------------------------#

# Regression and table formatting pipeline for models with almost eligible variable
Formulas02_str_05 <- str_replace_all(Formulas02_str, 'hit_sem_l', 'hit_05_l')
names(Formulas02_str_05) <- names(Formulas02_str)

Formulas02_plot_str_05 <- str_replace_all(Formulas02_plot_str, 'hit_sem_l', 'hit_05_l')
Formulas02_plot_str_05 <- str_replace_all(Formulas02_plot_str_05, 'month2', 'month2_05')
Formulas02_plot_str_05 <- str_replace_all(Formulas02_plot_str_05, 'month3', 'month3_05')
Formulas02_plot_str_05 <- str_replace_all(Formulas02_plot_str_05, 'month4', 'month4_05')
Formulas02_plot_str_05 <- str_replace_all(Formulas02_plot_str_05, 'month5', 'month5_05')
Formulas02_plot_str_05 <- str_replace_all(Formulas02_plot_str_05, 'month6', 'month6_05')

names(Formulas02_plot_str_05) <- names(Formulas02_plot_str)

dd_formulas_alt_m2_05 <- str_replace_all(dd_formulas_alt_m2, 'hit_sem_fm_l ', 'hit_05_fm_l')
dd_formulas_alt_m2_05 <- str_replace_all(dd_formulas_alt_m2_05, 'month6', 'month6_05')
names(dd_formulas_alt_m2_05) <- names(dd_formulas_alt_m2)

createTable_alt_05 <- function(reg_list, 
                               add_lines_list = NULL,
                               title = "",
                               dep_var_labels = NULL,
                               col_labels = NULL,
                               outPath = NULL,
                               type = 'html',
                               placebo = F,
                               notes = NULL){
  
  if (placebo){
    keep = c(  "hit_05_l",
               "last_month_on_target05",
               'month2_05',
               'month3_05',
               'month4_05',
               'month5_05',
               'month6_05')
  }else{
    keep = c("hit_05_l",
             'month2_05',
             'month3_05',
             'month4_05',
             'month5_05',
             'month6_05')
  }
  
  stargazer(reg_list,
            keep = keep,
            covariate.labels = c("Almost Eligible (5%)",
                                 'Almost Eligible (5%)* Month 2',
                                 'Almost Eligible (5%)* Month 3',
                                 'Almost Eligible (5%)* Month 4',
                                 'Almost Eligible (5%)* Month 5',
                                 'Almost Eligible (5%)* Month 6'),
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

table_fun_alt_05 <- function(crime_vec,
                             out = NULL,
                             title = "",
                             dep_var_labels = NULL,
                             col_labels = NULL,
                             add_lines_list = NULL,
                             outPath = NULL,
                             type = 'html',
                             ols_data = sr,
                             dd_data = dd_df,
                             placebo = F,
                             notes = notes){
  
  if (placebo){
    # Specification block template
    table_list_fun <- function(crime){
      list(feRegSim(crime, model = 'placebo_01', data = sr_pl),
           feRegSim(crime, model = 'placebo_02', data = sr_pl),
           ddRegSim_pla(crime))
    }
  } else {
    # Specification block template
    table_list_fun <- function(crime){
      list(feRegSim(crime, model = Formulas02_str_05, data = ols_data),
           feRegSim(crime, model = Formulas02_plot_str_05, data = ols_data),
           ddRegSim(crime,formula_vector2 = dd_formulas_alt_m2_05,model = 2, data = dd_data))
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
    col_labels <- rep(c("OLS","OLS", "DD"), n_blocks)
  }
  
  # Obtaining non-log mean
  # crime_vec_no <- str_remove_all(crime_vec, '')
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
        # c("Chief FE", rep(c("Yes", "Yes", "Yes"), n_blocks)),
        # c("AISPxSemester FE", rep(c("Yes",   "Yes", 'Yes'), n_blocks)),
        c("Month FE", rep(c("Yes",   "Yes", 'No'), n_blocks)),
        # Ymean_row(tab_list),
        Ymean_row_vector,
        c("Number of aisp", rep("39", 3*n_blocks))
      )
  }
  
  
  # Create final table
  tab_list %>% createTable_alt_05(add_lines_list = add_lines_list,
                                  title = title,
                                  dep_var_labels = dep_var_labels,
                                  col_labels = col_labels,
                                  outPath = outPath,
                                  type = type,
                                  placebo = placebo,
                                  notes = notes)
  
}
# Tabela 4 --------------------------
table_fun_alt_05(c('violent_death_sim',
                   'vehicle_robbery',
                   'street_robbery'),
                 dep_var_labels = c("Violent deaths", 
                                    "Vehicle robbery (Carjacking)",
                                    "Street robbery"),
                 title = "Table A1 - Effect of expectancy of receiving bonuses on crime rates with flexible targets",
                 outPath = export("Table A1 - Effect of expectancy of receiving bonuses on crime rates with flexible targets.html"),
                 type = table_type,
                 notes = c('Note: This table reports regression results for district-level crime from the second semester of 2009 through the first semester of 2015. Columns 3, 6, and 9 restrict the sample to the last and first month of each semester. The main variables include almost eligibility for the bonus, its interactions with semester–month indicators, and an interaction with the final month of the semester. Controls in all columns include the maximum bonus, number of civil police precincts, commander fixed effects, and fixed effects for district, month of the semester, and year. Robust standard errors, clustered at the district level, are reported in parentheses. All dependent variables are log-transformed. * p<0.1, ** p<0.05, *** p<0.01.'))



# Pre-Trends ------------------------------------------------------------------------------------
sr_pt <- sr %>%
  left_join(sr %>%
              group_by(aisp, sem_year) %>%
              summarise(hit_sem_l_6 = as.numeric(hit_sem_l[month %in% c(6,12)] == 1)) %>%
              ungroup() %>%
              group_by(aisp) %>%
              mutate(hit_sem_l_6 = dplyr::lag(hit_sem_l_6)),
            by = c('aisp', 'sem_year')) %>%
  mutate(month_alt7 = hit_sem_l_6*(as.numeric(month %in% c(1,7))),
         month_alt8 = hit_sem_l_6*(as.numeric(month %in% c(2,8))),
         month_alt9 = hit_sem_l_6*(as.numeric(month %in% c(3,9))),
         month_alt10 = hit_sem_l_6*(as.numeric(month %in% c(4,10))),
         month_alt11 = hit_sem_l_6*(as.numeric(month %in% c(5,11))),
         month_alt12 = hit_sem_l_6*(as.numeric(month %in% c(6,12)))) %>%
  filter(month %in% c(1,7,2,8))


Formulas_pt_str <- paste(depVars, paste('month_alt7  + n_precinct + max_prize + population', config2), sep = " ~ ")
names(Formulas_pt_str) <- depVars

createTable_pt <- function(reg_list, 
                           add_lines_list = NULL,
                           title = "",
                           dep_var_labels = NULL,
                           col_labels = NULL,
                           outPath = NULL,
                           type = 'html',
                           placebo = F,
                           notes = NULL){
  
  if (placebo){
    keep = c(  'month_alt7',
               'month_alt8')
  }else{
    keep = c(  'month_alt7',
               'month_alt8')
  }
  
  stargazer(reg_list,
            keep = keep,
            covariate.labels = c('Eligible (at t=6) * Month 7',
                                 'Eligible (at t=6) * Month 8'),
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

table_fun_pt <- function(crime_vec,
                         out = NULL,
                         title = "",
                         dep_var_labels = NULL,
                         col_labels = NULL,
                         add_lines_list = NULL,
                         outPath = NULL,
                         type = 'html',
                         ols_data = sr,
                         dd_data = dd_df,
                         placebo = F,
                         notes = NULL){
  
  if (placebo){
    # Specification block template
    table_list_fun <- function(crime){
      list(feRegSim(crime, model = 'placebo_01', data = sr_pl),
           feRegSim(crime, model = 'placebo_02', data = sr_pl),
           ddRegSim_pla(crime))
    }
  } else {
    # Specification block template
    table_list_fun <- function(crime){
      list(feRegSim(crime, model = Formulas_pt_str, data = ols_data) )
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
    col_labels <- rep(c("OLS"), n_blocks)
  }
  
  # Obtaining non-log mean
  # crime_vec_no <- str_remove_all(crime_vec, '')
  crime_vec_no <- crime_vec
  tab_list_no <- list()
  for (i in crime_vec_no){
    tab_list_no <- append(tab_list_no, table_list_fun(i))
  }
  
  # Calculate Y means for each model taking into consideration that they have different original data.  
  ymean_dfs_list <- rep(
    list(ols_data), 
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
        # c("Chief FE", rep(c("Yes"), n_blocks)),
           # c("AISPxSemester FE", rep(c('Yes'), n_blocks)),
           c("Month FE", rep(c('Yes'), n_blocks)),
           # Ymean_row(tab_list),
           Ymean_row_vector,
           c("Number of aisp", rep("39", 3*n_blocks))
      )
  }
  
  
  # Create final table
  tab_list %>% createTable_pt(add_lines_list = add_lines_list,
                              title = title,
                              dep_var_labels = dep_var_labels,
                              col_labels = col_labels,
                              outPath = outPath,
                              type = type,
                              placebo = placebo,
                              notes = notes)
  
}


table_fun_pt(c('violent_death_sim',
               'vehicle_robbery',
               'street_robbery',
               'homicide',
               'police_killing',
               'dpolice_killing',
               'body_bones_found',
               'attempt_homicide',
               'other_robberies',
               'vehicle_theft',
               'street_theft'),
             dep_var_labels = c("Violent deaths", 
                                "Vehicle robbery (Carjacking)",
                                "Street robbery",
                                "Homicides",
                                "Police killing", 
                                "Police killing (dummy)",
                                "Cadavers and Bones Found",
                                "Attempted Murders",
                                "Other robberies not included in the target",
                                "Vehicle theft",
                                "Street theft"),
             title = "Table A2 – Parallel-trend test",
             outPath = export("Table A2 – Parallel-trend test.html"),
             type = table_type,
             ols_data = sr_pt,
             notes = c('Note: This table reports regression results for district-level crime from the second semester of 2009 through the first semester of 2015. The main variables include eligibility for the bonus in semester s and its interactions with month 1 at semester s+1. The sample includes the months one and two for semester s+1. The regressions control for the maximum bonus, number of civil police precincts, commander fixed effects, and fixed effects for district, year and month of the semester. Robust standard errors, clustered at the district level, are reported in parentheses. All dependent variables are log-transformed. * p<0.1, ** p<0.05, *** p<0.01.'))

# Above Target by M4 ---------------------------
sr2 <- sr %>%
  left_join(
    final_data %>%
      mutate(month1 = as.numeric(month %in% c(1,7)),
             month2 = as.numeric(month %in% c(2,8)),
             month3 = as.numeric(month %in% c(3,9)),
             month4 = as.numeric(month %in% c(4,10)),
             month5 = as.numeric(month %in% c(5,11)),
             month6 = as.numeric(month %in% c(6,12))) %>%
      subset((year*10 + semester > 20091)) %>%
      group_by(aisp, semester, year) %>%
      summarise(accum_vd_m4 = violent_death_sim_cum2[month4==1],
                target_vd_sem_adjusted = mean( target_vd_sem_adjusted)) %>%
      arrange(aisp, year, semester) %>%
      mutate(above_meta_vd_m4 = as.numeric(accum_vd_m4> target_vd_sem_adjusted)) %>%
      select(year,above_meta_vd_m4),
    by = c('aisp', 'semester', 'year')
  )

# Sample
sr_new_ab4 <- sr2 %>%
  # sr <- org_data %>%
  # Removing lower and upper bounds we don't have data on batallion sizes after
  # 2015 and the system begun in the second semmester of 2009
  subset((year*10 + semester > 20091)) %>%
  subset(month %in% c(5,6,11,12)) %>%
  mutate(month5 = hit_sem_l*(as.numeric(month %in% c(5,11))),
         month6 = hit_sem_l*(as.numeric(month %in% c(6,12))),
         month5_ab4 = above_meta_vd_m4*(as.numeric(month %in% c(5,11))),
         month6_ab4 = above_meta_vd_m4*(as.numeric(month %in% c(6,12))))


Formulas01_str_ab4 <- str_replace_all(Formulas01_str, 'hit_sem_l', 'hit_sem_l + above_meta_vd_m4')
Formulas01_str_ab4 <- str_replace_all(Formulas01_str_ab4, 'aisp_semester', 'aisp')
names(Formulas01_str_ab4) <- names(Formulas01_str)


Formulas02_str_ab4 <- str_replace_all(Formulas02_str, 'hit_sem_l', 'hit_sem_l + above_meta_vd_m4')
Formulas02_str_ab4 <- str_replace_all(Formulas02_str_ab4, 'aisp_semester', 'aisp')
names(Formulas02_str_ab4) <- names(Formulas02_str)

Formulas02_plot_str_ab4 <- str_replace_all(Formulas02_plot_str, '\\+ month2', '')
Formulas02_plot_str_ab4 <- str_replace_all(Formulas02_plot_str_ab4, '\\+ month3', '')
Formulas02_plot_str_ab4 <- str_replace_all(Formulas02_plot_str_ab4, '\\+ month4', '')
Formulas02_plot_str_ab4 <- str_replace_all(Formulas02_plot_str_ab4, '\\+ month5', '')
Formulas02_plot_str_ab4 <- str_replace_all(Formulas02_plot_str_ab4, 'month6', 'month6 + above_meta_vd_m4 + month6_ab4')
Formulas02_plot_str_ab4 <- str_replace_all(Formulas02_plot_str_ab4, 'aisp_semester', 'aisp')
names(Formulas02_plot_str_ab4) <- names(Formulas02_plot_str)


createTable_ab4 <- function(reg_list, 
                            add_lines_list = NULL,
                            title = "",
                            dep_var_labels = NULL,
                            col_labels = NULL,
                            outPath = NULL,
                            type = 'html',
                            placebo = F,
                            notes = NULL){
  
  if (placebo){
    keep = c(  'hit_sem_l',
               "above_meta_vd_m4")
  }else{
    keep = c('hit_sem_l',
             "above_meta_vd_m4")
  }
  
  stargazer(reg_list,
            keep = keep,
            covariate.labels = c('Eligible',
                                 "Above Target by M4"),
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

table_fun_ab4 <- function(crime_vec,
                          out = NULL,
                          title = "",
                          dep_var_labels = NULL,
                          col_labels = NULL,
                          add_lines_list = NULL,
                          outPath = NULL,
                          type = 'html',
                          ols_data = sr_new_ab4,
                          dd_data = dd_df,
                          placebo = F,
                          notes = NULL){
  
  if (placebo){
    # Specification block template
    table_list_fun <- function(crime){
      list(feRegSim(crime, model = 'placebo_01', data = sr_pl),
           feRegSim(crime, model = 'placebo_02', data = sr_pl),
           ddRegSim_pla(crime))
    }
  } else {
    # Specification block template
    table_list_fun <- function(crime){
      list(feRegSim(crime, model = Formulas02_str_ab4, data = sr_new_ab4))
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
    col_labels <- rep(c("OLS"), n_blocks)
  }
  
  # Obtaining non-log mean
  # crime_vec_no <- str_remove_all(crime_vec, '')
  crime_vec_no <- crime_vec
  tab_list_no <- list()
  for (i in crime_vec_no){
    tab_list_no <- append(tab_list_no, table_list_fun(i))
  }
  
  # Calculate Y means for each model taking into consideration that they have different original data.  
  ymean_dfs_list <- rep(
    list(sr_new_ab4), 
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
        # c("Chief FE", rep(c("Yes"), n_blocks)),
           # c("AISPxSemester FE", rep(c('Yes', "Yes",   "Yes"), n_blocks)),
           c("Month FE", rep(c('Yes'), n_blocks)),
           # Ymean_row(tab_list),
           Ymean_row_vector,
           c("Number of aisp", rep("39", 1*n_blocks))
      )
  }
  
  
  # Create final table
  tab_list %>% createTable_ab4(add_lines_list = add_lines_list,
                               title = title,
                               dep_var_labels = dep_var_labels,
                               col_labels = col_labels,
                               outPath = outPath,
                               type = type,
                               placebo = placebo,
                               notes = notes)
  
}

table_fun_ab4(c('violent_death_sim',
                'vehicle_robbery',
                'street_robbery'),
              dep_var_labels = c("Violent deaths", 
                                 "Vehicle robbery (Carjacking)",
                                 "Street robbery",
                                 "Street robbery"),
              title = "Table 4 - Effect on crime outcomes after police officers are no longer eligible",
              outPath = export("Table 4 - Effect on crime outcomes after police officers are no longer eligible.html"),
              type = table_type,
              notes = c('Note: This table reports regression results for district-level crime from the second semester of 2009 through the first semester of 2015. Columns 1 to 3 restrict the sample to the fifth and sixth month of each semester. Main variables include eligibility for the bonus, its interactions with an indicator if the crime levels of the district were above the target by the fourth month of the semester. Controls include the maximum bonus, number of civil police precincts, commander fixed effects, and fixed effects for district, year and month of the semester. Robust standard errors clustered at the district level in parentheses. All dependent variables are log-transformed. All dependent variables are log-transformed. * p<0.1, ** p<0.05, *** p<0.01.'))


