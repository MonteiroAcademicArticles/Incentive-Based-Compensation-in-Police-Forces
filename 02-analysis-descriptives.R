#------------------------------------------------------------------------------#

#	 SIM - Descriptive Stats	

#------------------------------------------------------------------------------#



# This code depends on analysis_main.R to run! So if this option is not selected
# on master it will be run here
if(!RUN_main_analysis){
  source(file.path(GITHUB, "02-analysis-main.R"))
}



# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = F
}


#------------------------------------------------------------------------------#
#### Load data ####

#sd <- final_data

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
                   "Homicides Attempt",
                   "Other robberies not included in the target",
                   "Vehicle theft",
                   "Street theft",
                   "Maximum prize (R$ 1000)",
                   "District population (1000 hab)",
                   "Number of precint")


stargazer(sd %>% dplyr::select(tab1Vars),
          summary.stat = c("n", "mean", "sd", "min", "max"),
          type = "text",
          out = file.path(OUTPUTS_final, "tab1.html"),
          covariate.labels = tab1VarLabels
          )


#------------------------------------------------------------------------------#
#### Box plot - Number of ocurrences per AISP ####


boxPlotSim <- function(var,
                       xlab,
                       ylab,
                       data){
  ggplot(data = data ,
         aes(y = var,
             x =  factor(aisp, levels = unique(data$aisp) %>% sort(decreasing = T))
         )
  ) + 
    geom_boxplot(col = "dodgerblue4") +
    coord_flip() + 
    labs(x= ylab, y = xlab) +  # Because of coord_flip() these are inverted
  theme_minimal()
  
}

bplot_vd <- 
  boxPlotSim(sd$violent_death_sim,
             data = sd,
             xlab = "Victims of Violent Death",
             ylab = "")

bplot_rr <- 
  boxPlotSim(sd$street_robbery,
             data = sd,
             xlab = "Records of Street Robbery",
             ylab = "")

bplot_vr <- 
  boxPlotSim(sd$vehicle_robbery,
             data = sd,
             xlab = "Records of Vehicle Robbery",
             ylab = "")


if(EXPORT_plots){
  
  bplot_vd %>% print()
    ggsave(filename = file.path(OUTPUTS_final, "Boxplot_violent_death_sim.png"),
           width = 6,
           height = 4)
  dev.off()
  
  bplot_rr %>% print()
    ggsave(filename = file.path(OUTPUTS_final, "Boxplot_street_robbery.png"),
           width = 6,
           height = 4)
  dev.off()
  
  bplot_vr %>% print() 
    ggsave(filename = file.path(OUTPUTS_final, "Boxplot_vehicle_robbery.png"),
           width = 6,
           height = 4)
  dev.off()
  
}



#------------------------------------------------------------------------------#
#### Bar plot 2 - % last months on target per AISP ####


pp_on_target_df <-  sd %>% 
  subset(month %in% c(6,12)) %>% 
  group_by(aisp) %>% 
  summarise(x = sum(hit_sem_l), n_months = n_distinct(year_month),  .groups = "keep") %>% 
  mutate(pp = x/n_months) 

p_months_plot <- 
  ggplot(data = pp_on_target_df ,
         aes(y = pp*100,
             x =  factor(aisp, levels = unique(sd$aisp))
         )
  ) +
  geom_bar(stat="identity", fill = "dodgerblue4")+
  xlab("AISP") +
  ylab("Percentage of semester last month on PFP target") +
  theme_minimal() +
  theme(axis.text.x =element_text(size=7))





if(EXPORT_plots){
  
  p_months_plot  %>% print()
    ggsave(filename = file.path(OUTPUTS_final, "percentage_months_on_target.png"),
           width = 6,
           height = 4)
  
  
}


