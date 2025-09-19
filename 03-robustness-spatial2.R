#------------------------------------------------------------------------------#

#	 SIM - Robustness Spatial

#------------------------------------------------------------------------------#

# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = F
}

#------------------------------------------------------------------------------#
#### Load data ####



# Loading data into a new object to be processed
sr <- final_data %>%
  # sr <- org_data %>%
  # Removing lower and upper bounds we don't have data on batallion sizes after
  # 2015 and the system begun in the second semmester of 2009
  subset((year*10 + semester > 20091))

# Load aisps shapefile
aisp <- st_read(dsn = GIS, layer = "lm_aisp_072024")
aisp <- st_make_valid(st_as_sf(aisp))

aisp <- st_as_sf(aisp) %>%
  group_by(aisp) %>%
  summarise(across(c('shape_Leng','shape_Area','AREA_GEO') , sum))
#------------------------------------------------------------------------------#
#### Process data ####

#### Spatial analysis

# Keep only analysis years
sr_sl <- sr %>% subset(sem_year > 100)


# Remove ilha do governador and keep balanced panel
sr_sl <- sr_sl[!(sr_sl$aisp %in% c(17,41,43))]
aisp <- aisp %>%
  filter(!aisp %in% c(17,41,43))


#------------------------------------------------------------------------------#
#### Global objects ####


#------------------------------------------------------------------------------#
#### List regression variables ####

depVars <- c("violent_death_sim_log",
             "vehicle_robbery_log",
             "street_robbery_log")

indepVars <- c("hit_sem_l",
               # "policemen_aisp",
               # "policemen_upp",
               "n_precinct",
               "max_prize",
               "population" )

indepVars_plot <- c("hit_sem_l",
                    "month2", 
                    "month3",
                    "month4",
                    "month5",
                    "month6",
                    "n_precinct",
                    "max_prize",
                    "population" )



FEVars <- c("aisp",
            "month_semester",
            "year")



#------------------------------------------------------------------------------#
### Spatial lag formulas ####

# Add FEs
sFormulaFE <- paste0("factor(",FEVars,")")
sFormula1 <- paste(c(indepVars, sFormulaFE), collapse = " + ")

Formulas01_sl_str <- paste(depVars, sFormula1, sep = " ~ ")

names(Formulas01_sl_str) <- depVars

### Model 2 whith cmnd FE and monthly coefficients - placebo
#### Construct monthly regression formulas
month_dummies <- c()

Formulas_sl_plot_str <- paste(c(indepVars_plot,sFormulaFE), collapse = " + ") 

Formulas_sl_plot_str <- paste(depVars, Formulas_sl_plot_str, sep = " ~ ")
names(Formulas_sl_plot_str) <- depVars



#------------------------------------------------------------------------------#
#### Spatial analysis ####


#------------------------#
#### Create variables ####


# Year and month
#sr_sl$year_month <- sr_sl$year*100 + sr_sl$month

# Deaths per 100 thousand
sr_sl$lv_pop <- sr_sl$violent_death_sim/(sr_sl$population/100000)

# Carjack per 100 thousand
sr_sl$vr_pop <- sr_sl$vehicle_robbery /(sr_sl$population/100000)

# Street robbery per 100 thousand
sr_sl$sr_pop <- sr_sl$street_robbery/(sr_sl$population/100000)

# Montly variation - current month minus previous
sr_sl <- sr_sl %>% 
  group_by(aisp) %>% 
  arrange(aisp, year_month) %>% 
  mutate(lv_pop_d = lv_pop - dplyr::lag(lv_pop, n = 1),
         vr_pop_d = vr_pop - dplyr::lag(vr_pop, n = 1),
         sr_pop_d = sr_pop - dplyr::lag(sr_pop, n = 1),
         # Replace inf values
         lv_pop_d = ifelse(!is.finite(lv_pop_d),NA, lv_pop_d),
         vr_pop_d = ifelse(!is.finite(vr_pop_d),NA, vr_pop_d),
         sr_pop_d = ifelse(!is.finite(sr_pop_d),NA, sr_pop_d)) %>%
  ungroup()

# Set panel data indexes
ps <- pdata.frame(sr_sl, index = c("aisp", "year_month"))
ps_dd <- pdata.frame(sr_sl %>% filter(month %in% c(6,7,1,12)), index = c("aisp", "year_month"))


#### Spatial Variables

# Neighbourhood definition
nb <- poly2nb(aisp, queen = T)

# Neighbour weights
lw <- nb2listw(nb,
               style = "W",
               zero.policy = TRUE)

# Calculate spatial lagged values level
ps$lv_pop_slag <- slag(ps$lv_pop, lw)
ps$vr_pop_slag <- slag(ps$vr_pop, lw)
ps$sr_pop_slag <- slag(ps$sr_pop, lw)

#------------------------------------------------------------------------------#
#### Spatial lag models ####

#### Regressions

# SAR model function
SARlag_reg <- function(formula, data, nbW ){
  spml(formula = as.formula(formula), 
       data=data, 
       model = "pooling",
       lag = T,
       listw = nbW)
}


# Column 1
sl_vd_01 <- SARlag_reg(Formulas01_sl_str["violent_death_sim_log"],
                       data = ps,
                       nbW = lw)
sl_vr_01 <- SARlag_reg(Formulas01_sl_str["vehicle_robbery_log"],
                       data = ps,
                       nbW = lw)
sl_sr_01 <- SARlag_reg(Formulas01_sl_str["street_robbery_log"],
                       data = ps,
                       nbW = lw)

# Column 1
sl_vd_02 <- SARlag_reg(Formulas_sl_plot_str["violent_death_sim_log"],
                       data = ps,
                       nbW = lw)
sl_vr_02 <- SARlag_reg(Formulas_sl_plot_str["vehicle_robbery_log"],
                       data = ps,
                       nbW = lw)
sl_sr_02 <- SARlag_reg(Formulas_sl_plot_str["street_robbery_log"],
                       data = ps,
                       nbW = lw)

#------------------------------------------------------------------------------#
#### Moran's I ####

# Moran's I level
# moran_lv <- lm(lv_pop_lag ~ lv_pop, data = ps)
# moran_rv <- lm(vr_pop_lag ~ vr_pop, data = ps) %>% summary
# moran_sr <- lm(sr_pop_lag ~ sr_pop, data = ps) %>% summary


# Moran's I delta
#can't have NAs, so I'm removing all obs where the
#monthly crime difference is NA

ps_semJJ <- ps %>% subset(!(month %in% c(1,7)))


# Calculate spatial lagged values for on_target
ps_semJJ$lv_pop_d_slag <- slag(ps_semJJ$lv_pop_d, lw)
ps_semJJ$vr_pop_d_slag <- slag(ps_semJJ$vr_pop_d, lw)
ps_semJJ$sr_pop_d_slag <- slag(ps_semJJ$sr_pop_d, lw)


moran_lv_01 <- lm(lv_pop_d_slag ~ lv_pop_d, data = ps_semJJ)
moran_vr_01 <- lm(vr_pop_d_slag ~ vr_pop_d, data = ps_semJJ)
moran_sr_01 <- lm(sr_pop_d_slag ~ sr_pop_d, data = ps_semJJ)

moran_lv_02 <- lm(lv_pop_d_slag ~ lv_pop_d + factor(year) + factor(month), data = ps_semJJ)
moran_vr_02 <- lm(vr_pop_d_slag ~ vr_pop_d + factor(year) + factor(month), data = ps_semJJ)
moran_sr_02 <- lm(sr_pop_d_slag ~ sr_pop_d + factor(year) + factor(month), data = ps_semJJ)

#------------------------------------------------------------------------------#
##### Initial Exporting tables ####



#### Export spatial lag MANUALLY ####

# Regression variables
regVars <- c("violent_death_sim_log",
             "vehicle_robbery_log",
             "street_robbery_log",
             "hit_sem_l",
             "policemen_aisp",
             "policemen_upp",
             "n_precinct",
             "max_prize",
             "population",
             "aisp",
             "year",
             "month_semester")

# Regression data
ps_complete_bol <- complete.cases(ps[,regVars])
slregData <- ps[ps_complete_bol,]


# Spatial leg coeff lambda

coefs_lambda <- 
  c(sl_vd_01$arcoef,
    sl_vd_02$arcoef,
    sl_vr_01$arcoef,
    sl_vr_02$arcoef,
    sl_sr_01$arcoef,
    sl_sr_02$arcoef)


# Formatted SEs
seFormatFun <- function(model, var = NULL){
  
  if (is.null(var)){
    se <- abs(model$vcov.arcoef) %>% sqrt()
    coef <- model$arcoef
    
  } else if (var == 'lambda'){
    coef <- model$arcoef
    se <- model$vcov.arcoef %>% sqrt()
    
    } else {
    coef <- model$coefficients[var]
    se <- model$vcov[var,var] %>% sqrt()
  }
  
  
  # Calculate pvalue
  if(abs(coef) > 2.576*se){
    star <- "***"
  } else if(abs(coef) > 1.96*se){
    star <- "**"
  } else if (abs(coef) > 1.645*se){
    star <- "*"
  } else{
    star <- ""
  }
  
  # result <- paste0( "(", round(se,3), ")", star)
  result <- rbind( paste0( round(coef,3),star),
                   paste0( "(", round(se,3), ")"))
  
  return(result)
  
}



coefs <-
  cbind(seFormatFun(sl_vd_01, 'hit_sem_l'),
        seFormatFun(sl_vd_02, 'hit_sem_l'),
        seFormatFun(sl_vr_01, 'hit_sem_l'),
        seFormatFun(sl_vr_02, 'hit_sem_l'),
        seFormatFun(sl_sr_01, 'hit_sem_l'),
        seFormatFun(sl_sr_02, 'hit_sem_l'))


coefs_m2 <-
  cbind(NA,
        seFormatFun(sl_vd_02, 'month2'),
        NA,
        seFormatFun(sl_vr_02, 'month2'),
        NA,
        seFormatFun(sl_sr_02, 'month2'))

coefs_m3 <-
  cbind(NA,
        seFormatFun(sl_vd_02, 'month3'),
        NA,
        seFormatFun(sl_vr_02, 'month3'),
        NA,
        seFormatFun(sl_sr_02, 'month3'))

coefs_m4 <-
  cbind(NA,
        seFormatFun(sl_vd_02, 'month4'),
        NA,
        seFormatFun(sl_vr_02, 'month4'),
        NA,
        seFormatFun(sl_sr_02, 'month4'))

coefs_m5 <-
  cbind(NA,
        seFormatFun(sl_vd_02, 'month5'),
        NA,
        seFormatFun(sl_vr_02, 'month5'),
        NA,
        seFormatFun(sl_sr_02, 'month5'))

coefs_m6 <-
  cbind(NA,
        seFormatFun(sl_vd_02, 'month6'),
        NA,
        seFormatFun(sl_vr_02, 'month6'),
        NA,
        seFormatFun(sl_sr_02, 'month6'))

coefs_lambda <- 
  cbind(seFormatFun(sl_vd_01, 'lambda'),
    seFormatFun(sl_vd_02, 'lambda'),
    seFormatFun(sl_vr_01, 'lambda'),
    seFormatFun(sl_vr_02, 'lambda'),
    seFormatFun(sl_sr_01, 'lambda'),
    seFormatFun(sl_sr_02, 'lambda'))



# N obs
n_obs <- c(sl_vd_01$residuals %>% length(),
           sl_vd_02$residuals %>% length(),
           sl_vr_01$residuals %>% length(),
           sl_vr_02$residuals %>% length(),
           sl_sr_01$residuals %>% length(),
           sl_sr_02$residuals %>% length())

# Number of AISPs
n_aisp <- c(37,37,37,37,37,37)


# Y mean
Ymean_vec <- c(mean(slregData$violent_death_sim),
           mean(slregData$violent_death_sim),
           mean(slregData$vehicle_robbery),
           mean(slregData$vehicle_robbery),
           mean(slregData$street_robbery),
           mean(slregData$street_robbery))

# Adjusted R squared
arsq <- 
  function(model,data, depVar){
    n <- nrow(data)
    k <- length(model) -1
    sse <- sum(model$residuals^2)
    sst <- var( data[,depVar] ) * (n-1)
    rsq <- 1-(sse/sst)
    
    arsq <- 1 -(1-rsq)*((n-1)/(n-k-1))
    return(arsq)
  }

adjus_Rsq <- 
  c(arsq(sl_vd_01, slregData, "violent_death_sim_log"),
    arsq(sl_vd_02, slregData, "violent_death_sim_log"),
    arsq(sl_vr_01, slregData, "vehicle_robbery_log"),
    arsq(sl_vr_02, slregData, "vehicle_robbery_log"),
    arsq(sl_sr_01, slregData, "street_robbery_log"),
    arsq(sl_sr_02, slregData, "street_robbery_log"))


# Add significance level placeholder




#### Create the table ####
#sl_vd_01

slRegTable <- 
  rbind(c("", "","Number of occurrences","","", ""),
        c("Violent  deaths","Violent  deaths",
          "Vehicle  robbery  (Carjacking)", "Vehicle  robbery  (Carjacking)", 
          "Street  robbery","Street  robbery"),
        c("SAR", "SAR", "SAR","SAR", "SAR", "SAR"),
        c("(1)","(1)", "(2)", "(2)", "(3)", "(3)"),
        coefs,
        coefs_m2,
        coefs_m3,
        coefs_m4,
        coefs_m5,
        coefs_m6,
        coefs_lambda,
        c("Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
        Ymean_vec %>% round(2),
        n_aisp,
        n_obs,
        adjus_Rsq %>% round(2))

#colnames(slRegTable) <- c("violent_death_sim", "vehicle_robbery", "street_robbery")


# Add parenthesis to  SE
# slRegTable[5,] <- paste0("(",slRegTable[5,],")")
# slRegTable[7,] <- paste0("(",slRegTable[7,],")")

# Add stars


# Add row names (gambiarra da porra)
rows <- c("",
          " ",
          "  ",
          "   ",
          "Eligible",
          "    ",
          "Eligible * Month 2",
          "     ",
          "Eligible * Month 3",
          "      ",
          "Eligible * Month 4",
          "       ",
          "Eligible * Month 5",
          "        ",
          "Eligible * Month 6",
          "         ",
          "Lambda",
          "          ",
          "Month FE",
          "Y mean",
          "Number of aisp",
          "Observations",
          "R2 adjusted")

# Remove column and row names
rownames(slRegTable) <- (1:dim(slRegTable)[1])


slRegTable <- slRegTable %>%
  as.data.frame(stringsAsFactors = F)

# slRegTable <- 
#   cbind(rows,
#         slRegTable) 
names(slRegTable) <- NA
rownames(slRegTable) <- rows

#------------------------------------------------------------------------------#
##### Processing tables ####

(slRegTable_hux <-
   huxtable(slRegTable, add_colnames = F, add_rownames = T) %>%
   set_tb_padding(2) %>% 
   set_top_border(1, everywhere) %>%
   set_bottom_border(1, 2:7) %>%
   set_bottom_border(18, everywhere) %>%
   set_bottom_border(23, everywhere))


#------------------------------------------------------------------------------#
##### Moran's I plots ####

# Graphics formatting definition
moran_plot <- function(reg, label, data){
  moranI <- reg$coefficients[2] %>% round(4)
  pVal <- summary(reg)$coefficients[regIndepVars(reg)
                                    ,"Pr(>|t|)"] %>% round(4)
  title <- paste("Moran's I =",
                 moranI,
                 "\n",
                 "P value ~",
                 pVal)
  with(data, 
       plot(regIndepVars(reg)%>% get(), 
            regDepVars(reg)%>% get(),
            xlab = label,
            ylab = paste("W", label),
            cex.lab=2,
            cex.axis=1.5))
  abline(reg)
  title(main=title, 
        cex.main=2 )
  
}


#### Export graphs
if(EXPORT_plots){
  
  # Violent death  
  png(file = file.path(OUTPUTS_final, 
                       "moran_lv_01.png"),
      width = 600, 
      height = 600)
  par(mar = c(5, 5, 5, 5))
  moran_plot(moran_lv_01, 
             "Violent Death",
             ps_semJJ)
  dev.off()
  
  # Vehicle rob
  png(file = file.path(OUTPUTS_final, 
                       "moran_vr_01.png"),
      width = 600, 
      height = 600)
  par(mar = c(5, 5, 5, 5))
  moran_plot(moran_vr_01, 
             "Vehicle robbery",
             ps_semJJ)
  dev.off()
  
  # Street rob
  png(file = file.path(OUTPUTS_final, 
                       "moran_sr_01.png"),
      width = 600, 
      height = 600)
  par(mar = c(5, 5, 5, 5))
  moran_plot(moran_sr_01, 
             "Street robbery",
             ps_semJJ)
  dev.off()
  
}

#------------------------------------------------------------------------------#
##### Actually exporting ####


if(EXPORT_tables){
  huxtable::quick_xlsx(slRegTable_hux, file = file.path(OUTPUTS_final, "spatial_lag_unformated.xlsx"))
  
}

