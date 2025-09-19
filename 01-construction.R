#------------------------------------------------------------------------------#

#	 SIM- Construction and Placebo targets calculation				

#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#### Settings ####



# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = T
  EXPORT_plots = F
  EXPORT_tables = F
}



#------------------------------------------------------------------------------#
#### Load constructed data ####

sim <- read.dta13("C:/Users/mathi/Downloads/SIM - CQ/pmrj - Mathias/data/data_SIM_2021-07.dta")

# Loading data into a new object to be processed
sim <- raw_data
sim <- sim[!is.na(sim$aisp) & !is.na(sim$year) & !is.na(sim$month), ]

# Create this useful numeric version of this variable
sim$year_month <- sim$year*100+ sim$month

# Load shapefiles
aisp_shp <- st_read("C:/Users/mathi/Downloads/SIM - CQ/pmrj - Mathias/GIS/lm_aisp_072024.shp")
aisp_shp <- st_make_valid(st_as_sf(aisp_shp))

aisp_shp <- st_as_sf(aisp_shp) %>%
  group_by(aisp) %>%
  summarise(across(c('shape_Leng','shape_Area','AREA_GEO') , sum))

# Load chief IDs
cmd <- fread(file = file.path(DATA, "ComandantesBatalhao.csv"),
             encoding = "Latin-1")


#------------------------------------------------------------------------------#
#### End of semester model variables ####

cimeVars <- c("violent_death_sim",
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
              "burglary",
              "store_robbery",
              "arrest",
              "arrest2",
              "drug_seizure")



# Create regression variables
sim %<>%
  mutate(month_semester = case_when(month %in% c(1,7) ~ 1,
                                    month %in% c(2,8) ~ 2,
                                    month %in% c(3,9) ~ 3,
                                    month %in% c(4,10) ~ 4,
                                    month %in% c(5,11) ~ 5,
                                    month %in% c(6,12) ~ 6),
         aisp_semester = paste0(aisp, '_', (sem_year-98)),
         aisp_bimester = case_when(month %in% c(6,12)  ~ paste0(aisp, '_', (sem_year-98)),
                                   month %in% c(1,7)  ~ paste0(aisp, '_', (sem_year-99)))) %>%
  # Remove original on_target variable
  mutate(on_target_deprecated = on_target) %>% 
  dplyr::select(-on_target) %>% 
  
  # Create semester sums of all crime variables adding suffix 6. This are
  # used in the orignal model
  group_by(aisp, sem_year) %>% 
  mutate_at( c(cimeVars, "target_vd", "target_sr", "target_vr"),
             .funs = list("6" = ~sum(., na.rm = T))
  ) %>%
  # Create cumulative sum of monthly target until the end of the month.
  # the original one represents what's the expected target up to the
  # start of the month, hence Jan and Jul have NAs.
  mutate(target_vd_cum2 = cumsum(target_vd),
         target_vr_cum2 = cumsum(target_vr),
         target_sr_cum2 = cumsum(target_sr)) %>% 
  ungroup() %>% 
  
  # Create variables
  mutate(
    # Adjust sester target to take into account the 10% marging they had
    # after 2013
    target_vd_sem_adjusted = ifelse(year >= 2013,
                                    target_vd_sem,
                                    target_vd_sem*1.1),
    target_sr_sem_adjusted = ifelse(year >= 2013,
                                    target_sr_sem,
                                    target_sr_sem*1.1),
    target_vr_sem_adjusted = ifelse(year >= 2013,
                                    target_vr_sem,
                                    target_vr_sem*1.1),
    
    target_vd_cum2_adjusted = ifelse(year >= 2013,
                                     target_vd_cum2,
                                     target_vd_cum2*1.1),
    target_sr_cum2_adjusted = ifelse(year >= 2013,
                                     target_sr_cum2,
                                     target_sr_cum2*1.1),
    target_vr_cum2_adjusted = ifelse(year >= 2013,
                                     target_vr_cum2,
                                     target_vr_cum2*1.1),
    
    # MAke the target more flexible as of Oct 21
    
    # target_vd_sem_adjusted = target_vd_sem_adjusted*1.1,
    # target_sr_sem_adjusted = target_sr_sem_adjusted*1.1,
    # target_vr_sem_adjusted = target_vr_sem_adjusted*1.1,
    # 
    # target_vd_cum2_adjusted = target_vd_cum2_adjusted*1.1,
    # target_sr_cum2_adjusted = target_sr_cum2_adjusted*1.1,
    # target_vr_cum2_adjusted = target_vr_cum2_adjusted*1.1,
    
    
    # Since on_target variable is if the AISP is wihtin the expeceted target 
    # for that month, regardless if it still has any perspective of being awarded
    # in that semester, create a variable (to be lagged) that means the aisp is still
    # within the semestertarget for all three crimes.
    
    # Still within the semester target for each crime. Using _cum2 variables
    # because these are the cumulative sum until the end of the month, that
    # is, for June it sums up to the end of that month. The other variables
    # _cum are just until the start of the month, for June it would only
    # account to all May crime, but no June crime.
    hit_violent_death_sem = as.integer(violent_death_sim_cum2 <= target_vd_sem_adjusted),
    hit_street_robbery_sem = as.integer(street_robbery_cum2 <= target_sr_sem_adjusted),
    hit_vehicle_robbery_sem = as.integer(vehicle_robbery_cum2 <= target_vr_sem_adjusted),
    
    # Still within the cum monthly target for each crime 
    hit_violent_death = as.integer(violent_death_sim_cum2 <= target_vd_cum2_adjusted),
    hit_street_robbery = as.integer(street_robbery_cum2  <= target_sr_cum2_adjusted),
    hit_vehicle_robbery = as.integer(vehicle_robbery_cum2  <= target_vr_cum2_adjusted),
    
    # Creating dummies for when aisp is just 5% above target,
    hit_violent_death05 = as.integer(violent_death_sim_cum2 <= target_vd_cum2_adjusted*1.05),
    hit_street_robbery05 = as.integer(street_robbery_cum2  <= target_sr_cum2_adjusted*1.05),
    hit_vehicle_robbery05 = as.integer(vehicle_robbery_cum2  <= target_vr_cum2_adjusted*1.05),
    
    # If within the month target for all 3 crimes
    hit_month = as.integer(hit_violent_death==1 & 
                             hit_street_robbery==1 & 
                             hit_vehicle_robbery==1),
    # If within the semester target for all 3 crimes
    hit_sem = as.integer(hit_violent_death_sem==1 & 
                           hit_street_robbery_sem==1 & 
                           hit_vehicle_robbery_sem==1),
    # If within 5% above semester target for all 3 crimes
    hit_05 = as.integer(hit_violent_death05==1 & 
                          hit_street_robbery05==1 & 
                          hit_vehicle_robbery05==1),
    # Last month dummy
    last_month = ifelse(month==6 | month==12,
                        1,0)
    # last_month = ifelse(month==5 | month==6 | month==11 | month==12,
    #                     1,0)
  ) %>% 
  
  # Create lagged variable
  group_by(aisp) %>%
  arrange(aisp, year, month) %>%
  mutate(hit_month_l = dplyr::lag(hit_month,
                                  n = 1L),
         hit_sem_l = dplyr::lag(hit_sem,
                                n = 1L),
         hit_05_l = dplyr::lag(hit_05,
                               n = 1L),
         # Create lag target variable based if on target on the previous 4 months
         #positive_shock = hit_month_l*hit_month_l2*hit_month_l3*hit_month_l4
         # on_target = hit_month_l
         on_target = hit_sem_l
         
  ) %>%
  
  ungroup() %>% 
  # Interaction
  mutate(last_month_on_target = last_month*hit_sem_l,
         last_month_on_target05 = last_month*hit_05_l,
         policemen_all = policemen_aisp + policemen_upp,
         policemen_all_small_d = ifelse(policemen_all < median(policemen_all, na.rm = T), 1, 0),
         last_month_size = last_month*policemen_all_small_d,
         on_target_size = hit_sem_l*policemen_all_small_d, 
         last_month_on_target_size = last_month_on_target*policemen_all_small_d) %>%
  mutate(body_bones_found = body_found + bones_found,
         all_violent_deaths = body_bones_found + violent_death_sim,
         other_homicides = involuntary_homicide + attempt_homicide)
# mutate(last_month_hit = last_month*hit_month_l)

#------------------------------------------------------------------------------#
#### Centroids ####

a_coords <- 
  st_as_sf(aisp_shp) %>%
  st_centroid(.) %>%
  bind_cols(st_coordinates(.)) %>% 
  rename(latitude = Y, longitude = X)

#------------------------------------------------------------------------------#
#### Other variables ####

#### Clean chief names
cmd <- cmd %>% 
  rename(year = ano,
         month = mes,
         cmd_name = nome_comandante_bpm)

# Remover patente
cmd$cmd_name  <- gsub("TEN CEL |CEL |MAJ", "", cmd$cmd_name )

# Add variable to sim
sim <- merge(sim, 
             cmd, 
             by = c("aisp", "year", "month"), 
             all.x = T)


#### Crime lag 

lagFun <- function(x, n){
  dplyr::lag(x,
             n = n,
             default = NA)
}

#### Laged crimes to construct placebos
sim <- 
  sim %>%
  dplyr::arrange(aisp, year,  semester) %>%
  dplyr::group_by(aisp) %>%
  dplyr::mutate(vd_l = lagFun(violent_death, 12),
                vr_l = lagFun(vehicle_robbery, 12),
                sr_l = lagFun(street_robbery, 12))

#### Add GIS variables
sim <- merge(sim, 
             a_coords, 
             by = "aisp", 
             all.x = T)



#------------------------------------------------------------------------------#
#### Construcao do Dataset por semestre ####

# Base semestral
simSem <- sim %>%
  dplyr::arrange(aisp, year,  semester) %>%
  dplyr::group_by(year, aisp, semester) %>%
  dplyr::summarise(pop = mean(population, na.rm = T),
                   vd = sum(violent_death, na.rm = T),
                   vr = sum(vehicle_robbery, na.rm = T),
                   sr = sum(street_robbery, na.rm = T),
                   tar = unique(target_vd_sem, na.rm = T))

# Indicador defasado
simSem <- 
  simSem %>%
  dplyr::arrange(aisp, year,  semester) %>%
  dplyr::group_by(aisp) %>%
  dplyr::mutate(vd_l = lagFun(vd,2),
                sr_l = lagFun(sr,2),
                vr_l = lagFun(vr,2))


#### Taxa de crime por 100mil hab. por semestre
simSem$vd_tx    <- (simSem$vd/simSem$pop)*10e4
simSem$vr_tx    <- (simSem$vr/simSem$pop)*10e4
simSem$sr_tx    <- (simSem$sr/simSem$pop)*10e4

#### Taxa no mesmo semestre do ano anterior
simSem$vd_tx_l  <- lagFun(simSem$vd_tx,2)
simSem$vr_tx_l  <- lagFun(simSem$vr_tx,2)
simSem$sr_tx_l  <- lagFun(simSem$sr_tx,2)

#------------------------------------------------------------------------------#
#### Quartis ####
# Os quartis de cada indicador sao definidos considerando a taxa do indicador 
# no mesmo semestre do ano anterior. Comecou a partir de 2011

# Funcao pra definir os quartis
colQ <- function(x){
  # Calcular os valores
  qx <- quantile(x, na.rm = T)
  
  # Definir os quartos
  newVec <- NA
  newVec[x < qx[2]] <- 1
  newVec[x >= qx[2] & x < qx[3]] <- 2
  newVec[x >=  qx[3] & x < qx[4]] <- 3
  newVec[x > qx[4]] <- 4
  
  return(newVec)
}


#### Adicionar quartis pra base sestral
simSem <- 
  simSem %>% 
  dplyr::group_by(year) %>% # Qartil daquele ano
  mutate(qua_vd = colQ(vd_tx_l),
         qua_vr = colQ(vr_tx_l),
         qua_sr = colQ(sr_tx_l))


#### Merge quartis na base mensal

idVars <- c("aisp",
            "year",
            "semester")

sim <- merge(sim, 
             simSem[,c("aisp",
                       "year",
                       "semester",
                       "qua_vd",
                       "qua_vr",
                       "qua_sr",
                       "vd_tx_l",
                       "vr_tx_l",
                       "sr_tx_l")],
             by = idVars)

#------------------------------------------------------------------------------#
#### Meta placebo ####


#### Criterios de reducao por quartil
# reduQ1_vd = 0
# reduQ2_vd = 0.04
# reduQ3_vd = 0.055
# reduQ4_vd = 0.075
# 
# reduQ1_vr = 0
# reduQ2_vr = 0.03
# reduQ3_vr = 0.05
# reduQ4_vr = 0.065
# 
# reduQ1_sr = 0
# reduQ2_sr = 0.03
# reduQ3_sr = 0.05
# reduQ4_sr = 0.07


for (x in c('reduQ1_vd',
            'reduQ2_vd',
            'reduQ3_vd',
            'reduQ4_vd',
            
            'reduQ1_vr',
            'reduQ2_vr',
            'reduQ3_vr',
            'reduQ4_vr',
            
            'reduQ1_sr',
            'reduQ2_sr',
            'reduQ3_sr',
            'reduQ4_sr')){
  assign(x, 0.09)
}

#### Criar a booleano de quartil
Qbol<- function(q, var){
  return(sim[,paste0("qua_", var)] == q & !is.na(sim[,paste0("qua_", var)]))
}


#### Placebo targets

# Define empty variable
sim$plaTar_vd <- NA
sim$plaTar_vr <- NA
sim$plaTar_sr <- NA


sim$plaTar_vd[Qbol(1, "vd")] <- round(sim$vd_l[Qbol(1, "vd")]*(1-reduQ1_vd))
sim$plaTar_vd[Qbol(2, "vd")] <- round(sim$vd_l[Qbol(2, "vd")]*(1-reduQ2_vd))
sim$plaTar_vd[Qbol(3, "vd")] <- round(sim$vd_l[Qbol(3, "vd")]*(1-reduQ3_vd))
sim$plaTar_vd[Qbol(4, "vd")] <- round(sim$vd_l[Qbol(4, "vd")]*(1-reduQ4_vd))

sim$plaTar_vr[Qbol(1, "vr")] <- round(sim$vr_l[Qbol(1, "vr")]*(1-reduQ1_vr))
sim$plaTar_vr[Qbol(2, "vr")] <- round(sim$vr_l[Qbol(2, "vr")]*(1-reduQ2_vr))
sim$plaTar_vr[Qbol(3, "vr")] <- round(sim$vr_l[Qbol(3, "vr")]*(1-reduQ3_vr))
sim$plaTar_vr[Qbol(4, "vr")] <- round(sim$vr_l[Qbol(4, "vr")]*(1-reduQ4_vr))

sim$plaTar_sr[Qbol(1, "sr")] <- round(sim$sr_l[Qbol(1, "sr")]*(1-reduQ1_sr))
sim$plaTar_sr[Qbol(2, "sr")] <- round(sim$sr_l[Qbol(2, "sr")]*(1-reduQ2_sr))
sim$plaTar_sr[Qbol(3, "sr")] <- round(sim$sr_l[Qbol(3, "sr")]*(1-reduQ3_sr))
sim$plaTar_sr[Qbol(4, "sr")] <- round(sim$sr_l[Qbol(4, "sr")]*(1-reduQ4_sr))

# Semester target for placebo
sim <- sim %>% 
  group_by(aisp, year, semester) %>% 
  mutate(plaTar_vd_sem = sum(plaTar_vd),
         plaTar_vr_sem = sum(plaTar_vr),
         plaTar_sr_sem = sum(plaTar_sr))


#------------------------------------------------------------------------------#
#### Regression variables placebo ####

#placebo_PRE_bol <- sim$year <2009 | (sim$year == 2009 & sim$semester == 1)


#### Create cummulative targets both to the start of the month and end of
# the month. The original one represents what's the expected target up to the
#   # start of the month, hence Jan and Jul have NAs.


sim <- sim %>%
  group_by(aisp, year, semester) %>%
  mutate(plaTar_vd_l = Lag(plaTar_vd, +1),
         plaTar_vd_cum = cumsum(replace_na(plaTar_vd_l,0)),
         plaTar_vd_cum2 = cumsum(replace_na(plaTar_vd,0)),
         plaTar_vr_l = Lag(plaTar_vr, +1),
         plaTar_vr_cum = cumsum(replace_na(plaTar_vr_l,0)),
         plaTar_vr_cum2 = cumsum(replace_na(plaTar_vr,0)),
         plaTar_sr_l = Lag(plaTar_sr, +1),
         plaTar_sr_cum = cumsum(replace_na(plaTar_sr_l,0)),
         plaTar_sr_cum2 = cumsum(replace_na(plaTar_sr,0))) %>% 
  dplyr::select(-c(plaTar_vd_l, plaTar_vr_l, plaTar_sr_l)) # remove lagged variables

# Change zeros to NA if year is 2004
bol04 <- sim$year ==2004

sim$plaTar_vd_cum[bol04] <- NA
sim$plaTar_vr_cum[bol04] <- NA
sim$plaTar_sr_cum[bol04] <- NA


# Fix this so dist is not divided by zero
sim$plaTar_vr_sem <- ifelse(sim$plaTar_vr_sem == 0,
                            NA,
                            sim$plaTar_vr_sem)


# Create target per crime
# First month of semester on target is always NA
sim$on_target_vd_plapre <- 
  (sim$violent_death_sim_cum <=  sim$plaTar_vd_cum) %>% as.numeric()
sim$on_target_vr_plapre <- 
  (sim$vehicle_robbery_cum <=  sim$plaTar_vr_cum) %>% as.numeric()
sim$on_target_sr_plapre <- 
  (sim$street_robbery_cum <=  sim$plaTar_sr_cum) %>% as.numeric()

# Create overall target
sim$on_target_plapre <-  (sim$on_target_vd_plapre==1 &
                            sim$on_target_vr_plapre==1 & 
                            sim$on_target_sr_plapre==1) %>% as.numeric()


#------------------------------------------------------------------------------#
#### End of semester placebo ####

# Create regression variables
sim %<>%
  
  
  # Create variables
  mutate(
    # Still within the semester target for each crime. Using _cum2 variables
    # because these are the cumulative sum until the end of the month, that
    # is, for June it sums up to the end of that month. The other variables
    # _cum are just until the start of the month, for June it would only
    # account to all May crime, but no June crime.
    hit_violent_death_pla_sem = as.integer(violent_death_sim_cum2 <= plaTar_vd_sem),
    hit_street_robbery_pla_sem = as.integer(street_robbery_cum2 <= plaTar_sr_sem),
    hit_vehicle_robbery_pla_sem = as.integer(vehicle_robbery_cum2 <= plaTar_vr_sem),
    
    
    # Within monthly target
    hit_violent_death_pla = as.integer(violent_death_sim_cum2 <= plaTar_vd_cum2),
    hit_street_robbery_pla = as.integer(street_robbery_cum2 <= plaTar_sr_cum2),
    hit_vehicle_robbery_pla = as.integer(vehicle_robbery_cum2 <= plaTar_vr_cum2),
    
    
    # If within the month target for all 3 crimes
    hit_month_pla = as.integer(hit_violent_death_pla==1 &
                                 hit_street_robbery_pla==1 &
                                 hit_vehicle_robbery_pla==1),
    
    
    # If within the semester target for all 3 crimes
    hit_sem_pla = as.integer(hit_violent_death_pla_sem==1 &
                               hit_street_robbery_pla_sem==1 &
                               hit_vehicle_robbery_pla_sem==1)
    
    
  ) %>%
  
  # Create lagged variable
  group_by(aisp) %>%
  arrange(aisp, year, month) %>%
  mutate(hit_month_pla_l = dplyr::lag(hit_month_pla,
                                      n = 1L),
         hit_sem_pla_l = dplyr::lag(hit_sem_pla,
                                    n = 1L),
         on_target_plapre = hit_sem_pla_l
  ) %>%
  
  ungroup() %>%
  # Interaction
  mutate(last_month_on_target_plapre = last_month*hit_sem_pla_l,
         hit_sem_pla_fm_l  = hit_sem_pla_l)
# mutate(last_month_hit = last_month*hit_month_l)

#------------------------------------------------------------------------------#
#### Plots do fit  ####

simp <- sim[sim$year>2010,]


if (EXPORT_plots){
  # Letalidade Violenta
  png(file.path(DATA, "lv_placebo_fit.png"),
      width = 730, height = 480)
  
  slope_vd <- 1 + (reduQ1_vd + reduQ2_vd + reduQ3_vd + reduQ4_vd)/4
  plot(simp$target_vd, 
       simp$vd_l,
       xlab="Meta real", 
       ylab="L.V. mesmo mes do ano anterior")
  title(main="Fit medio percentuais de reducao  - Letalidade Violenta (2011-2015)")
  abline(a = 0, b= slope_vd, col = "red")
  
  dev.off()
  
  # Roubo de veiculos
  png(file.path(DATA, "rv_placebo_fit.png"),
      width = 730, height = 480)
  slope_vr <- 1 + (reduQ1_vr + reduQ2_vr + reduQ3_vr + reduQ4_vr)/4
  plot(simp$target_vr, simp$vr_l,
       xlab="Meta real", 
       ylab="R.V. mesmo mes do ano anterior")
  title(main="Fit medio percentuais de reducao  - Roubo de Veiculos (2011-2015)")
  abline(a = 0, b= slope_vr, col = "red")
  dev.off()
  
  # Roubo de rua
  png(file.path(DATA, "rv_placebo_fit.png"),
      width = 730, height = 480)
  slope_sr <- 1 + (reduQ1_sr + reduQ2_sr + reduQ3_sr + reduQ4_sr)/4
  plot(simp$target_sr, simp$sr_l,
       xlab="Meta real", 
       ylab="R.R. mesmo mes do ano anterior")
  title(main="Fit medio percentuais de reducao  - Roubo de Rua (2011-2015)")
  abline(a = 0, b= slope_sr, col = "red")
  dev.off()
  
}

#------------------------------------------------------------------------------#
#### Exportart a base ####

#### Cosmetic changes
sim <-
  sim %>%
  rename("vd_placebo_tar" = "plaTar_vd",
         "vr_placebo_tar" = "plaTar_vr",
         "sr_placebo_tar" = "plaTar_sr") %>%
  mutate(month2 = hit_sem_l*(as.numeric(month %in% c(2,8))),
         month3 = hit_sem_l*(as.numeric(month %in% c(3,9))),
         month4 = hit_sem_l*(as.numeric(month %in% c(4,10))),
         month5 = hit_sem_l*(as.numeric(month %in% c(5,11))),
         month6 = hit_sem_l*(as.numeric(month %in% c(6,12)))) %>%
  mutate(month2_05 = hit_05_l*(as.numeric(month %in% c(2,8))),
         month3_05 = hit_05_l*(as.numeric(month %in% c(3,9))),
         month4_05 = hit_05_l*(as.numeric(month %in% c(4,10))),
         month5_05 = hit_05_l*(as.numeric(month %in% c(5,11))),
         month6_05 = hit_05_l*(as.numeric(month %in% c(6,12)))) %>%
  mutate(across(contains('police_killing'), ~case_when(year >= 2011 ~ .,
                                                       TRUE ~ NA_real_))) %>%
  mutate(hit_sem_fm_l = hit_sem_l,
         hit_05_fm_l = hit_05_l,
         hit_sem_l_chaise = 1-hit_sem_l) %>%
  mutate(across(c('violent_death_sim',
                  'vehicle_robbery',
                  'street_robbery',
                  'homicide',
                  'police_killing',
                  'body_bones_found',
                  'attempt_homicide',
                  'other_robberies',
                  'vehicle_theft',
                  'street_theft'), ~log(.x+1), .names = "{.col}_log"))


if (EXPORT_data){
  
  # Exporting the entire data to run in R
  write.csv2(sim,
             file.path(DATA, "data_SIM_2019_constructed.csv"),
             row.names = F,
             na = "")
  
}


#------------------------------------------------------------------------------#
#### Include extra crimes not originally in the data ####

# This code processes raw data on crimes not originally included in
# our data. It also merges these into the data.
# NEEDS TO BE ORGANIZED AND INCORPORATED INTO 01-construction.R 

# Load constructed data
# org_data <- fread(file = file.path(DATA, "data_SIM_2019_constructed.csv"),
#                   encoding = "UTF-8")

org_data <- sim

# Load raw data
other_crimes <- read.csv(
  file.path(DATA, 'BaseDPEvolucaoMensalCisp.csv'),
  sep = ';', 
  header = T)

# BASE DO SITE DO ISP NÃO EXISTE MAIS (?)
# hom_fla <- read.csv(
#   file.path(DATA, 'BaseDPLetalidadeFlagrante.csv'),
#   sep = ';', 
#   header = T)

#------------------------------------------------------------------------------#
#### Process flagrante #### 

# Reshape to cisp level
# names(hom_fla) <- c( "ano_mes",
#                      "crime",
#                      "cisp",
#                      "total",
#                      "sem_flagrante",
#                      "com_flagrante")
# hom_simp <- hom_fla %>% 
#   mutate(total = as.character(total),
#          sem_flagrante = as.character(sem_flagrante),
#          com_flagrante =  as.character(com_flagrante)) %>% 
#   pivot_longer(cols = !c(ano_mes, cisp, crime),
#                names_to = 'flagr',
#                values_to = 'count') %>% 
#   # Recode
#   mutate(crime = recode(crime, 
#                         "Homicídio doloso" = "hom_doloso",
#                         "Morte por intervenção de agente do Estado" = "hom_por_interv_policial",
#                         "Lesão corporal seguida de morte" = "lesao_corp_mort",
#                         "Roubo seguido de morte - vítimas" = 'latrocinio') ) %>% 
#   # Combine columns to reshape
#   mutate(crime = paste(crime, flagr, sep = "_")) %>% 
#   dplyr::select(-flagr) %>% 
#   
#   # Reshape wide
#   pivot_wider(id_cols = c(ano_mes, cisp),
#               names_from = crime,
#               values_from = count) %>% 
#   # Split time vars
#   separate('ano_mes', into = c('year', 'month'), sep = '-') %>% 
#   
#   # Keep only crimes we want 
#   dplyr::select('year', 'month', 'cisp', 
#                 "hom_doloso_com_flagrante", 
#                 "lesao_corp_mort_total",
#                 "hom_por_interv_policial_total")  %>% 
#   # Rename vars
#   rename("violent_death_fla" = "hom_doloso_com_flagrante",
#          "assaut_death" = "lesao_corp_mort_total",
#          "police_killing_tot" = "hom_por_interv_policial_total")

#------------------------------------------------------------------------------#
#### Proces other crimes ####

# Process other crimes
oth_simp <- other_crimes %>% 
  rename('month' = 'mes',
         'year'= 'ano',
         'fraud'= 'estelionato') %>% 
  dplyr::select(aisp, 
                cisp, 
                month, 
                year, 
                fraud)

#------------------------------------------------------------------------------#
#### Merge ####

# Merge together since hom_fla is in the CISP level and original data
# doesn't contain that var anymore

final <- oth_simp %>% 
  mutate(year = as.character(year),
         month = as.integer(month)) %>% 
  # full_join(hom_simp %>% 
  #             mutate(month = as.integer(month)), 
  #           by = c('year', 'month', 'cisp')) %>% 
  # Drop cisps that don't match
  subset(!is.na(aisp))

# Aggregate at aisp level
final_agg <- final %>% dplyr::group_by(aisp, year, month) %>%
  summarise(fraud = sum(fraud %>% as.integer()),
            # violent_death_fla = sum(violent_death_fla %>% as.integer()),
            # assaut_death = sum(assaut_death %>% as.integer()),
            # police_killing_tot = sum(police_killing_tot %>% as.integer())
  ) %>%
  ungroup() %>%
  # Just make sure types are compatible
  mutate(year = as.integer(year))

#------------------------------------------------------------------------------#
#### Merge to paper data ####

export_data <-org_data %>% 
  left_join(final_agg %>%
              mutate(year = as.numeric(year)), by = c("aisp", "year", "month"))


# Construct dummy variables
dummy_fun <- function(x){
  return(ifelse(x > 0, 1, 0))
}

export_data$dfraud <- dummy_fun(export_data$fraud)
# export_data$dviolent_death_fla <- dummy_fun(export_data$violent_death_fla)
# export_data$dassaut_death <- dummy_fun(export_data$assaut_death)
# export_data$dpolice_killing_tot <- dummy_fun(export_data$police_killing_tot)

# Export
if (EXPORT_data){
  export_data %>% write.csv2(
    file = file.path(DATA, 
                     "data_SIM_2019_constructed_extra.csv"),
    row.names = F,
    na = "")
}
