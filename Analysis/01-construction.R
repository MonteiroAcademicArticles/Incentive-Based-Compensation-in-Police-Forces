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
#################################################################################################################
# ORGANIZE PCERJ INFORMATION
#################################################################################################################

sim2 <- read.delim(file.path(DATA,"police/Base_DP_consolidado.csv"), sep = ';') %>%
  janitor::clean_names()

# Includes population information
pop <- read.delim(file.path(DATA,"population/painel_dp_populacao_MES.csv"), sep = ';') %>%
  janitor::clean_names() %>%
  mutate(pop_cisp = as.numeric(str_replace(pop, ',','.'))) %>%
  select(-pop)

sim2 <- sim2 %>%
  left_join(pop,
            by=c('cisp','ano','mes'))

# Create street theft variables
sim2 <- sim2 %>%
  mutate(furto_rua = furto_transeunte + furto_coletivo + furto_celular,
         n_precinct=1) %>%
  rename(id_precinct = cisp,
         year = ano,
         month = mes)

# Includes precint area information
sim2 <- sim2 %>%
  left_join(read.dta13(file.path(DATA, 'geo/precinct_area.dta')),
            by = 'id_precinct')


# Collapse database in aisp, year, month strucuture
sim2 <- sim2 %>%
  group_by(aisp, month, year) %>%
  summarise(across(c('n_precinct', 'letalidade_violenta', 'hom_doloso', 'hom_culposo',
                     'tentat_hom', 'encontro_cadaver', 'encontro_ossada',
                     'apreensao_drogas', 'apf', 'aaapai', 'hom_por_interv_policial',
                     'total_roubos', 'total_furtos', 'roubo_comercio', 'roubo_residencia',
                     'roubo_rua', 'roubo_veiculo', 'furto_veiculos', 'furto_rua',
                     'roubo_carga', 'roubo_cx_eletronico', 'roubo_conducao_saque',
                     'pessoas_desaparecidas', 'pop_cisp', 'area_precinct'), sum),
            across(c('mcirc', 'risp'), max)) %>%
  dplyr::ungroup()

# Including targets in sim
sim2 <- sim2 %>%
  left_join(read.dta13(file.path(DATA, 'targets/meta_lv.dta')),
            by = c('aisp',
                   'year' = 'ano',
                   'month' = 'mes')) %>%
  left_join(read.dta13(file.path(DATA, 'targets/meta_rr.dta')),
            by = c('aisp',
                   'year' = 'ano',
                   'month' = 'mes')) %>%
  left_join(read.dta13(file.path(DATA, 'targets/meta_rv.dta')),
            by = c('aisp',
                   'year' = 'ano',
                   'month' = 'mes'))

# Add number of policemen. sim2 starts in 2008 and ends in 2015/6
sim2 <- sim2 %>%
  mutate(mes_ano = paste(year, month, "01", sep = "-"),
         mes_ano = as.Date(mes_ano)) %>%
  left_join(read.dta13(file.path(DATA, 'police/efetivo_aisp_upp.dta')),
            by = c('aisp','mes_ano'))

# Add information on commanders
sim2 <- sim2 %>%
  mutate(mes_ano = paste(year, month, "01", sep = "-"),
         mes_ano = as.Date(mes_ano)) %>%
  left_join(read.dta13(file.path(DATA, 'police/efetivo_aisp.dta')),
            by = c('aisp','mes_ano')) %>%
  select(-starts_with('d_clus_det'),
         -starts_with('exper_'),
         -'troca',
         -'exper_cmt_total',
         -'cluster_det')

# Creating and renaming variables
sim2 <- sim2 %>%
  mutate(semestre = case_when(month>=1 & month<=6 ~ 1,
                              month>=7 & month<=12 ~ 2),
         max_prize = case_when(year==2009 & semestre==2 | year==2010 & semestre==1 ~ 1.5,
                               year==2010 & semestre==2 ~ 3,
                               year==2011 & semestre==1 ~ 6,
                               year==2011 & semestre==2 | year==2012 ~ 9,
                               year>=2013 ~ 13.5),
         min_prize = case_when(year==2009 & semestre==2 | year==2010 & semestre==1 ~ .5,
                               year==2010 & semestre==2 ~ 1,
                               year==2011 & semestre==1 ~ 2,
                               year==2011 & semestre==2 | year==2012 ~ 3,
                               year>=2013 ~ 1.5 )) %>%
  rename(violent_death = letalidade_violenta,
         homicide = hom_doloso,
         involuntary_homicide = hom_culposo,
         attempt_homicide = tentat_hom,
         police_killing = hom_por_interv_policial,
         robbery = total_roubos,
         theft = total_furtos,
         street_robbery = roubo_rua,
         vehicle_robbery = roubo_veiculo,
         street_theft = furto_rua,
         id_municipality = mcirc,
         population = pop_cisp,
         cargo_robbery = roubo_carga,
         vehicle_theft = furto_veiculos,
         target_vd = meta_lv,
         target_sr = meta_rr,
         target_vr = meta_rv,
         semester = semestre,
         body_found = encontro_cadaver,
         bones_found = encontro_ossada,
         area_aisp = area_precinct,
         policemen_aisp = efetivo_bpm,
         policemen_upp = efetivo_upp,
         month_year = mes_ano) %>%
  mutate(violent_death_sim = case_when(year<=2010 ~ homicide,
                                       year>2010 ~ violent_death),
         dpolice_killing = as.numeric(police_killing>0)) %>%
  mutate(other_robberies= robbery - street_robbery,
         sem_year = interval(mdy(01011960), month_year) %/% months(6)) %>%
  filter(year>2003 & year<2019,
         !is.na(aisp),
         !(aisp==1 | aisp==13)) %>% #There are no information on targets for AISP 1 and AISP 13
  arrange(month,year,semester,month_year,sem_year, aisp)

#################################################################################################################
# CREATE TREATMENT VARIABLES 
#################################################################################################################
sim2 <- sim2 %>%
  arrange(aisp, month_year) %>%
  group_by(aisp, sem_year) %>%
  mutate(across(c('violent_death_sim', 'target_vd',
                  'street_robbery', 'target_sr',
                  'vehicle_robbery', 'target_vr'), ~cumsum(.x), .names = "{.col}_cum")) %>%
  mutate(across(c('violent_death_sim_cum', 'target_vd_cum',
                  'street_robbery_cum', 'target_sr_cum',
                  'vehicle_robbery_cum', 'target_vr_cum'), ~dplyr::lag(.x))) %>%
  mutate(across(c('violent_death_sim', 'target_vd',
                  'street_robbery', 'target_sr',
                  'vehicle_robbery', 'target_vr'), ~cumsum(.x), .names = "{.col}_cum2")) %>%
  mutate(across(c('target_vd',
                  'target_sr',
                  'target_vr'), ~sum(.x), .names = "{.col}_sem")) %>%
  ungroup() %>%
  select(aisp, year, month, semester, month_year, sem_year, everything()) %>%
  arrange(aisp, year, month, semester, month_year, sem_year)


rm(list = c('pop','RjProj_aze', 'RjProj_unp'))
# Loading data into a new object to be processed
sim <- sim2
sim <- sim[!is.na(sim$aisp) & !is.na(sim$year) & !is.na(sim$month), ]

# Create this useful numeric version of this variable
sim$year_month <- sim$year*100+ sim$month

# Load shapefiles
aisp_shp <- readOGR(dsn = GIS, layer = "lm_aisp_072024")
aisp_shp <- st_make_valid(st_as_sf(aisp_shp))

aisp_shp <- st_as_sf(aisp_shp) %>%
  group_by(aisp) %>%
  summarise(across(c('shape_Leng','shape_Area','AREA_GEO') , sum))

# Load chief IDs
cmd <- fread(file = file.path(DATA, "police/ComandantesBatalhao.csv"),
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
              "other_robberies")



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
    
    # Creating dummies for when aisp is just 5% above target,
    hit_violent_death05 = as.integer(violent_death_sim_cum2 <= target_vd_cum2_adjusted*1.05),
    hit_street_robbery05 = as.integer(street_robbery_cum2  <= target_sr_cum2_adjusted*1.05),
    hit_vehicle_robbery05 = as.integer(vehicle_robbery_cum2  <= target_vr_cum2_adjusted*1.05),
    
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
  ) %>% 
  
  # Create lagged variable
  group_by(aisp) %>%
  arrange(aisp, year, month) %>%
  mutate(hit_sem_l = dplyr::lag(hit_sem,
                                n = 1L),
         hit_05_l = dplyr::lag(hit_05,
                               n = 1L),
         # Create lag target variable based if on target on the previous 4 months
         on_target = hit_sem_l
         
  ) %>%
  
  ungroup() %>% 
  # Interaction
  mutate(last_month_on_target = last_month*hit_sem_l,
         last_month_on_target05 = last_month*hit_05_l,
         policemen_all = policemen_aisp + policemen_upp) %>%
  mutate(body_bones_found = body_found + bones_found,
         all_violent_deaths = body_bones_found + violent_death_sim,
         other_homicides = involuntary_homicide + attempt_homicide)

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
#### Semester dataset building ####

# Semester dataset
simSem <- sim %>%
  dplyr::arrange(aisp, year,  semester) %>%
  dplyr::group_by(year, aisp, semester) %>%
  dplyr::summarise(pop = mean(population, na.rm = T),
                   vd = sum(violent_death, na.rm = T),
                   vr = sum(vehicle_robbery, na.rm = T),
                   sr = sum(street_robbery, na.rm = T),
                   tar = unique(target_vd_sem, na.rm = T))

# Lagged indicator
simSem <- 
  simSem %>%
  dplyr::arrange(aisp, year,  semester) %>%
  dplyr::group_by(aisp) %>%
  dplyr::mutate(vd_l = lagFun(vd,2),
                sr_l = lagFun(sr,2),
                vr_l = lagFun(vr,2))


#### Crimes rates
simSem$vd_tx    <- (simSem$vd/simSem$pop)*10e4
simSem$vr_tx    <- (simSem$vr/simSem$pop)*10e4
simSem$sr_tx    <- (simSem$sr/simSem$pop)*10e4

#### Taxa no mesmo semestre do ano anterior
simSem$vd_tx_l  <- lagFun(simSem$vd_tx,2)
simSem$vr_tx_l  <- lagFun(simSem$vr_tx,2)
simSem$sr_tx_l  <- lagFun(simSem$sr_tx,2)

#------------------------------------------------------------------------------#
#### Quartiles ####

# Function for quartiles
colQ <- function(x){
  qx <- quantile(x, na.rm = T)
  
  newVec <- NA
  newVec[x < qx[2]] <- 1
  newVec[x >= qx[2] & x < qx[3]] <- 2
  newVec[x >=  qx[3] & x < qx[4]] <- 3
  newVec[x > qx[4]] <- 4
  
  return(newVec)
}


#### Adding to dataset
simSem <- 
  simSem %>% 
  dplyr::group_by(year) %>%
  mutate(qua_vd = colQ(vd_tx_l),
         qua_vr = colQ(vr_tx_l),
         qua_sr = colQ(sr_tx_l))


#### Merging

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
#### Placebo target ####


#### Quartile reduction criteria
reduQ1_vd = 0
reduQ2_vd = 0.04
reduQ3_vd = 0.055
reduQ4_vd = 0.075

reduQ1_vr = 0
reduQ2_vr = 0.03
reduQ3_vr = 0.05
reduQ4_vr = 0.065

reduQ1_sr = 0
reduQ2_sr = 0.03
reduQ3_sr = 0.05
reduQ4_sr = 0.07

#### Boolean
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

#### Create cummulative targets both to the start of the month and end of
# the month. The original one represents what's the expected target up to the
# start of the month, hence Jan and Jul have NAs.


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

#------------------------------------------------------------------------------#
#### Plots ####

simp <- sim[sim$year>2010,]


if (EXPORT_plots){
  # Violent deaths
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
  
  # Vehicle robbery
  png(file.path(DATA, "rv_placebo_fit.png"),
      width = 730, height = 480)
  slope_vr <- 1 + (reduQ1_vr + reduQ2_vr + reduQ3_vr + reduQ4_vr)/4
  plot(simp$target_vr, simp$vr_l,
       xlab="Meta real", 
       ylab="R.V. mesmo mes do ano anterior")
  title(main="Fit medio percentuais de reducao  - Roubo de Veiculos (2011-2015)")
  abline(a = 0, b= slope_vr, col = "red")
  dev.off()
  
  # Street ribbery
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
#### Export dataset ####

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


#------------------------------------------------------------------------------#
#### Merge to paper data ####

# Export
if (EXPORT_data){
  sim %>% write.csv2(
    file = file.path(DATA, 
                     "data_SIM_2019_constructed_extra.csv"),
    row.names = F,
    na = "")
}
