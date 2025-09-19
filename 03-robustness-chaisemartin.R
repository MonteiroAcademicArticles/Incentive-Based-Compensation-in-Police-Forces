library(DIDmultiplegtDYN)
library(openxlsx)

#--------------------------------------------------------------------#
# 1) Carregar a amostra base
#--------------------------------------------------------------------#
sd <- feRegSim('violent_death_sim') %>% regData(regdf = sr)

#--------------------------------------------------------------------#
# 2) Criar índice temporal e garantir que aisp é fator
#--------------------------------------------------------------------#
sd <- sd %>%
  dplyr::arrange(year, month) %>%
  dplyr::mutate(
    time_id = data.table::frank(year*100 + month, ties.method = "dense"),
    aisp = as.factor(aisp)
  )

#--------------------------------------------------------------------#
# 3) Balancear o painel (todos grupos x tempos)
#--------------------------------------------------------------------#
# 1) Garante que cada grupo tem todos os tempos
panel_full <- expand.grid(
  aisp = unique(sd$aisp),
  time_id = unique(sd$time_id)
)

sd_bal <- panel_full %>%
  left_join(sd, by = c("aisp","time_id")) %>%
  arrange(aisp, time_id)

sd_bal <- sd_bal %>%
  mutate(
    aisp = as.character(aisp)  # ou as.integer(aisp), se preferir numérico
  )

# 2) Substituir NAs de outcome e tratamento
sd_bal <- sd_bal %>%
  mutate(
    violent_death_sim_log = ifelse(is.na(violent_death_sim_log), 0, violent_death_sim_log),
    vehicle_robbery_log   = ifelse(is.na(vehicle_robbery_log), 0, vehicle_robbery_log),
    street_robbery_log    = ifelse(is.na(street_robbery_log), 0, street_robbery_log),
    hit_sem_l_chaise      = ifelse(is.na(hit_sem_l_chaise), 0, hit_sem_l_chaise),
    n_precinct            = ifelse(is.na(n_precinct), 0, n_precinct),
    population            = ifelse(is.na(population), 0, population),
    max_prize             = ifelse(is.na(max_prize), 0, max_prize)
  )


#--------------------------------------------------------------------#
# 4) Agora sim rodar o Chaisemartin
#--------------------------------------------------------------------#
tab1_chaise_fit <- lapply(
  list('violent_death_sim_log','vehicle_robbery_log','street_robbery_log'),
  function(Vars){
    did_multiplegt_dyn(
      df        = sd_bal,
      outcome   = Vars,
      group     = 'aisp',
      time      = 'time_id',
      treatment = 'hit_sem_l_chaise',
      cluster   = 'aisp',
      effects   = 2,
      placebo   = 2,
      controls  = c('n_precinct','population','max_prize'),
      save_sample = TRUE
    )
  }
)

# Chaisemartin --------------------------------------
tab1_chaise_fit <- lapply(list('violent_death_sim_log',
                               'vehicle_robbery_log',
                               'street_robbery_log'), function(Vars){
                                 fit <- did_multiplegt_dyn(df = sd,
                                                           outcome = Vars,
                                                           group = 'aisp',
                                                           time = 'month_year',
                                                           treatment = 'hit_sem_l_chaise',
                                                           cluster = clusterVars,
                                                           effects = 2,
                                                           placebo = 2,
                                                           controls = c('n_precinct','population','max_prize'),
                                                           save_sample = T)
                                 
                                 return(fit)
                               })

names(tab1_chaise_fit) <- c('violent_death_sim_log',
                            'vehicle_robbery_log',
                            'street_robbery_log')

tab1_chaise <- lapply(list('violent_death_sim_log',
                           'vehicle_robbery_log',
                           'street_robbery_log'), function(Vars){
                             
                             fit1 <- tab1_chaise_fit[[Vars]]
                             
                             esp1 <- tibble(sample = 'Chaisemartin',
                                            dynamic = 'Yes',
                                            chief_fe = 'No',
                                            coefs_avg_total_eff = fit1$results$ATE[1,'Estimate'],
                                            se_avg_total_eff = fit1$results$ATE[1,'SE'],
                                            n_avg_total_eff = fit1$results$ATE[1,'N'],
                                            switchers_avg_total_eff = fit1$results$ATE[1,'Switchers'],
                                            avg_times_accum = fit1$results$delta_D_avg_total,
                                            med = round(mean(sd[,Vars]),3),
                                            var = Vars)
                             
                             for (i in 1:2){
                               esp1 <- esp1 %>%
                                 bind_cols(
                                   tibble(!!sym(paste0('coefs_att_ef',i)) := fit1$results$Effects[i,'Estimate'],
                                          !!sym(paste0('se_att_ef',i)) := fit1$results$Effects[i,'SE'],
                                          !!sym(paste0('n_att_ef',i)) := fit1$results$Effects[i,'N'],
                                          !!sym(paste0('switchers_att_ef',i)) := fit1$results$Effects[i,'Switchers'])
                                 )
                               
                             }
                             return(esp1)
                           }) %>%
  Reduce(f=bind_rows)

# Exporting
seFormatFun_chaise <- function(model, variable = NULL){
  
    se <- abs(model[model$var == variable,'se_avg_total_eff'])
    coef <- model[model$var == variable,'coefs_avg_total_eff']
    
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
  cbind(seFormatFun_chaise(model = tab1_chaise, variable = 'violent_death_sim_log'),
        seFormatFun_chaise(model = tab1_chaise, variable = 'vehicle_robbery_log'),
        seFormatFun_chaise(model = tab1_chaise, variable = 'street_robbery_log'))


# Y mean
Ymean_vec <- c(mean(sd$violent_death_sim),
               mean(sd$vehicle_robbery),
               mean(sd$street_robbery))

n_aisp <- c(39,39,39)

slRegTable <- 
  rbind(c("","Panel A: de Chaisemartin, C, D'Haultfoeuille, X",""),
        c("","Number of occurrences",""),
        c("Violent  deaths",
          "Vehicle  robbery  (Carjacking)", 
          "Street  robbery"),
        c("(1)", "(2)", "(3)"),
        coefs,
        c('Yes','Yes','Yes'),
        Ymean_vec %>% round(2),
        n_aisp)

rows <- c("",
          " ",
          "  ",
          "   ",
          "Eligible",
          "    ",
          "Month FE",
          "Y mean",
          "Number of aisp")

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
   set_bottom_border(1, 2:4) %>%
   set_bottom_border(7, everywhere) %>%
   set_bottom_border(9, everywhere))

#------------------------------------------------------------------------------#
##### Actually exporting ####


if(EXPORT_tables){
  huxtable::quick_xlsx(slRegTable_hux, file = file.path(OUTPUTS_final, "chaisemartin_unformated.xlsx"))
  
}

