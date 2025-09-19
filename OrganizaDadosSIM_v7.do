
cd "C:\Users\mathi\OneDrive\Documents\GitHub\SIM"

*****************************************************************************************************************
* ORGANIZE PCERJ INFORMATION
*****************************************************************************************************************
use "lm_aisp_072024.dta", clear
rename aisp id_precinct
rename AREA_GEO area_precinct
duplicates drop id_precinct, force
save "lm_aisp_072024_attr.dta", replace

save "C:\Users\mathi\OneDrive\Documents\GitHub\SIM\GIS\lm_aisp_072024_attr.dta", replace

insheet using "data\PCERJ\Base_DP_consolidado.csv", delimiter(";") clear
destring _all, replace

*Add population figures
preserve
import delimited "data\ArquivosAuxiliares\painel_dp_populacao_MES.csv", delimiter(";") clear
destring pop, dpcomma gen (pop_cisp)
drop pop
save data\ArquivosAuxiliares\painel_dp_populacao_MES.dta, replace
restore

merge 1:1 cisp ano mes using data\ArquivosAuxiliares\painel_dp_populacao_MES.dta
drop if _merge 	==2
drop _merge

gen furto_rua=  furto_transeunte + furto_coletivo + furto_celular

gen n_precinct=1

rename cisp id_precinct
rename ano year
rename mes month


*Add precinct area
merge m:1 id_precinct using "lm_aisp_072024_attr.dta"
drop _merge

sort id_precinct

merge 1:1 id_precinct year month using data\PCERJ\gun_seizure.dta
drop _merge



* Collapse data from precinct level do AISP level
destring apf, replace
destring aaapai, replace
destring roubo_cx_eletronico, replace

collapse (sum) n_precinct letalidade hom_doloso hom_culposo tentat_hom encontro_cadaver encontro_ossada apreensao_drogas total_gun rifle pistol machine_gun apf aaapai hom_por_interv_policial total_roubos ///
 total_furtos roubo_comercio roubo_residencia roubo_rua roubo_veiculo furto_veiculo furto_rua roubo_carga roubo_cx_eletronico roubo_conducao_saque pessoas_desaparecidas pop_cisp area_precinct (max) mcirc risp, by(aisp month year)

rename year ano
rename month mes

*Add targets
merge 1:1 aisp ano mes using "data/SIM/meta_lv.dta"
sort aisp mes ano
drop _merge
merge 1:1 aisp ano mes using "data/SIM/meta_rr.dta"
sort aisp mes ano
drop _merge
merge 1:1 aisp ano mes using "data/SIM/meta_rv.dta"
sort aisp mes ano
drop _merge


*Add number of policemen. Dataset starts in 2008 and ends in 2015/6
gen mes_ano=ym(ano, mes)
merge 1:1 aisp mes_ano using data\ArquivosAuxiliares\Efetivo\efetivo_bpm_upp\efetivo_bpm_upp_mod.dta
drop _merge


*Add information on commanders
sort aisp mes_ano
merge 1:1 aisp mes_ano using data\ArquivosAuxiliares\Efetivo\Base_cmt_aisp.dta
drop _merge d_clus_det* exper_* troca exper_cmt_total cluster_det

*Create first and second semester variable
gen semestre=1 if mes>=1 & mes<=6
replace semestre=2 if mes>=7 & mes<=12

gen max_prize=1.5 if ano==2009 & semestre==2 | ano==2010 & semestre==1
replace max_prize=3 if  ano==2010 & semestre==2
replace max_prize=6 if  ano==2011 & semestre==1
replace max_prize=9 if  ano==2011 & semestre==2 | ano==2012
replace max_prize=13.5 if  ano>=2013 

gen min_prize=.5 if ano==2009 & semestre==2 | ano==2010 & semestre==1
replace min_prize=1 if  ano==2010 & semestre==2
replace min_prize=2 if  ano==2011 & semestre==1
replace min_prize=3 if  ano==2011 & semestre==2 | ano==2012
replace min_prize=1.5 if  ano>=2013 


rename letalidade violent_death
rename hom_doloso homicide
rename hom_culposo involuntary_homicide
rename tentat_hom attempt_homicide
rename apreensao_drogas drug_seizure
rename total_gun gun_seizure
rename apf arrest
rename aaapai juvenile_arrest
rename hom_por_interv_policial police_killing
rename total_roubos robbery
rename total_furtos theft
rename roubo_rua street_robbery
rename roubo_veiculo vehicle_robbery 
rename roubo_residencia burglary
rename roubo_comercio store_robbery
rename furto_rua street_theft
rename mcirc id_municipality
rename pop_cisp population
rename roubo_carga cargo_robbery
rename furto_veiculos vehicle_theft
rename mes month
rename ano year
rename meta_lv target_vd
rename meta_rr target_sr
rename meta_rv target_vr
rename semestre semester
rename encontro_cadaver body_found
rename encontro_ossada bones_found
rename area_precinct area_aisp
rename  efetivo_bpm policemen_aisp
rename efetivo_upp policemen_upp
rename mes_ano month_year
rename pessoas_desaparecidas missing_people 

gen violent_death_sim=homicide if year<=2010
replace violent_death_sim=violent_death if year>2010
gen dpolice_killing=(police_killing>0)
gen dbody_found=(body_found>0)
gen dbones_found=(bones_found>0)


rename roubo_conducao_saque withdraw_robbery
rename roubo_cx_eletronico atm_robbery
gen other_robberies= robbery - street_robbery
gen arrest2=arrest+juvenile_arrest

foreach x of varlist  juvenile_arrest arrest withdraw_robbery  atm_robbery street_robbery street_theft burglary store_robbery theft robbery gun_seizure drug_seizure vehicle_robbery vehicle_theft cargo_robbery other_robberies missing_people {
label variable `x' "registers of `x'"
}
foreach x of varlist  violent_death homicide involuntary_homicide attempt_homicide body_found bones_found police_killing {
label variable `x' "victims of `x'"
}
foreach x of varlist  rifle pistol machine_gun {
label variable `x' "number of `x' seizured"
}


label var population "predicted population at the precinct"
label var year "year"
label var month "month"
label var id_municipality "municipality identifier"
label var min_prize "monetary_value of the prize for AISP that hit the target (R$ 1000)"
label var max_prize "monetary_value of the prize for AISP in the 1st place (R$ 1000)"
label var n_precinct "number of precincts inside the aisp"
label var target_vd "month target for violent deaths"
label var target_vr "month target for vehicles robberies"
label var target_sr "month target for street robberies"
label var policemen_aisp "number of policemen per AISP"
label var policemen_upp "number of policemen at Pacifying Police Units located in the AISP"
label var id_cmt "id for police chief"
label var violent_death_sim "number of deaths contabilized in the target system"
label var arrest2 "total arrests (adults+juveniles)"
label var dpolice_killing "dummy variable for police killings"
label var dbody_found "dummy variable for body found"
label var dbones_found "dummy variable for bones found"


gen sem_year=yh(year, semester)
format sem_year %th
format month_year %tm

gen cycle=1 if month==1 | month==7
replace cycle=2 if month==2 | month==8
replace cycle=3 if  month==3 | month==9
replace cycle=4 if month==4 | month==10
replace cycle=5 if month==5 | month==11
replace cycle=6 if month==6 | month==12
label var cycle "Indicator for the month within the semester"


 keep if year>2003 & year<2019
 
 drop if aisp==.
 
xtset aisp month_year
order month year semester month_year  sem_year cycle aisp

*cd  "/Users/joanacmm/Dropbox/AvaliacaoSIM
*cd C:\Users\Presidencia\Dropbox\AvaliacaoSIM\

cd "C:\Users\mathi\OneDrive\Documents\GitHub\SIM\data"

save data_SIM_2021-07.dta, replace

*****************************************************************************************************************
* CREATE TREATMENT VARIABLES 
*****************************************************************************************************************

*There are no information on targets for AISP 1 and AISP 13
 drop if aisp==1 | aisp==13
*Meta mensal acumulada at顯 mes anterior

foreach x of varlist violent_death_sim target_vd street_robbery target_sr vehicle_robbery target_vr {

bysort aisp (sem_year month): gen `x'_cum= `x'[_n-1] if month==2 | month==8
bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==3 | month==9
bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==4 | month==10
bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==5 | month==11
bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==6 | month==12
}

foreach x of varlist violent_death_sim  street_robbery  vehicle_robbery  {
bysort aisp (sem_year month): gen `x'_cum2= `x' if month==1 | month==7
bysort  aisp (sem_year): replace `x'_cum2=  `x'+  `x'_cum2[_n-1] if month==2 | month==8
bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==3 | month==9
bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==4 | month==10
bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==5 | month==11
bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==6 | month==12
}


foreach x of varlist target_vd  target_sr  target_vr {
egen `x'_sem=sum(`x') , by(aisp sem_year) 
}
foreach x of varlist target_vd_sem  target_sr_sem  target_vr_sem {
replace `x'=. if year<2009 | year==2009 & semester==1 | year>2015
} 

gen on_target_vd=(violent_death_sim_cum<=target_vd_cum) if year<=2012
replace on_target_vd=(violent_death_sim_cum<=target_vd_cum*1.1) if year>=2013
gen on_target_sr=(street_robbery_cum<=target_sr_cum)
replace on_target_sr=(street_robbery_cum<=target_sr_cum*1.1) if year>=2013
gen on_target_vr=(vehicle_robbery_cum<=target_vr_cum)
replace on_target_vr=(vehicle_robbery_cum<=target_vr_cum*1.1) if year>=2013

replace on_target_vd=. if cycle==1
replace on_target_sr=. if cycle==1
replace on_target_vr=. if cycle==1

foreach x of varlist on_target_vr  on_target_vd  on_target_sr {
replace `x'=. if year<2009 | year==2009 & semester==1 | year>2015
} 

gen on_target= (on_target_vd==1 & on_target_sr==1 & on_target_vr==1)
replace on_target=. if cycle==1

sort aisp month_year 
gen lag1_on_target=on_target[_n-1]


gen dist_target_vd=violent_death_sim_cum /target_vd_sem -1 
bysort aisp (month_year): gen lag12_dist_target_vd=dist_target_vd[_n-12]

gen dist_target_vr=(vehicle_robbery_cum)/(target_vr_sem ) -1 
bysort aisp (month_year): gen lag12_dist_target_vr=dist_target_vr[_n-12]

gen dist_target_sr=street_robbery_cum /target_sr_sem -1 
bysort aisp (month_year): gen lag12_dist_target_sr=dist_target_sr[_n-12]



foreach x of varlist target_vd  {
label var `x' "violent death target (month)"
label var `x'_sem "violent death target (semester)"
label var `x'_cum "violent death target (cumulative until t-1)"
label var on_`x' "indicator for on target until t-1)"
label var dist_`x' "distance to the target (=0 on target, >0 above target)"

}


label var cycle "Indicator for the month"


order aisp year month semester month_year sem_year cycle violent_death homicide involuntary_homicide attempt_homicide violent_death_sim  police_killing body_found bones_found robbery theft street_robbery vehicle_robbery vehicle_theft cargo_robbery other_robberies drug_seizure gun_seizure arrest juvenile_arrest target_vd target_sr target_vr 

gen sample=(year==2009 & semester==2 | year>=2010 & year<=2014 | year==2015 & semester==1)

 
 keep if year>2003 & year<2019
 
 drop if aisp==.
 
xtset aisp month_year

egen tag=tag(aisp id_cmt)
egen n_comandos=sum(tag), by(id_cmt)
* media=2.2, median = 2
egen group=group(id_cmt aisp)
gen n=1 
egen tempo_comando=sum(n), by(group)
*median=mean=14 meses

*cd  "/Users/joanacmm/Dropbox/AvaliacaoSIM
*cd C:\Users\Presidencia\Dropbox\AvaliacaoSIM\

cd "C:\Users\mathi\OneDrive\Documents\GitHub\SIM\data"

save data_SIM_2021-07.dta, replace

