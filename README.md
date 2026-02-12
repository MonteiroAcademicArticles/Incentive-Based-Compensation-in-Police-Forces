This is the README file for the paper “Incentive-Based Compensation in Police Forces”, which has been accepted for publication in the Journal of Policy Analysis and Management.

The files are organized in three folders:

DATA folder contains raw data organized by source and the final dataset of the paper.

_police folders contain data from police sources:
Base_DP_consolidado indicates crime records available at: https://www.ispdados.rj.gov.br/
efetivo_aisp_upp indicates the number of police officers at each Unidade de Policia Pacificadora
efetivo_aisp indicates the number of police officers at each Area Integrada de Segurança Publica (AISP), which is equivalent to a police batalion area
_target folder contains information on crime targets set by the Secretariat of Security
_population folder contains data on the population at AISP area
_geo folder contain information on AISP area

All these datasets are merged by code creation.R and generate file data_SIM_2021-07.dta that in the final dataset of the paper.

ANALYSIS
All program files are located in the Analysis folder. There is one main program file (MASTER.R) used for the analysis, which makes use of the remaining user-written programs to facilitate the analysis and export of the estimations into user-friendly tables. The order in which should be ran separately are indicated in the file name. Script files with the same number do can be ran in any order among itself.

00-creation.R: Creates main intermediate dataset based on original police data
01-construction.R: Creates final dataset based on the intermediate dataset created in 00-creation.R and other files.
02-analysis-descriptives.R: Executes and export descriptive analysis
02-analysis-main.R: Executes and export main regression analysis
03-robustness-spatial2.R: Execute and export Spatial regression analysis
03-robustness-placebo.R: Execute and export placebo regression analysis
03-robustness-poisson.R: Execute and export Poisson regression analysis
utils.R: Contains useful functions to be used along in other scripts

OUTPUTS
