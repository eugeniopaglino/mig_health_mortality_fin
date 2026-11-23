# Loading packages
library(here)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/070_create_health_longitudinal.R')

in_dir <- here('data')
out_dir <- here('output')

visits <- read_feather(here(out_dir,'050_visits_data.arrow'))

# Remove anomalous pre-1987 records
visits <- visits[year_of_visit>=1987]

visits[,date_simple:=as.Date(date_simple)]

visits[,ICD10_letter:=str_sub(main_ICD10_2,1,1)]
visits[,ICD10_number:=as.integer(str_sub(main_ICD10_2,2,3))]

visits[(main_ICD10_2 == 'E11'),diabetes:=T]
visits[(main_ICD10_2 == 'J45'),asthma:=T]
visits[(main_ICD10_2 == 'I10'),hypertension:=T]
visits[(ICD10_letter == 'M' & between(ICD10_number,15,19)),arthritis:=T]
visits[(ICD10_letter == 'I' & between(ICD10_number,20,25)),heart_disease:=T]
visits[(ICD10_letter == 'F'),mental_disorder:=T]
visits[(ICD10_letter == 'C'),neoplasm:=T]

visits[(main_ICD9_3 == '250'),diabetes:=T]
visits[(main_ICD9_3 == '493'),asthma:=T]
visits[(main_ICD9_3 == '401'),hypertension:=T]
visits[(main_ICD9_3 == '715'),arthritis:=T]
visits[between(as.integer(main_ICD9_3),410,414),heart_disease:=T]
visits[between(as.integer(main_ICD9_3),290,319),mental_disorder:=T]
visits[between(as.integer(main_ICD9_3),140,239),neoplasm:=T]

conditions <- c('diabetes','asthma','hypertension','arthritis','heart_disease','mental_disorder','neoplasm')

for (condition in conditions) {
  
  visits[is.na(get(condition)),paste0(condition):=F]
  visits[,paste0(condition,'_diag'):=any(get(condition)),by=.(id)]
  visits[get(condition)==T,paste0(condition,'_date'):=min(date_simple),by=.(id)]
  visits[get(condition)==F,paste0(condition,'_date'):=make_date(2100,1,1),by=.(id)]
  visits[,paste0(condition,'_date'):=min(get(paste0(condition,'_date'))),by=.(id)]
  
}

diagnoses <- visits[,lapply(.SD,function(x) x[1]),by=.(id),.SDcols=sapply(conditions,function(x) c(paste0(x,'_diag'),paste0(x,'_date')))]

rm(visits)
gc()

# Fix naming for successful melt
setnames(
  diagnoses,
  c('heart_disease_diag','heart_disease_date',
    'mental_disorder_diag','mental_disorder_date'),
  c('heartdisease_diag','heartdisease_date',
    'mentaldisorder_diag','mentaldisorder_date')
  )

# Convert in long (id-condition) format
conditions <- c('diabetes','asthma','hypertension','arthritis','heartdisease','mentaldisorder','neoplasm')
diag_vars <- c('id',sapply(conditions,function(x) paste0(x,'_diag')))
diagnoses_long <- melt(
  diagnoses[,..diag_vars],
  id.vars = 'id',
  measure.vars = measure(condition,variable,sep='_'),
  value.name = 'diagnosed'
)
diagnoses_long[,variable:=NULL]

date_vars <- c('id',sapply(conditions,function(x) paste0(x,'_date')))
diagnosis_dates_long <- melt(
  diagnoses[,..date_vars],
  id.vars = 'id',
  measure.vars = measure(condition,variable,sep='_'),
  value.name = 'date'
)
diagnosis_dates_long[,variable:=NULL]

diagnoses_long <- merge(diagnoses_long,diagnosis_dates_long,by=c('id','condition'))

rm(diagnosis_dates_long)
gc()

# Go back to correct condition names
diagnoses_long[condition=='heartdisease',condition:='heart_disease']
diagnoses_long[condition=='mentaldisorder',condition:='mental_disorder']

reimbursements <- read_feather(here(out_dir,'060_med_reimbursements_data.arrow'))

diagnoses_long <- merge(diagnoses_long,reimbursements,by=c('id','condition'),all.x=T)

diagnoses_long[,diagnosis_date:=fifelse(is.na(start_date),date,pmin(date,start_date))]
diagnoses_long[!is.na(start_date),diagnosed:=T]
diagnoses_long[,`:=`(date=NULL,start_date=NULL,start_year=NULL)]

write_feather(diagnoses_long,here(out_dir,'070_health_data.arrow'))
