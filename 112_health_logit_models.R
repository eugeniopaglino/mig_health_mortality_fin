# Loading packages
library(here)
library(arrow)
library(mgcv)
library(survival)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/112_health_logit_models.R')

in_dir <- here('input')
out_dir <- here('output')
fig_dir <- here('figures')

data <- read_feather(here(out_dir,'011_general_model_data.arrow'))
setDT(data)

# Make native the references
data[,origin:=fct_relevel(origin,'Native-Finn')]
# Make never married the references
data[,marital_status:=fct_relevel(marital_status,'Never married','Married','Divorced','Widowed')]
# Make no child the references
data[,child_b18:=fct_relevel(child_b18,'None','1+','Unknown')]
# Make employed the references
data[,employment:=fct_relevel(employment,'Employed','Unemployed','Retired or receiving pension','Not in labor force')]
# Make upper-level white collar the reference
data[,ses_full:=fct_relevel(
  ses_full,
  'Upper white collar'
)]

# Bring in data to select only eligible cases_full
eligibility_data <- read_feather(here(out_dir,'012_health_eligibility.arrow'))

conditions_data <- data.table(
  condition = unique(eligibility_data$condition)
)
conditions_data[,diagnosis_variable:=sapply(conditions_data$condition,function(x) paste0('diagnosis_year_',x))]

coef_tables <- vector(mode='list',length = nrow(conditions_data))

for (i in 1:nrow(conditions_data)) {
  print(conditions_data$condition[i])
  this_data <- merge(data,eligibility_data[condition==conditions_data$condition[i],.(id,eligible)],by='id',all.x=T)
  
  # Set missing to eligible, since they have no hospital records
  this_data[is.na(eligible),eligible:=1]
  
  # Find diagnosis in follow-up 
  this_data[,diagnosed:=fifelse(get(conditions_data$diagnosis_variable[i])<2020,1,0)]
  # Include diagnoses in washout
  this_data[eligible==0,diagnosed:=1]
  # remake dataset so it has one observation per individual
  this_data <- this_data[
    ,
    .(id=id[1],diagnosed=diagnosed[1],
      sex=sex[1],age=age[1],origin=origin[1],birth_year=birth_year[1],
      ses_full=ses_full[1],income_q_h=income_q_h[1],employment=employment[1]),
    by=.(id)
    ]
  
  ##############################
  #         LOGIT BASIC        #
  ##############################
  
  this_data[
    ,
    age_group:=fcase(
      between(age,25,44),'25-44',
      between(age,45,64),'45-64',
      between(age,65,84),'65-84'
    )
  ]
  
  # Create birth cohorts
  this_data[,birth_cohort:=cut(birth_year,seq(1920,1995,5),right=F)]
  
  logit_basic_f <- glm(
    diagnosed ~ origin + age_group + origin:age_group + as.factor(birth_cohort),
    family = binomial(link='logit'),
    data = this_data[sex=='Female']
  )
  
  summary(logit_basic_f)
  
  logit_basic_m <- glm(
    diagnosed ~ origin + age_group + origin:age_group + as.factor(birth_cohort),
    family = binomial(link='logit'),
    data = this_data[sex=='Male']
  )
  
  summary(logit_basic_m)
  
  models <- list(logit_basic_f,logit_basic_m)
  
  coef_table <- vector(mode='list',length=length(models))
  coef_labs <- data.table(
    raw_name = c('originForeign-Foreign','originForeign-Foreign:age_group45-64','originForeign-Foreign:age_group65-84'),
    clean_name = c('International Immigrant','International Immigrant X Aged 45-64','International Immigrant X Aged 65-84')
  )
  
  for (j in 1:length(models)) {
    this_coef_table <- broom::tidy(models[[j]],exponentiate=T,conf.int=T)
    setDT(this_coef_table)
    
    this_coef_table <- this_coef_table[term %chin% coef_labs$raw_name]
    this_coef_table[,term:=plyr::mapvalues(term,coef_labs$raw_name,coef_labs$clean_name,warn_missing = F)]
    
    this_coef_table[,id:=j]
    
    coef_table[[j]] <- this_coef_table
  }
  
  coef_table <- rbindlist(coef_table)
  coef_table[,condition:=conditions_data$condition[i]]
  coef_tables[[i]] <- coef_table
  
}

coef_table_all_conditions <- rbindlist(coef_tables)

fwrite(coef_table_all_conditions,here('output','112_coef_table.csv'))
