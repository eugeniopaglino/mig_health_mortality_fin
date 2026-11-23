# Loading packages
library(here)
library(arrow)
library(mgcv)
library(survival)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/113_mortality_models_mig_cohort.R')

in_dir <- here('input')
out_dir <- here('output')
fig_dir <- here('figures')

data <- read_feather(here(out_dir,'011_general_model_data.arrow'))
setDT(data)

# Define death and exit variables
data[,death:=0]
data[stat_year==death_year,death:=1]

data[,exit:=0]
data[stat_year==mig_year_out,exit:=1]

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

# Compute person-years
data[,py:=fifelse(death==1 | exit==1,0.5,1)]

##############################
#          COX BASIC         #
##############################

data[,status:=death]
data[,tstart:=age-25]
data[,tend:=tstart+1]
data[death==1 | exit==1,tend:=tstart+0.5]
data[
  ,
  age_group:=fcase(
    between(age,25,44),'25-44',
    between(age,45,64),'45-64',
    between(age,65,84),'65-84'
  )
]

# Create birth cohorts
data[,birth_cohort:=cut(birth_year,seq(1920,1995,5),right=F)]

# Create migration cohorts
data[,origin:=fcase(
  origin=='Native-Finn','Native-Finn',
  origin=='Foreign-Foreign' & mig_year_in <= 1995,'Migrant (1990-1995)',
  origin=='Foreign-Foreign' & mig_year_in > 1995,'Migrant (1996-2000)'
)]

# Make native the references
data[,origin:=fct_relevel(origin,'Native-Finn')]

cox_basic_f <- coxph(
  Surv(tstart,tend,status) ~ origin + age_group + origin:age_group + as.factor(birth_cohort),
  data = data[sex=='Female']
)

summary(cox_basic_f)

cox_basic_m <- coxph(
  Surv(tstart,tend,status) ~ origin + age_group + origin:age_group + as.factor(birth_cohort),
  data = data[sex=='Male']
)

summary(cox_basic_m)

models <- list(cox_basic_f,cox_basic_m)
coef_table <- vector(mode='list',length=length(models))
coef_labs <- data.table(
  raw_name = c('originMigrant (1990-1995)',
               'originMigrant (1990-1995):age_group45-64',
               'originMigrant (1990-1995):age_group65-84',
               'originMigrant (1996-2000)',
               'originMigrant (1996-2000):age_group45-64',
               'originMigrant (1996-2000):age_group65-84'),
  clean_name = c('Migrant (1990-1995)',
                 'Migrant (1990-1995) X Aged 45-64',
                 'Migrant (1990-1995) X Aged 65-84',
                 'Migrant (1996-2000)',
                 'Migrant (1996-2000) X Aged 45-64',
                 'Migrant (1996-2000) X Aged 65-84')
)

for (i in 1:length(models)) {
  this_coef_table <- broom::tidy(models[[i]],exponentiate=T,conf.int=T)
  setDT(this_coef_table)
  
  this_coef_table <- this_coef_table[term %chin% coef_labs$raw_name]
  this_coef_table[,term:=plyr::mapvalues(term,coef_labs$raw_name,coef_labs$clean_name,warn_missing = F)]
  
  this_coef_table[,id:=i]
  
  coef_table[[i]] <- this_coef_table
}

coef_table <- rbindlist(coef_table)

fwrite(coef_table,here('output','113_coef_table.csv'))
