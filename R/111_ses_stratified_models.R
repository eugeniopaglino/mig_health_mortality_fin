# Loading packages
library(here)
library(arrow)
library(mgcv)
library(survival)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/111_ses_stratified_models.R')

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

# Compute person-years
data[,py:=fifelse(death==1 | exit==1,0.5,1)]

# Direct estimation
rates <- data[,.(Dx=sum(death),Ex=sum(py)),by=.(age,sex,origin)]

rates[,age_group:=cut(age,seq(25,85,5),right=F)]
rates[,x:=as.integer(str_match(age_group,'\\d+'))]

rates <- rates[,.(Dx=sum(Dx),Ex=sum(Ex)),by=.(x,sex,origin)]
setorder(rates,x)
rates[,Mx:=Dx/Ex]

raw_rates_plot <- rates |>
  ggplot() +
  geom_line(mapping=aes(x=x,y=log(Mx),color=origin,linetype=sex)) +
  labs(x='Age (x)',color='Origin',linetype='Sex') +
  theme_minimal()

raw_rates_plot

svg(here(fig_dir,'111_raw_rates.svg'),width = 6,height = 5)
raw_rates_plot
dev.off()

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

# Drop SES groups with poor overlap
data <- data[!(ses_full %chin% c('Unknown','Never in labor force'))]
data[,ses_full:=fct_drop(ses_full)]

setorder(data,sex,ses_full)
data[,group:=.GRP,by=.(sex,ses_full)]

groups_dict <- unique(data[,.(group,sex,ses_full)])
models <- vector(mode='list',length = max(data$group))

for (i in 1:length(models)) {
  
  cox_fit <- coxph(
    Surv(tstart,tend,status) ~ origin,
    data = data[group==i]
  )
  
  models[[i]] <- cox_fit
  
}

coef_table <- vector(mode='list',length=length(models))
coef_labs <- data.table(
  raw_name = c('originForeign-Foreign'),
  clean_name = c('International Immigrant')
)

for (i in 1:length(models)) {
  this_coef_table <- broom::tidy(models[[i]],exponentiate=T,conf.int=T)
  setDT(this_coef_table)
  
  this_coef_table <- this_coef_table[term %chin% coef_labs$raw_name]
  this_coef_table[,term:=plyr::mapvalues(term,coef_labs$raw_name,coef_labs$clean_name,warn_missing = F)]
  
  this_coef_table[,group:=i]
  
  coef_table[[i]] <- this_coef_table
}

coef_table <- rbindlist(coef_table)
coef_table <- merge(coef_table,groups_dict,by=c('group'))

coef_table[,group:=NULL]
setcolorder(coef_table,c('sex','ses_full'))

fwrite(coef_table,here('output','111_coef_table.csv'))
