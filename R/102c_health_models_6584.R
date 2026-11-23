# Loading packages
library(here)
library(arrow)
library(mgcv)
library(survival)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/102c_health_models_6584.R')

in_dir <- here('input')
out_dir <- here('output')
fig_dir <- here('figures')

data <- read_feather(here(out_dir,'011_general_model_data.arrow'))
setDT(data)

# Define diagnosis, death, and exit variables
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
  
  # Get only eligible (i.e. disease free in 2005)
  this_data <- this_data[eligible==1]
  # Censor all those who have been diagnosed
  this_data <- this_data[stat_year<=get(conditions_data$diagnosis_variable[i])]
  # Define diagnosis, death, and exit variables
  this_data[,diagnosis:=0]
  this_data[stat_year==get(conditions_data$diagnosis_variable[i]),diagnosis:=1]
  
  # Compute person-years
  this_data[,py:=fifelse(diagnosis==1 | death==1 | exit==1,0.5,1)]
  
  # Direct estimation
  rates <- this_data[,.(Dx=sum(diagnosis),Ex=sum(py)),by=.(age,sex,origin)]
  
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
  
  svg(here(fig_dir,paste0('102_raw_rates_',conditions_data$condition[i],'.svg')),width = 6,height = 5)
  print(raw_rates_plot)
  dev.off()
  
  ##############################
  #          GAM MODEL         #
  ##############################
  
  collapsed_data <- this_data[,.(D=sum(diagnosis),E=sum(py)), by=.(age,sex,origin)]
  
  collapsed_data[,group:=paste0(sex,'_',origin)]
  collapsed_data[,group:=as.factor(group)]
  
  nb_basic <- bam(
    D ~ sex + origin + sex:origin + s(age,by=group,k=10),
    data = collapsed_data,
    offset = log(E),
    family = nb()
  )
  
  new_data <- expand.grid(
    sex = unique(data$sex),
    origin = unique(data$origin),
    age = seq(25,84)
  )
  
  setDT(new_data)
  
  new_data[,group:=paste0(sex,'_',origin)]
  
  new_data[,log_h:=predict(nb_basic,newdata = new_data)]
  
  setorder(new_data,sex,origin,age)
  new_data[,S:=exp(-shift(cumsum(exp(log_h)),fill=0)),by=.(sex,origin)]
  
  estimates_basic <- copy(new_data)
  
  incidence_plot <- estimates_basic |>
    ggplot() +
    geom_line(mapping=aes(x=age,y=log_h,color=origin,linetype=sex),linewidth=1.1) +
    geom_point(data=rates,mapping=aes(x=x+2.5,y=log(Mx),color=origin,shape=sex)) +
    labs(x='Age (x)',y='log Hazard Rate log(h(x))',color='Origin',linetype='Sex',shape='Sex') +
    theme_minimal()
  
  incidence_plot
  
  svg(here(fig_dir,paste0('102_incidence_plot_',conditions_data$condition[i],'.svg')),width = 6,height = 5)
  print(incidence_plot)
  dev.off()
  
  prevalence_plot <- estimates_basic |>
    ggplot() +
    geom_line(mapping=aes(x=age,y=S,color=origin,linetype=sex),linewidth=1.1) +
    labs(x='Age (x)',y='Survival S(x)',color='Origin',linetype='Sex') +
    theme_minimal()
  
  prevalence_plot
  
  svg(here(fig_dir,paste0('102_prevalence_plot_',conditions_data$condition[i],'.svg')),width = 6,height = 5)
  print(prevalence_plot)
  dev.off()
  
  ##############################
  #          COX BASIC         #
  ##############################
  
  this_data[,status:=diagnosis]
  this_data[,tstart:=age-25]
  this_data[,tend:=tstart+1]
  this_data[death==1 | exit==1 | diagnosis==1,tend:=tstart+0.5]
  this_data[
    ,
    age_group:=fcase(
      between(age,25,44),'25-44',
      between(age,45,64),'45-64',
      between(age,65,84),'65-84'
    )
  ]
  
  this_data[,age_group:=fct_relevel(age_group,'65-84')]
  
  # Create birth cohorts
  this_data[,birth_cohort:=cut(birth_year,seq(1920,1995,5),right=F)]
  
  cox_basic_f <- coxph(
    Surv(tstart,tend,status) ~ origin + age_group + origin:age_group + as.factor(birth_cohort),
    data = this_data[sex=='Female']
  )
  
  summary(cox_basic_f)
  
  cox_basic_m <- coxph(
    Surv(tstart,tend,status) ~ origin + age_group + origin:age_group + as.factor(birth_cohort),
    data = this_data[sex=='Male']
  )
  
  summary(cox_basic_m)
  
  ##############################
  #          COX FULL         #
  ##############################
  
  cox_full_f <- coxph(
    Surv(tstart,tend,status) ~ origin + age_group + origin:age_group + ses_full + income_q_h + employment + as.factor(birth_cohort),
    data = this_data[sex=='Female']
  )
  
  summary(cox_full_f)
  
  cox_full_m <- coxph(
    Surv(tstart,tend,status) ~ origin + age_group + origin:age_group + ses_full + income_q_h + employment + as.factor(birth_cohort),
    data = this_data[sex=='Male']
  )
  
  summary(cox_full_m)
  
  models <- list(cox_basic_f,cox_basic_m,cox_full_f,cox_full_m)
  coef_table <- vector(mode='list',length=length(models))
  coef_labs <- data.table(
    raw_name = c('originForeign-Foreign','originForeign-Foreign:age_group25-44','originForeign-Foreign:age_group45-64',
                 'ses_fullLower white collar','ses_fullManual',
                 'ses_fullSelf-employed','ses_fullStudent',
                 'ses_fullNever in labor force','ses_fullUnknown',
                 'income_q_h2nd','income_q_h3rd','income_q_h4th','income_q_h5th','income_q_hUnknown',
                 'employmentUnemployed','employmentRetired or receiving pension','employmentNot in labor force'),
    clean_name = c('International Immigrant','International Immigrant X Aged 25-44','International Immigrant X Aged 45-64',
                   'Lower white collar','Manual or farmer',
                   'Self-employed','Student',
                   'Never in labor force','Unknown',
                   '2nd income quintile','3rd income quintile','4th income quintile','5th income quintile','Unknown income',
                   'Unemployed','Retired or receiving pension','Not in labor force')
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

fwrite(coef_table_all_conditions,here('output','102c_coef_table.csv'))
