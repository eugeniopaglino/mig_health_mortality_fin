# Loading packages
library(here)
library(arrow)
library(mgcv)
library(survival)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/105_mortality_models_pob_ageing.R')

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
# Combined Asia, Oceania, and Americas
data[
  ,
  pob:=fcase(
    pob %chin% c('Americas','Asia and Oceania'),'Americas, Asia, and Oceania',
    rep(TRUE,.N), pob
  )
]
# Set Finland as reference
data[,pob:=fct_relevel(pob,'Finland')]

# Compute person-years
data[,py:=fifelse(death==1 | exit==1,0.5,1)]

# Direct estimation
rates <- data[,.(Dx=sum(death),Ex=sum(py)),by=.(age,sex,pob)]

rates[,age_group:=cut(age,seq(25,85,5),right=F)]
rates[,x:=as.integer(str_match(age_group,'\\d+'))]

rates <- rates[,.(Dx=sum(Dx),Ex=sum(Ex)),by=.(x,sex,pob)]
setorder(rates,x)
rates[,Mx:=Dx/Ex]

raw_rates_plot <- rates |>
  ggplot() +
  geom_line(mapping=aes(x=x,y=log(Mx),color=pob,linetype=sex)) +
  labs(x='Age (x)',color='Origin',linetype='Sex') +
  theme_minimal()

raw_rates_plot

svg(here(fig_dir,'105_raw_rates.svg'),width = 6,height = 5)
raw_rates_plot
dev.off()

##############################
#         BASIC MODEL        #
##############################

collapsed_data <- data[,.(D=sum(death),E=sum(py)), by=.(age,sex,pob)]

collapsed_data[,group:=paste0(sex,'_',pob)]
collapsed_data[,group:=as.factor(group)]

nb_basic <- bam(
  D ~ sex + pob + sex:pob + s(age,by=group,k=10),
  data = collapsed_data,
  offset = log(E),
  family = nb()
)

summary(nb_basic)

new_data <- expand.grid(
  sex = unique(data$sex),
  pob = unique(data$pob),
  age = seq(25,84)
)

setDT(new_data)

new_data[,group:=paste0(sex,'_',pob)]

new_data[,log_h:=predict(nb_basic,newdata = new_data)]

setorder(new_data,sex,pob,age)
new_data[,S:=exp(-shift(cumsum(exp(log_h)),fill=0)),by=.(sex,pob)]

estimates_basic <- copy(new_data)

fwrite(estimates_basic,here(out_dir,'105_model_estimates_basic.csv'))

incidence_plot <- estimates_basic |>
  ggplot() +
  geom_line(mapping=aes(x=age,y=log_h,color=pob,linetype=sex),linewidth=1.1) +
  geom_point(data=rates,mapping=aes(x=x+2.5,y=log(Mx),color=pob,shape=sex)) +
  labs(x='Age (x)',y='log Hazard Rate log(h(x))',color='Place of Birth',linetype='Sex',shape='Sex') +
  theme_minimal()

incidence_plot

svg(here(fig_dir,paste0('105_incidence_plot.svg')),width = 6,height = 5)
print(incidence_plot)
dev.off()

prevalence_plot <- estimates_basic |>
  ggplot() +
  geom_line(mapping=aes(x=age,y=S,color=pob,linetype=sex),linewidth=1.1) +
  labs(x='Age (x)',y='Survival S(x)',color='Place of Birth',linetype='Sex') +
  theme_minimal()

prevalence_plot

svg(here(fig_dir,paste0('105_prevalence_plot.svg')),width = 6,height = 5)
print(prevalence_plot)
dev.off()

##############################
#          COX BASIC         #
##############################

data[,status:=death]
data[,tstart:=age-25]
data[,tend:=tstart+1]
data[death==1 | exit==1,tend:=tstart+0.5]
data[,age_group:=fifelse(age<65,'25-64','65+')]

ajfit_f <- survfit(Surv(tstart,tend,status) ~ pob, data = data[sex=='Female'])

plot(ajfit_f)

cox_basic_f <- coxph(
  Surv(tstart,tend,status) ~ pob + age_group + pob:age_group + as.factor(stat_year),
  data = data[sex=='Female']
)

summary(cox_basic_f)

ajfit_m <- survfit(Surv(tstart,tend,status) ~ pob, data = data[sex=='Male'])

plot(ajfit_m)

cox_basic_m <- coxph(
  Surv(tstart,tend,status) ~ pob + age_group + pob:age_group + as.factor(stat_year),
  data = data[sex=='Male']
)

summary(cox_basic_m)

##############################
#          COX FULL         #
##############################

#cox_full_f <- coxph(
#  Surv(tstart,tend,status) ~ pob + age_group + pob:age_group + ses_full + income_q_h + employment + as.factor(stat_year),
#  data = data[sex=='Female']
#)

#summary(cox_full_f)

#cox_full_m <- coxph(
#  Surv(tstart,tend,status) ~ pob + age_group + pob:age_group + ses_full + income_q_h + employment + as.factor(stat_year),
#  data = data[sex=='Male']
#)

#summary(cox_full_m)

models <- list(cox_basic_f,cox_basic_m)
coef_table <- vector(mode='list',length=length(models))
coef_labs <- data.table(
  raw_name = c("pobAfrica","pobAfrica:age_group65+",
               "pobAmericas","pobAmericas:age_group65+",
               "pobAsia and Oceania","pobAsia and Oceania:age_group65+",
               "pobMiddle East","pobMiddle East:age_group65+",
               "pobEurope","pobEurope:age_group65+",         
               "pobRussia","pobRussia:age_group65+"),
  clean_name = c('Africa','Africa X Aged 65+',
                 'Americas','Americas X Aged 65+',
                 'Asia and Oceania','Asia and Oceania X Aged 65+',
                 'Middle East','Middle East X Aged 65+',
                 'Europe','Europe X Aged 65+',
                 'Russia','Russia X Aged 65+')
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

fwrite(coef_table,here('output','105_coef_table.csv'))
