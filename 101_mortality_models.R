# Loading packages
library(here)
library(arrow)
library(mgcv)
library(survival)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/101_mortality_models.R')

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

svg(here(fig_dir,'101_raw_rates.svg'),width = 6,height = 5)
raw_rates_plot
dev.off()

# As a basic check, compute temporary life expectancy between 25 and 85 to 
# compare with official life tables
nat_rates <- data[,.(Dx=sum(death),Ex=sum(py)),by=.(age,sex,stat_year)]

nat_rates[,period:=cut(stat_year,seq(2005,2020,5),right=F)]
nat_rates[,age_group:=cut(age,seq(25,85,5),right=F)]
nat_rates[,x:=as.integer(str_match(age_group,'\\d+'))]

nat_rates <- nat_rates[,.(Dx=sum(Dx),Ex=sum(Ex)),by=.(x,sex,period)]
setorder(nat_rates,x)
nat_rates[,Mx:=Dx/Ex]

rates_trend_plot <- nat_rates |>
  ggplot() +
  geom_line(mapping=aes(x=x,y=log(Mx),color=period,group = period)) +
  labs(x='Age (x)',color='Period') +
  facet_wrap(~sex) + 
  theme_minimal()

rates_trend_plot

# Set up life table functions
my_lt <- function(Mx, ages) {
  N <- length(Mx)
  w <- c(diff(ages),5)
  ax <- c(rep(0.5, N))
  nax <- w * ax
  qx <- (w * Mx) / (1 + w * (1-ax) * Mx)
  px <- 1 - qx
  lx <- c(1,  cumprod(px))
  dx <- -diff(lx)
  Lx <- lx[-1] * w + dx * nax
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx[-(N+1)]
  
  data.table(
    x=ages,Mx=Mx,w=w,nax=nax,qx=qx,px=px,
    lx=lx[-(N+1)],dx=dx,
    Lx=Lx,Tx=Tx,ex=ex
  )
}

ea_b <- function(lt,a=25,b=85) {
  la <- lt[x==a]$lx
  Ta_b <- sum(lt[between(x,a,b)]$Lx)
  Ta_b/la
}

lt_male_10_14 <- my_lt(nat_rates[sex=='Male' & period=='[2010,2015)']$Mx,ages=seq(25,84,5))
lt_male_15_19 <- my_lt(nat_rates[sex=='Male' & period=='[2015,2020)']$Mx,ages=seq(25,84,5))
lt_female_10_14 <- my_lt(nat_rates[sex=='Female' & period=='[2010,2015)']$Mx,ages=seq(25,84,5))
lt_female_15_19 <- my_lt(nat_rates[sex=='Female' & period=='[2015,2020)']$Mx,ages=seq(25,84,5))

# Our e25_85 Estimates
# Males 2010-2014: 51.45
# Males 2015-2019: 52.30
# Females 2010-2014: 55.40
# Females 2015-2019: 55.76

# HMD e25_85 Estimates and gaps
# Males 2010-2014: 51.20 + 0.25
# Males 2015-2019: 52.12 + 0.18
# Females 2010-2014: 55.28 + 0.12
# Females 2015-2019: 55.64 + 0.12

# Very comforting considering we are oversampling the foreign-born population,
# which means some discrepancies are to be expected

##############################
#         BASIC MODEL        #
##############################

collapsed_data <- data[,.(D=sum(death),E=sum(py)), by=.(age,sex,origin)]

collapsed_data[,group:=paste0(sex,'_',origin)]
collapsed_data[,group:=as.factor(group)]

nb_basic <- bam(
  D ~ sex + origin + sex:origin + s(age,by=group,k=10),
  data = collapsed_data,
  offset = log(E),
  family = nb()
)

summary(nb_basic)

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

fwrite(estimates_basic,here(out_dir,'101_model_estimates_basic.csv'))

incidence_plot <- estimates_basic |>
  ggplot() +
  geom_line(mapping=aes(x=age,y=log_h,color=origin,linetype=sex),linewidth=1.1) +
  geom_point(data=rates,mapping=aes(x=x+2.5,y=log(Mx),color=origin,shape=sex)) +
  labs(x='Age (x)',y='log Hazard Rate log(h(x))',color='Origin',linetype='Sex',shape='Sex') +
  theme_minimal()

incidence_plot

svg(here(fig_dir,paste0('101_incidence_plot.svg')),width = 6,height = 5)
print(incidence_plot)
dev.off()

prevalence_plot <- estimates_basic |>
  ggplot() +
  geom_line(mapping=aes(x=age,y=S,color=origin,linetype=sex),linewidth=1.1) +
  labs(x='Age (x)',y='Survival S(x)',color='Origin',linetype='Sex') +
  theme_minimal()

prevalence_plot

svg(here(fig_dir,paste0('12_prevalence_plot.svg')),width = 6,height = 5)
print(prevalence_plot)
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

# Create birth cohorts
data[,birth_cohort:=cut(birth_year,seq(1920,1995,5),right=F)]

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

##############################
#          COX FULL         #
##############################

cox_full_f <- coxph(
  Surv(tstart,tend,status) ~ origin + age_group + origin:age_group + ses_full + income_q_h + employment + as.factor(birth_cohort),
  data = data[sex=='Female']
)

summary(cox_full_f)

cox_full_m <- coxph(
  Surv(tstart,tend,status) ~ origin + age_group + origin:age_group + ses_full + income_q_h + employment + as.factor(birth_cohort),
  data = data[sex=='Male']
)

summary(cox_full_m)

models <- list(cox_basic_f,cox_basic_m,cox_full_f,cox_full_m)
coef_table <- vector(mode='list',length=length(models))
coef_labs <- data.table(
  raw_name = c('originForeign-Foreign','originForeign-Foreign:age_group45-64','originForeign-Foreign:age_group65-84',
               'ses_fullLower white collar','ses_fullManual',
               'ses_fullSelf-employed','ses_fullStudent',
               'ses_fullNever in labor force','ses_fullUnknown',
               'income_q_h2nd','income_q_h3rd','income_q_h4th','income_q_h5th','income_q_hUnknown',
               'employmentUnemployed','employmentRetired or receiving pension','employmentNot in labor force'),
  clean_name = c('International Immigrant','International Immigrant X Aged 45-64','International Immigrant X Aged 65-84',
                 'Lower white collar','Manual or farmer',
                 'Self-employed','Student',
                 'Never in labor force','Unknown',
                 '2nd income quintile','3rd income quintile','4th income quintile','5th income quintile','Unknown income',
                 'Unemployed','Retired or receiving pension','Not in labor force')
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

fwrite(coef_table,here('output','101_coef_table.csv'))
