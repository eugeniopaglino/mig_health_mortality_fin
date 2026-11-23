# Loading packages
library(here)
library(arrow)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/011_create_model_data.R')

out_dir <- here('output')

# Load data
data <- read_feather(here(out_dir,'008_basic_data.arrow'))
setnames(data,c('ses'),c('current_ses'))
household_data <- read_feather(here(out_dir,'009_household_data.arrow'))

mig_data_foreign <- read_feather(here(out_dir,'001_mig_histories.arrow'))

# Calculation of how many migrants we lose because we don't have health
# data before 2000

# Lost
nrow(mig_data_foreign[(mig_year_in %between% c(1990,2000)) & ((mig_year_out-mig_year_in)>5) & mig_year_out<2005])
# Total
nrow(mig_data_foreign[(mig_year_in %between% c(1990,2000)) & (((mig_year_out-mig_year_in)>5) | is.na(mig_year_out))])

mig_data_foreign[,time_in_fi:=NULL]
mig_data_native <- read_feather(here(out_dir,'001_mig_histories_native.arrow'))

mig_data <- rbindlist(list(mig_data_foreign,mig_data_native),use.names = T)

rm(mig_data_foreign,mig_data_native)
gc()

# Merge basic and household data
data <- merge(data,household_data,by=c('id','stat_year'),all.x=T)

rm(household_data)
gc()

# Merge migration data
data <- merge(data,mig_data,by=c('id'),all.x=T)

rm(mig_data)
gc()

# BASIC ELIGIBILITY CHECKS

# Add dummy for death
data[,dead:=fifelse(is.na(death_year),F,T)]

# Drop those that died before 2005
data <- data[dead==F | death_year>=2005]

# Add dummy for in-migration
data[,has_come:=fifelse(is.na(mig_year_in),F,T)]

# Drop those that moved in after 2000
data <- data[has_come==F | mig_year_in<2000]

# Add dummy for out-migration
data[,has_left:=fifelse(is.na(mig_year_out),F,T)]

# Drop those that moved out before 2005
data <- data[has_left==F | mig_year_out>=2005]

# In addition to the sample variables, we code the origin variable
# to distinguish both by nativity and background
data[
  ,
  origin:=case_when(
    origin==11 ~ 'Native-Finn',
    origin==12 ~ 'Foreign-Finn',
    origin==21 ~ 'Native-Foreign',
    origin==22 ~ 'Foreign-Foreign'
    )]

# Are all the foreign-born immigrants? They should be.
table(data$origin,data$has_come)
# Yes, good.

# Does the number of records decrease with stat-year?
table(data$stat_year)
# Yes, good, because no one should be allowed to join the sample later.

# Bring in alternative SES measure
ses_data <- read_feather(here(out_dir,'010_ses_data.arrow'))
setDT(ses_data)
ses_data[,ses:=haven::as_factor(ses)]

data <- merge(data,ses_data,by=c('id','stat_year'),all.x=T)

rm(ses_data)
gc()

# Bring in health_data
health_data <- read_feather(here(out_dir,'007_health_data.arrow'))
setnames(health_data,'diagnosed','ever_diagnosed')
health_data[,diagnosis_year:=year(diagnosis_date)]
health_data[,ever_diagnosed:=as.integer(ever_diagnosed)]
health_data[,diagnosis_date:=NULL]

# Turn to wide
health_data_wide <- dcast(
  health_data,
  id ~ condition,
  value.var = c('ever_diagnosed','diagnosis_year')
)

rm(health_data)
gc()

data <- merge(data,health_data_wide,by=c('id'),all.x=T)

rm(health_data_wide)
gc()

# Set correct missing values
conditions <- c('diabetes','neoplasm','heart_disease','arthritis','asthma','hypertension','mental_disorder')

diagnosis_vars <- sapply(conditions,function(x) paste0('ever_diagnosed_',x))
setnafill(data,type='const',fill=0,cols=diagnosis_vars)

diagnosis_year_vars <- sapply(conditions,function(x) paste0('diagnosis_year_',x))
setnafill(data,type='const',fill=2100,cols=diagnosis_year_vars)

# Set country codes
# Get file with country names
country_codes <- fread(here('data','country_codes.txt'))

# Get country groups
data <- country_codes[,.(country_birth_in=country,group_in=group)][data,on=.(country_birth_in)]
data <- country_codes[,.(country_arrival=country,group_out=group)][data,on=.(country_arrival)]

# Set country group as factor and put Sweden first
data[,group_in:=fct_relevel(group_in,'Sweden')]
data[,group_out:=fct_relevel(group_out,'Sweden')]

# Let us compute age at in- and out-migration
data[,age_in:=age+(mig_year_in-stat_year)]
data[,age_out:=age+(mig_year_out-stat_year)]

# Let us combined some origin and destination groups to get slightly larger sample sizes
data[,group_in:=case_when(
  group_in=='Caribbean' | group_in=='Central America' ~ 'Central America and Caribbean',
  group_in=='East Asia' | group_in=='South East Asia' | group_in=='Oceania' ~ 'East Asia and Oceania',
  T ~ group_in
)]

data[,group_out:=case_when(
  group_out=='Caribbean' | group_out=='Central America' ~ 'Central America and Caribbean',
  group_out=='East Asia' | group_out=='South East Asia' | group_out=='Oceania' ~ 'East Asia and Oceania',
  T ~ group_out
)]

# Set SES status to factor
data[,ses:=factor(
  fcase(
    ses=='Upper WC', 'Upper white collar',
    ses=='Lower WC, managerial', 'Lower white collar',
    ses=='Lower WC, non-managerial', 'Lower white collar',
    ses=='Worker', 'Manual',
    ses=='Farmer', 'Manual',
    ses=='Self employed', 'Self-employed',
    ses=='Student', 'Student',
    ses=='Other', 'Other',
    rep(T,.N), 'Unknown'
  )
)]

# Keep only observations from 2004 to 2019
data <- data[data.table::between(stat_year,2004,2019)]
# Drop observations after death (very few)
nrow(data[death_year<=stat_year & !is.na(death_year)])
data <- data[stat_year<=death_year | is.na(death_year)]
# There will be cases in which the last out-migration comes before the last
# in-migration. This means the individual left but then came back and we don't
# see them leave again, in this case, we should set the out-migration info
# as missing.
data[
  mig_date_out<=mig_date_in & !is.na(mig_date_in) & !is.na(mig_date_out),
  `:=`(group_out=NA,country_birth_out=NA,mig_date_out=NA,mig_year_out=NA,
       country_arrival=NA,municipality_departure=NA)
  ]
# Drop observations after out-migration
nrow(data[mig_year_out<=stat_year & !is.na(mig_year_out)])
data <- data[stat_year<=mig_year_out | is.na(mig_year_out)]

# Indicators for out-migration or death in the follow-up
data[,dead_follow_up:=(!is.na(death_year) & data.table::between(death_year,2005,2019))]
data[,left_follow_up:=(!is.na(mig_year_out) & data.table::between(mig_year_out,2005,2019))]

# We miss a record for the year of death or outmigration. We will add it
# using the information from the previous year
# Identify cases in which the record is present
exception_ids <- unique(data[death_year==stat_year]$id)
data_death <- data[dead_follow_up==T & stat_year==(death_year-1) & !(id %chin% exception_ids)]
data_death[,stat_year:=stat_year+1]
data_death[,age:=age+1]

# Identify cases in which the record is present
exception_ids <- unique(data[mig_year_out==stat_year]$id)
data_outmig <- data[left_follow_up==T & stat_year==(mig_year_out-1) & !(id %chin% exception_ids)]
data_outmig[,stat_year:=stat_year+1]

# Create additional records
data <- rbindlist(list(data,data_death,data_outmig))

setorder(data,id,stat_year)

# See if we can get more information on SES from current SES
# Distinguish between those for whom we have no information and those
# that we always see out of the labor force or unemployed
data[,min_ses:=min(current_ses),by=.(id)] # This gets any ses below the unknown/not in labor force codes

data[,ses_full:=ses]
data[(ses_full=='Other' | ses_full=='Unknown') & (min_ses==99),ses_full:='Unknown']
data[(ses_full=='Other' | ses_full=='Unknown') & (min_ses==70 | min_ses==81 | min_ses==82),ses_full:='Never in labor force']
data[(ses_full=='Other' | ses_full=='Unknown') & min_ses==10,ses_full:='Manual']
data[(ses_full=='Other' | ses_full=='Unknown') & min_ses==20,ses_full:='Self-employed']
data[(ses_full=='Other' | ses_full=='Unknown') & (min_ses>30 & min_ses<40),ses_full:='Upper white collar']
data[(ses_full=='Other' | ses_full=='Unknown') & (min_ses>40 & min_ses<50),ses_full:='Lower white collar']
data[(ses_full=='Other' | ses_full=='Unknown') & (min_ses>50 & min_ses<60),ses_full:='Manual']
data[(ses_full=='Other' | ses_full=='Unknown') & min_ses==60,ses_full:='Student']

data[,ses_full:=fct_drop(ses_full)]

# Alright, so now we are allowing people to age into the sample and age out
# of the sample, which will break the nice declining sample size over time.
# But we don't want very young or very old individuals in the sample.

# Drop observation for individuals less than 25 years old.
nrow(data[age<25])
data <- data[age>=25]

# Drop observation for individual-years older than 85 years old.
nrow(data[age>=85])
data <- data[age<85]

# Set sex to factor
data[,sex:=factor(fifelse(sex==1,'Male','Female'))]

# Set marital status to factor
data[,marital_status:=factor(
  case_when(
    marital_status==1 ~ 'Never married',
    marital_status==2 ~ 'Married',
    marital_status==4 ~ 'Divorced',
    marital_status==5 ~ 'Widowed'
  )
)]

# Set employment to factor
data[,employment:=factor(
  case_when(
    employment==11 ~ 'Employed',
    employment==12 ~ 'Unemployed',
    employment==21 ~ 'Not in labor force',
    employment==22 ~ 'Not in labor force',
    employment==24 ~ 'Retired or receiving pension',
    employment==25 ~ 'Employed',
    employment==29 ~ 'Unemployed',
    employment==99 ~ 'Not in labor force',
  )
)]

# Compute quintile thresholds for household income
income_t_h <- quantile(data[origin=='Native-Finn' & stat_year==2005]$disposable_income_h,na.rm=T,probs=c(0.2,0.4,0.6,0.8))

data[,income_q_h:=case_when(
  disposable_income_h <= income_t_h[1] ~ '1st',
  disposable_income_h > income_t_h[1] & disposable_income_h <= income_t_h[2] ~ '2nd',
  disposable_income_h > income_t_h[2] & disposable_income_h <= income_t_h[3] ~ '3rd',
  disposable_income_h > income_t_h[3] & disposable_income_h <= income_t_h[4] ~ '4th',
  disposable_income_h > income_t_h[4] ~ '5th',
)]

data[is.na(income_q_h),income_q_h:='Unknown']

# Get a sense of median incomes by quintile
data[,.(median_income_q=median(disposable_income_h)),by=.(income_q_h)]

# Repeat but this time dividing by consumption units to control for household
# size (though I am not sure if this would cause a bias and in which direction)
data[,disposable_income_i:=disposable_income_h/cons_units]
# Compute quintile thresholds for adjusted household income
income_t_i <- quantile(data[origin=='Native-Finn' & stat_year==2005]$disposable_income_i,na.rm=T,probs=c(0.2,0.4,0.6,0.8))

# Let us create quintiles of adjusted household disposable income
data[,income_q_i:=case_when(
  disposable_income_i <= income_t_i[1] ~ '1st',
  disposable_income_i > income_t_i[1] & disposable_income_i <= income_t_i[2] ~ '2nd',
  disposable_income_i > income_t_i[2] & disposable_income_i <= income_t_i[3] ~ '3rd',
  disposable_income_i > income_t_i[3] & disposable_income_i <= income_t_i[4] ~ '4th',
  disposable_income_i > income_t_i[4] ~ '5th',
)]

data[is.na(income_q_i),income_q_i:='Unknown']

# Get a sense of median incomes by quintile
data[,.(median_income_q=median(disposable_income_i)),by=.(income_q_i)]

# Create regions of birth and fix country of birth for natives
data[origin=='Native-Finn',group_in:='Finland']

data[
  ,
  pob:=fcase(
    group_in %chin% c('Central America and Caribbean','South America','North America'),'Americas',
    group_in %chin% c('North Africa','Sub-Saharan Africa'),'Africa',
    group_in %chin% c('South Asia','East Asia and Oceania','China','Central Asia'),'Asia and Oceania',
    group_in %chin% c('Central Europe','Southern Europe','Western Europe','Eastern Europe','Balkans','Estonia','Northern Europe','Sweden'),'Europe',
    rep(TRUE,.N), group_in
  )
]

# Create dummy for children below 18 in the household
data[,child_b18:=case_when(child_b18==0~'None',child_b18>0~'1+',T~'Unknown')]

# Sample 200,000 Finns
set.seed(42)
data_foreign <- data[origin=='Foreign-Foreign']
data_native <- data[id %chin% sample(unique(data[origin=='Native-Finn']$id),size=200000,replace = F)]

data_sample <- rbindlist(list(data_foreign,data_native))

# Save data
write_feather(data_sample,here(out_dir,'011_general_model_data.arrow'))

table(data_sample[stat_year==2004]$ses_full,data_sample[stat_year==2004]$origin,useNA = 'always')

