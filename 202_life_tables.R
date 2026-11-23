# Loading packages
library(here)
library(arrow)
library(gt)
library(msm)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/202_life_tables.R')

in_dir <- here('input')
out_dir <- here('output')
fig_dir <- here('figures')

data <- read_feather(here(out_dir,'011_general_model_data.arrow'))
setDT(data)

# Remove 2004
data <- data[stat_year>2004]

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

# Create rates by sex, period, and origin
rates <- data[,.(Dx=sum(death),Ex=sum(py)),by=.(age,sex,origin,stat_year)]

# Add national rates
rates_nat <- data[,.(Dx=sum(death),Ex=sum(py)),by=.(age,sex,stat_year)]
rates_nat[,origin:='Total']

rates <- rbindlist(list(rates,rates_nat),use.names = T)

rates[,period:=cut(stat_year,seq(2005,2020,5),right=F)]
rates[,age_group:=cut(age,seq(25,85,5),right=F)]
rates[,x:=as.integer(str_match(age_group,'\\d+'))]

rates <- rates[,.(Dx=sum(Dx),Ex=sum(Ex)),by=.(x,sex,origin,period)]
setorder(rates,x)
rates[,Mx:=Dx/Ex]

# Create groups
rates[,group:=.GRP,by=.(sex,period,origin)]
groups_dict <- unique(rates[,.(group,sex,period,origin)])
lts <- vector(mode='list',length = max(rates$group))

for (i in 1:length(lts)) {
  these_rates <- rates[group==i]$Mx
  this_lt <- my_lt(these_rates,ages=seq(25,84,5))
  
  this_lt[,group:=rates$group[i]]
  
  lts[[i]] <- this_lt
  
}

lts <- rbindlist(lts)
lts <- merge(lts,groups_dict,by='group')

setorder(lts,sex,period,origin,x)

# This is good to have in a supplementary table
lts_table <- dcast(
  lts[x==25],
  period ~ sex + origin,
  value.var = c('ex')
)

# Add gaps
lts_table[,Female_Gap:=`Female_Foreign-Foreign`-`Female_Native-Finn`]
lts_table[,Male_Gap:=`Male_Foreign-Foreign`-`Male_Native-Finn`]

lts_table[,Female_Contrib:=`Female_Total`-`Female_Native-Finn`]
lts_table[,Male_Contrib:=`Male_Total`-`Male_Native-Finn`]

setcolorder(
  lts_table,
  c('period',
    'Female_Total','Female_Native-Finn','Female_Foreign-Foreign','Female_Gap','Female_Contrib',
    'Male_Total','Male_Native-Finn','Male_Foreign-Foreign','Male_Gap','Male_Contrib'
    )
  )

lts_table |>
  gt() |>
  cols_label(
    period='',
    `Female_Total`='Total',
    `Female_Native-Finn`='Native',
    `Female_Foreign-Foreign`='Foreign',
    `Female_Gap`='Foreign Advantage',
    `Female_Contrib`='Foreign Contribution',
    `Male_Total`='Total',
    `Male_Native-Finn`='Native',
    `Male_Foreign-Foreign`='Foreign',
    `Male_Gap`='Foreign Advantage',
    `Male_Contrib`='Foreign Contribution',
  ) |>
  fmt_number(
    decimals = 2
  ) |>
  tab_spanner(
    `Female_Total`:`Female_Contrib`,
    label='Females'
  ) |>
  tab_spanner(
    `Male_Total`:`Male_Contrib`,
    label='Males'
  ) |>
  gtsave(filename=here('tables','202_e25_85_by_year.docx'))

# Use data for full period for main table

# Create rates by sex, period, and origin
rates <- data[,.(Dx=sum(death),Ex=sum(py)),by=.(age,sex,origin)]

rates[,age_group:=cut(age,seq(25,85,5),right=F)]
rates[,x:=as.integer(str_match(age_group,'\\d+'))]

rates <- rates[,.(Dx=sum(Dx),Ex=sum(Ex)),by=.(x,sex,origin)]
setorder(rates,x)
rates[,Mx:=Dx/Ex]

# Create groups
rates[,group:=.GRP,by=.(sex,origin)]
groups_dict <- unique(rates[,.(group,sex,origin)])
mortality_lts <- vector(mode='list',length = max(rates$group))

for (i in 1:length(lts)) {
  these_rates <- rates[group==i]$Mx
  this_lt <- my_lt(these_rates,ages=seq(25,84,5))
  
  this_lt[,group:=rates$group[i]]
  
  mortality_lts[[i]] <- this_lt
  
}

mortality_lts <- rbindlist(mortality_lts)
mortality_lts <- merge(mortality_lts,groups_dict,by='group')

setorder(mortality_lts,sex,x)

# Recode for easy names
mortality_lts[,origin:=fifelse(origin=='Native-Finn','N','F')]
mortality_lts[,lx:=lx*100]

# This is good to have in a supplementary table
mortality_lts_table <- dcast(
  mortality_lts,
  x + sex ~ origin,
  value.var = c('lx','ex')
)

# Add gaps
mortality_lts_table[,ex_G:=ex_F-ex_N]
mortality_lts_table[,lx_G:=lx_F-lx_N]

setcolorder(mortality_lts_table,c('sex','x','ex_F','ex_N','ex_G','lx_F','lx_N','lx_G'))

mortality_lts_table |>
  group_by(sex) |>
  gt() |>
  cols_label(
    sex='',
    x='Age',
    ex_F='Foreign',
    ex_N='Native',
    ex_G='Foreign Advantage',
    lx_F='Foreign',
    lx_N='native',
    lx_G='Foreign Advantage'
  ) |>
  fmt_number(
    decimals = 2
  ) |>
  tab_spanner(
    ex_F:ex_G,
    label='Life expectancy'
  ) |>
  tab_spanner(
    lx_F:lx_G,
    label='% survived'
  ) |>
  gtsave(filename=here('tables','202_ex_lx.docx'))

mortality_lts[,condition:='death']

# Do the same for health. Is this right?

# Bring in data to select only eligible cases_full
eligibility_data <- read_feather(here(out_dir,'012_health_eligibility.arrow'))

conditions_data <- data.table(
  condition = unique(eligibility_data$condition)
)
conditions_data[,diagnosis_variable:=sapply(conditions_data$condition,function(x) paste0('diagnosis_year_',x))]

health_lts <- vector(mode='list',length = nrow(conditions_data))

for (c in 1:nrow(conditions_data)) {
  print(conditions_data$condition[c])
  this_data <- merge(data,eligibility_data[condition==conditions_data$condition[c],.(id,eligible)],by='id',all.x=T)
  
  # Set missing to eligible, since they have no hospital records
  this_data[is.na(eligible),eligible:=1]
  
  # Get only eligible (i.e. disease free in 2005)
  this_data <- this_data[eligible==1]
  # Censor all those who have been diagnosed
  this_data <- this_data[stat_year<=get(conditions_data$diagnosis_variable[c])]
  # Define diagnosis, death, and exit variables
  this_data[,diagnosis:=0]
  this_data[stat_year==get(conditions_data$diagnosis_variable[c]),diagnosis:=1]
  
  # Compute person-years
  this_data[,py:=fifelse(diagnosis==1 | death==1 | exit==1,0.5,1)]
  
  # Direct estimation
  rates <- this_data[,.(Dx=sum(diagnosis),Ex=sum(py)),by=.(age,sex,origin)]
  
  rates[,age_group:=cut(age,seq(25,85,5),right=F)]
  rates[,x:=as.integer(str_match(age_group,'\\d+'))]
  
  rates <- rates[,.(Dx=sum(Dx),Ex=sum(Ex)),by=.(x,sex,origin)]
  setorder(rates,x)
  rates[,Mx:=Dx/Ex]
  
  # Create groups
  rates[,group:=.GRP,by=.(sex,origin)]
  groups_dict <- unique(rates[,.(group,sex,origin)])
  lts <- vector(mode='list',length = max(rates$group))
  
  for (i in 1:length(lts)) {
    these_rates <- rates[group==i]$Mx
    this_lt <- my_lt(these_rates,ages=seq(25,84,5))
    
    this_lt[,group:=rates$group[i]]
    
    lts[[i]] <- this_lt
    
  }
  
  lts <- rbindlist(lts)
  lts[,condition:=conditions_data$condition[c]]
  lts <- merge(lts,groups_dict,by='group')
  health_lts[[c]] <- copy(lts)
  
}

health_lts <- rbindlist(health_lts)

setorder(health_lts,sex,origin,condition,x)

# Recode for easy names
health_lts[,origin:=fifelse(origin=='Native-Finn','N','F')]
health_lts[,lx:=lx*100]

all_lts <- rbindlist(list(mortality_lts,health_lts),use.names = T)

all_lts[,sex:=fifelse(sex=='Female','F','M')]

setorder(all_lts,sex,origin,condition,x)

# This is good to have in a supplementary table
all_lts_table <- dcast(
  all_lts,
  x + condition ~ sex + origin,
  value.var = c('lx')
)

# Add gaps
all_lts_table[,F_G:=F_F-F_N]
all_lts_table[,M_G:=M_F-M_N]

setcolorder(all_lts_table,c('condition','x','F_F','F_N','F_G','M_F','M_N','M_G'))

# Pretty and sorted condition names
all_lts_table[
  ,
  condition:=str_to_sentence(
    str_replace(condition,'_',' ')
  )
]
all_lts_table <- all_lts_table[condition!='Mental disorder']
all_lts_table[
  ,
  condition:=fct_relevel(
    condition,
    'Death',
    'Arthritis','Asthma',
    'Diabetes','Heart disease','Hypertension',
    'Neoplasm'
  )
]

setorder(all_lts_table,condition,x)

all_lts_table[x %in% c(45,65,80)] |>
  group_by(condition) |>
  gt() |>
  cols_label(
    condition='',
    x='Age',
    F_F='Foreign',
    F_N='Native',
    F_G='Foreign Advantage',
    M_F='Foreign',
    M_N='native',
    M_G='Foreign Advantage'
  ) |>
  fmt_number(
    columns = -x,
    decimals = 2
  ) |>
  tab_spanner(
    F_F:F_G,
    label='Females'
  ) |>
  tab_spanner(
    M_F:M_G,
    label='Males'
  ) |>
  gtsave(filename=here('tables','202_survival_by_condition.docx'))

  