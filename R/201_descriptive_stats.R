# Loading packages
library(here)
library(arrow)
library(gt)
library(gtsummary)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/201_descriptive_stats.R')

in_dir <- here('input')
out_dir <- here('output')
fig_dir <- here('figures')
tab_dir <- here('tables')

data <- read_feather(here(out_dir,'011_general_model_data.arrow'))
setDT(data)

data <- data[stat_year==2004]

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
  'Upper white collar',
  'Lower white collar',
  'Manual',
  'Self-employed',
  'Student',
  'Never in labor force',
  'Unknown'
)]

# Create age groups
data[
  ,
  age_group:=fcase(
    between(age,25,44),'Early adulthood (25-44)',
    between(age,45,64),'Middle adulthood (45-64)',
    age>=65,'Late adulthood (65-84)'
  )
]
data[,age_group:=fct_relevel(age_group,'Early adulthood (25-44)','Middle adulthood (45-64)')]

# Combined Asia, Oceania, and Americas
data[
  ,
  pob:=fcase(
    pob %chin% c('Americas','Asia and Oceania'),'Americas, Asia, and Oceania',
    rep(TRUE,.N), pob
  )
]

# Set country of birth to Finland for all the Finnish-born
data[origin=='Native-Finn',country_birth_in:='Finland']

# Check if composition changes a lot with age
origin_table <- data[pob!='Finland',.N,by=.(pob,age_group)][,prop:=N/sum(N),by=.(age_group)]
dcast(
  origin_table,
  pob ~ age_group,
  value.var = 'prop'
)

# Differentiate between secondary and return migration
data[left_follow_up==0,mig_type_follow_up:='Stayed']
data[left_follow_up==1 & country_arrival == country_birth_in,mig_type_follow_up:='Return migrant']
data[left_follow_up==1 & country_arrival != country_birth_in,mig_type_follow_up:='Secondary migrant']
data[left_follow_up==1 & is.na(country_arrival),mig_type_follow_up:='Unknown destination']

# Assess correlation between SES measures

cramers_v <- function(x,y) {
  # 1. filter out missing
  valid <- complete.cases(x,y)
  x_clean <- x[valid]
  y_clean <- y[valid]
  n <- length(x_clean)
  
  # 2. Determine matrix dimension
  r <- length(unique(x_clean))
  c <- length(unique(y_clean))
  
  cat('Table with ',r,' rows and ',c,' columns')
  # 3. Compute chi-square statistics
  chi_sq <- suppressWarnings(
    chisq.test(
      x_clean,
      y_clean,
      correct = F
    )$statistic
  )
  
  # 4. Calculate Cramer's V
  v <- sqrt(chi_sq / (n * min(r-1,c-1)))
  
  return(as.numeric(v))
}

data[,cramers_v(ses_full,employment)]
data[,cramers_v(ses_full,income_q_h)]
data[,cramers_v(employment,income_q_h)]

# DESCRIPTIVE STATISTICS

# Create table with basic info
tbl_summary(
  data,
  by = origin,
  include = c(c('age','age_group','sex','employment','ses_full','income_q_h',
                'pob','mig_type_follow_up','dead_follow_up')),
  statistic = list(
    all_continuous() ~ "{median} ({min}, {max})", 
    all_categorical() ~ "{n} ({p}%)"
    )
  ) |>
  as_gt() |>
  gt::gtsave(filename = here(tab_dir,'201_descriptive_stats.docx'))
