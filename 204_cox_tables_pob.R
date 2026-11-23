# Loading packages
library(here)
library(gt)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/204_cox_tables_pob.R')

in_dir <- here('input')
out_dir <- here('output')
fig_dir <- here('figures')

mortality_tables <- fread(here(out_dir,'103_coef_table.csv'))
health_tables <- fread(here(out_dir,'104_coef_table.csv'))

mortality_tables[,condition:='death']

coef_tables <- rbindlist(list(mortality_tables,health_tables))

# Remove mental disorders
coef_tables <- coef_tables[condition!='mental_disorder']

# Clean condition names
coef_tables[,condition:=str_to_sentence(str_replace(condition,'_',' '))]

# Clean model names
coef_tables[
  ,
  model_name:=fcase(
    id==1 | id==2,'Baseline',
    id==3 | id==4,'Full'
  )
]

# Create sex variable
coef_tables[,sex:='Female']
coef_tables[id==2 | id==4,sex:='Male']

coef_tables[
  ,
  condition:=fct_relevel(
    condition,
    'Death',
    'Arthritis','Asthma',
    'Diabetes','Heart disease','Hypertension',
    'Neoplasm'
  )
]

setorder(coef_tables,condition,model_name,sex,term)

create.summary <- function(x) {
  x_round <- as.character(round(x,2))
  x_round <- fifelse(nchar(x_round)==1,paste0(x_round,'.'),x_round)
  x_formatted <- str_pad(x_round,width = 4,pad='0',side = 'right')
  
}

# Create tidy estimate column. Let us do a nice job and add padding with zeros.
coef_tables[,estimate_str:=create.summary(estimate)]
coef_tables[,conf_low_str:=create.summary(conf.low)]
coef_tables[,conf_high_str:=create.summary(conf.high)]
coef_tables[,coef_summary:=paste0(estimate_str,' (',conf_low_str,', ',conf_high_str,')')]

# Remove unnecessary variables
cols_to_delete <- c('std.error','statistic','p.value','id',
                    'estimate','estimate_str',
                    'conf.low','conf_low_str',
                    'conf.high','conf_high_str')
coef_tables[,(cols_to_delete):=NULL]

# Reorder columns
setcolorder(coef_tables,c('sex','model_name','condition','term'))

# Convert to wide
coef_tables <- dcast(
  coef_tables,
  sex + model_name + term ~ condition,
  value.var = c('coef_summary')
)

# Just keep place of birth and sort
coef_tables <- coef_tables[term %chin% c("Africa","Americas, Asia, and Oceania","Europe","Middle East","Russia")]
coef_tables <- coef_tables[
  ,
  term:=fct_relevel(
    term,
    "Africa","Americas, Asia, and Oceania",
    "Europe","Middle East","Russia"
    )]

# Split by sex
female_tables <- coef_tables[sex=='Female']
female_tables[,sex:=NULL]
male_tables <- coef_tables[sex=='Male']
male_tables[,sex:=NULL]

# Save
female_tables |>
  group_by(model_name) |>
  gt() |>
  cols_label(
    term=''
  ) |>
  gtsave(filename = here('tables','204_cox_pob_table_female.docx'))

male_tables |>
  group_by(model_name) |>
  gt() |>
  cols_label(
    term=''
  ) |>
  gtsave(filename = here('tables','204_cox_pob_table_male.docx'))
