# Loading packages
library(here)
library(gt)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/209_logit_tables.R')

in_dir <- here('input')
out_dir <- here('output')
fig_dir <- here('figures')

coef_tables <- fread(here(out_dir,'113_coef_table.csv'))

# Remove mental disorders
coef_tables <- coef_tables[condition!='mental_disorder']

# Clean condition names
coef_tables[,condition:=sapply(condition,function(x) str_replace(x,'_',' '))]
coef_tables[,condition:=str_to_sentence(condition)]

# Create sex variable
coef_tables[,sex:='Female']
coef_tables[id==2,sex:='Male']

# Set order
coef_tables[
  ,
  term:=fct_relevel(
    term,
    'International Immigrant',
    'International Immigrant X Aged 45-64',
    'International Immigrant X Aged 65-84'
  )
]

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

setorder(coef_tables,condition,sex,term)

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
setcolorder(coef_tables,c('sex','condition','term'))

# Convert to wide
coef_tables <- dcast(
  coef_tables,
  sex + term ~ condition,
  value.var = c('coef_summary')
)

# Save
coef_tables |>
  group_by(sex) |>
  gt() |>
  cols_label(
    term=''
  ) |>
  gtsave(filename = here('tables','209_logit_table.docx'))
