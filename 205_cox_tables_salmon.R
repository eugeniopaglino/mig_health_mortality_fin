# Loading packages
library(here)
library(gt)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/205_cox_tables_salmon.R')

in_dir <- here('input')
out_dir <- here('output')
fig_dir <- here('figures')

coef_tables_all <- fread(here(out_dir,'107_coef_table.csv'))
coef_tables_all[,migration_type:='All']
coef_tables_return <- fread(here(out_dir,'108_coef_table.csv'))
coef_tables_return[,migration_type:='Return']
coef_tables_secondary <- fread(here(out_dir,'109_coef_table.csv'))
coef_tables_secondary[,migration_type:='Secondary']

# Combine
coef_tables <- rbindlist(list(coef_tables_all,coef_tables_return,coef_tables_secondary))

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
    'Arthritis','Asthma',
    'Diabetes','Heart disease','Hypertension',
    'Neoplasm'
  )
]

setorder(coef_tables,migration_type,condition,model_name,sex,term)

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
  migration_type + sex + model_name + term ~ condition,
  value.var = c('coef_summary')
)

# Just keep even diagnosed
coef_tables <- coef_tables[term == 'Ever Diagnosed']

# Save
coef_tables |>
  group_by(sex,migration_type) |>
  gt() |>
  cols_label(
    sex='',
    model_name='Model Type',
    term=''
  ) |>
  gtsave(filename = here('tables','205_cox_salmon_table.docx'))
