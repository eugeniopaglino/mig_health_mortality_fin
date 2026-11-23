# Loading packages
library(here)
library(gt)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/209_cox_ses_stratified_tables.R')

in_dir <- here('input')
out_dir <- here('output')
fig_dir <- here('figures')

coef_tables <- fread(here(out_dir,'111_coef_table.csv'))

setorder(coef_tables,sex)

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
cols_to_delete <- c('term','std.error','statistic','p.value','id',
                    'estimate','estimate_str',
                    'conf.low','conf_low_str',
                    'conf.high','conf_high_str')
coef_tables[,(cols_to_delete):=NULL]

# Reorder columns
setcolorder(coef_tables,c('sex','ses_full'))

# Save
coef_tables |>
  group_by(sex) |>
  gt() |>
  cols_label(
    ses_full='Occupational status',
    coef_summary='Foreign-born hazard ratio'
  ) |>
  gtsave(filename = here('tables','209_cox_ses_stratified_table.docx'))

