# Loading packages
library(here)
library(arrow)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/005_clean_visits.R')

out_dir <- here('output')

data <- read_feather(here(out_dir,'003_combined_data.arrow'))

sample_ids <- unique(data$id)
rm(data)
gc()

visits <- read_feather(here(out_dir,'004_visits.arrow'),col_select = c('id','date_of_visit','main_ICD10','main_ICD9'))
setDT(visits)

visits <- visits[id %chin% sample_ids]
  
visits[,date_time:=parse_date_time(date_of_visit,'%d.%m.%Y %H:%M')]
visits[,month_of_visit:=month(date_time)]
visits[,year_of_visit:=year(date_time)]
visits[,date_simple:=make_date(year_of_visit,month_of_visit)]
  
visits[,main_ICD10_2:=str_sub(main_ICD10,1,3)]
visits[,main_ICD9_3:=str_sub(main_ICD9,1,3)]
  
write_feather(visits,here(out_dir,'005_visits_data.arrow'))
