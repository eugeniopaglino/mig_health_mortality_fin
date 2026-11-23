# Loading packages
library(here)
library(arrow)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/006_read_med_reimbursements_data.R')

in_dir <- 'D:/d31/external/u1259_al5_kela_aineisto'
out_dir <- here('output')

data <- read_feather(here(out_dir,'003_combined_data.arrow'))

sample_ids <- unique(data$id)
rm(data)
gc()

# Load reimbursement data
data <- haven::read_dta(
  here(in_dir,'u1259_laakekorvausoikeud_korj_s.dta'))

setDT(data)

# Clean names
setnames(
  data,
  c("shnro","SK1","apvm","lpvm","diag"),
  c('id','disease_code','start','end','icd10')
)

# Keep only individuals in the sample
data <- data[id %chin% sample_ids]

# Set dates
data[,start:=make_date(str_sub(start,1,4),str_sub(start,5,6),1)]
data[,start_year:=year(start)]
data[,end:=make_date(str_sub(end,1,4),str_sub(end,5,6),1)]
data[,end_year:=year(end)]

# Filter post 1990 observations
data <- data[start_year>=1990]

# Create condition codes
data[,condition:=fcase(
  disease_code=='103','diabetes',
  disease_code %chin% c('115','116','117','118','128','130'),'neoplasm',
  disease_code %chin% c('201','206','207'),'heart_disease',
  disease_code=='202','arthritis',
  disease_code=='203','asthma',
  disease_code=='205','hypertension',
  default = 'other'
)]

# Filter conditions we do not care about
data <- data[condition!='other']

# Get first start data for each individual and each condition
health_data <- data[,.(start_date=min(start),start_year=min(start_year)),by=.(id,condition)]

write_feather(health_data,here(out_dir,'006_med_reimbursements_data.arrow'))
