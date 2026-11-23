# Loading packages
library(here)
library(arrow)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/012_health_eligibility.R')

in_dir <- here('data')
out_dir <- here('output')

health_data <- read_feather(here(out_dir,'007_health_data.arrow'))
setDT(health_data)

health_data[,eligible:=0]
health_data[diagnosis_date<make_date(2000,01,01) | diagnosis_date>make_date(2004,12,31),eligible:=1]

health_data[,diagnosed:=0]
health_data[diagnosis_date<make_date(2100,01,01),diagnosed:=1]

health_data[,.(prop=sum(eligible)/.N),by=.(condition)]
health_data[,.(prop=sum(diagnosed)/.N),by=.(condition)]

health_data[diagnosed==1,.(prop=sum(eligible)/.N),by=.(condition)]

write_feather(health_data,here(out_dir,'012_health_eligibility.arrow'))
