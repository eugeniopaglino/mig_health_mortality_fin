# Loading packages
library(here)
library(arrow)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/003_combine_data.R')

in_dir <- 'D:/d31/external/u1259_al5_thl_aineisto_hilmot'
out_dir <- here('output')

data_foreign <- read_feather(here(out_dir,'002_foreign_data.arrow'))
setDT(data_foreign)
data_foreign[,time_in_fi:=NULL]

data_native <- read_feather(here(out_dir,'002_native_data.arrow'))
setDT(data_native)
data <- rbindlist(list(data_foreign,data_native),use.names = T)

write_feather(data,here(out_dir,'003_combined_data.arrow'))