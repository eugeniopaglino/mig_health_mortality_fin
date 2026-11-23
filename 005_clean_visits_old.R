# Loading packages
library(here)
library(arrow)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/050_clean_visits.R')

out_dir <- here('output')

read.chunk <- function(path,start_row,end_row) {
  chunk <- fread(
    path,
    skip = start_row-1,
    nrows = (end_row-start_row+1),
    col.names=c('id','date_of_visit','main_ICD10','main_ICD9')
  )
  
  setDT(chunk)
  chunk
}

clean.chunk <- function(chunk,ids) {
  
  chunk <- chunk[id %chin% ids]
  
  chunk[,date_time:=parse_date_time(date_of_visit,'%d.%m.%Y %H:%M')]
  chunk[,month_of_visit:=month(date_time)]
  chunk[,year_of_visit:=year(date_time)]
  chunk[,date_simple:=make_date(year_of_visit,month_of_visit)]
  
  chunk[,main_ICD10_2:=str_sub(main_ICD10,1,3)]
  chunk[,main_ICD9_3:=str_sub(main_ICD9,1,3)]
  
  chunk
  
}

read.clean.chunk <- function(path,start_row,end_row,ids) {
  chunk <- read.chunk(path,start_row,end_row)
  chunk <- clean.chunk(chunk,ids)
  chunk
}

read.data.batched <- function(path,batch_size=1E6,stop=250E6,ids) {
  start_rows <- seq(1,stop,batch_size)
  end_rows <- start_rows + batch_size -1
  
  n_batches <- length(start_rows)
  
  chunks <- vector('list',n_batches)
  pb <- progress::progress_bar$new(
    format='reading [:bar] :percent in :elapsed',
    total=n_batches, clear=F, width=60
  )
  
  for (i in 1:n_batches) {
    chunks[[i]] <- read.clean.chunk(path,start_rows[i],end_rows[i],ids)
    pb$tick()
  }
  
  data <- data.table::rbindlist(chunks)
  data
}

data <- read_feather(here(out_dir,'03_combined_data.feather'))

sample_ids <- unique(data$id)
rm(data)
gc()

file_rows <- as.numeric(str_extract(shell('wc -l W:/Eugenio/international_migration/output/04_visits.csv', intern = T),'^\\d+'))

visits <- read.data.batched(here(out_dir,'04_visits.csv'),stop=file_rows,ids=sample_ids)

write_feather(visits,here(out_dir,'05_visits_data.feather'))
