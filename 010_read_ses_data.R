# Loading packages
library(here)
library(arrow)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/010_read_ses_data.R')

out_dir <- here('output')

# Load data to get sample ids
data <- read_feather(here(out_dir,'003_combined_data.arrow'))

sample_ids <- unique(data$id)
rm(data)
gc()

# Bring in alternative SES measure

read.chunk <- function(path,start_row,end_row) {
  chunk <- haven::read_dta(
    path,
    skip = start_row-1,
    n_max = (end_row-start_row+1),
    col_select = c(
      id=shnro,
      stat_year=vuosi,
      ses=sesx
    )
  )
  
  setDT(chunk)
  chunk
}

clean.chunk <- function(chunk) {
  setnames(
    chunk,
    c('shnro','vuosi','sesx'),
    c('id','stat_year','ses')
  )
  
  chunk <- chunk[id %chin% sample_ids & stat_year>=2004]
  
  chunk
}

read.clean.chunk <- function(path,start_row,end_row) {
  chunk <- read.chunk(path,start_row,end_row)
  chunk <- clean.chunk(chunk)
  chunk
}

read.dta.batched <- function(path,batch_size=1E6,start=1,stop=3E6) {
  start_rows <- seq(start,stop,batch_size)
  end_rows <- start_rows + batch_size -1
  
  n_batches <- length(start_rows)
  
  chunks <- vector('list',n_batches)
  pb <- txtProgressBar(max=n_batches)
  
  for (i in 1:n_batches) {
    chunks[[i]] <- read.clean.chunk(path,start_rows[i],end_rows[i])
    setTxtProgressBar(pb,i)
  }
  
  close(pb)
  
  data <- data.table::rbindlist(chunks)
  data
}

ses_data <- read.dta.batched(here('data','retroses.dta'),stop = 180E6)

setorder(ses_data,id,stat_year)

write_feather(ses_data,here(out_dir,'010_ses_data.arrow'))
