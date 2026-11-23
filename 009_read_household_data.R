# Loading packages
library(here)
library(arrow)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/009_read_household_data.R')

in_dir <- 'D:/ready-made'
out_dir <- here('output')

data <- read_feather(here(out_dir,'003_combined_data.arrow'))

sample_ids <- unique(data$id)
rm(data)
gc()

read.chunk <- function(path,start_row,end_row) {
  chunk <- haven::read_dta(
    path,
    skip = start_row-1,
    n_max = (end_row-start_row+1),
    col_select = c(
      id=shnro,
      stat_year=vuosi,
      household_size=akoko,
      disposable_income_h=kturaha_ak,
      cons_units=modoecd,
      income_decile_h=desiili
    )
  )
  
  setDT(chunk)
  chunk
}

clean.chunk <- function(chunk) {
  setnames(
    chunk,
    c('shnro','vuosi',
      'akoko','kturaha_ak','modoecd','desiili'),
    c('id','stat_year',
      'household_size','disposable_income_h','cons_units','income_decile_h')
  )
  
  chunk <- chunk[id %chin% sample_ids]
  
  chunk
}

read.clean.chunk <- function(path,start_row,end_row) {
  chunk <- read.chunk(path,start_row,end_row)
  chunk <- clean.chunk(chunk)
  chunk
}

read.dta.batched <- function(path,batch_size=1E6,stop=3E6) {
  start_rows <- seq(1,stop,batch_size)
  end_rows <- start_rows + batch_size -1
  
  n_batches <- length(start_rows)
  
  chunks <- vector('list',n_batches)
  pb <- progress::progress_bar$new(
    format='reading [:bar] :percent in :elapsed',
    total=n_batches, clear=F, width=60
  )
  
  for (i in 1:n_batches) {
    chunks[[i]] <- read.clean.chunk(path,start_rows[i],end_rows[i])
    pb$tick()
  }
  
  data <- data.table::rbindlist(chunks)
  data
}

files <- list.files(
  file.path(in_dir,'FOLK_askun_0110a'),
  pattern = '.dta',
  full.names = T
)

files_dates <-  file.info(files)$mtime
file <- files[which.max(files_dates)]

data_01_10 <- read.dta.batched(file,stop = 80E6)

files <- list.files(
  file.path(in_dir,'FOLK_askun_11a'),
  pattern = '.dta',
  full.names = T
)

files_dates <-  file.info(files)$mtime
file <- files[which.max(files_dates)]

data_11_20 <- read.dta.batched(file,stop = 80E6)

data <- rbindlist(list(data_01_10,data_11_20))

write_feather(data,here(out_dir,'009_household_data.arrow'))
