# Loading packages
library(here)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/080_read_basic_data.R')

in_dir <- 'D:/ready-made'
out_dir <- here('output')

data <- read_feather(here(out_dir,'030_combined_data.arrow'))

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
      sex=sukup,
      age=ika,
      birth_year=syntyv,
      death_year=kuolv,
      country_birth=svaltio_k,
      residence_region=mkunta,
      residence_municipality=kunta,
      origin=syntyp2,
      marital_status=sivs,
      ses=sose,
      edu=ututku_aste,
      employment=ptoim1,
      child_b18=a18lkm_k,
      income=palk_k
    )
  )
  
  setDT(chunk)
  chunk
}

clean.chunk <- function(chunk) {
  setnames(
    chunk,
    c('shnro','vuosi','sukup','ika',
      'syntyv','kuolv',
      'svaltio_k','mkunta','kunta','syntyp2',
      'sivs','sose','ututku_aste','ptoim1','a18lkm_k','palk_k'),
    c("id","stat_year","sex","age",
      "birth_year","death_year",
      "country_birth","residence_region","residence_municipality",'origin',
      'marital_status',"ses",'edu','employment','child_b18','income')
  )
  
  chunk <- chunk[id %chin% sample_ids]
  
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

files <- list.files(
  file.path(in_dir,'FOLK_perus_0110a'),
  pattern = '.dta',
  full.names = T
)

files_dates <-  file.info(files)$mtime
file <- files[which.max(files_dates)]

data_01_10 <- read.dta.batched(file,stop = 80E6)

files <- list.files(
  file.path(in_dir,'FOLK_perus_11a'),
  pattern = '.dta',
  full.names = T
)

files_dates <-  file.info(files)$mtime
file <- files[which.max(files_dates)]

data_11_20 <- read.dta.batched(file,stop = 80E6)

data <- rbindlist(list(data_01_10,data_11_20))

write_feather(data,here(out_dir,'080_basic_data.arrow'))
