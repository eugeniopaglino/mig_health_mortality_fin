# Loading packages
library(here)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/002b_read_native_data.R')

in_dir <- 'D:/ready-made'
out_dir <- here('output')

files <- list.files(
  file.path(in_dir,'FOLK_perus_8800a'),
  pattern = '.dta',
  full.names = T
)

files_dates <-  file.info(files)$mtime
files <- files[which.max(files_dates)]

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

clean.chunk <- function(chunk,end_year) {
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
  
  chunk <- chunk[stat_year==1995 & origin==11]
  
  chunk
}

read.clean.chunk <- function(path,start_row,end_row) {
  chunk <- read.chunk(path,start_row,end_row)
  chunk <- clean.chunk(chunk)
  chunk
}

read.dta.batched <- function(path,end_year,batch_size=1E6,start=1,stop=3E6) {
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

data <- read.dta.batched(files,start=65E5,stop = 80E6)

# Bring in migration histories
mig_histories <- read_feather(here(out_dir,'001_mig_histories_native.arrow'))
setDT(mig_histories)

# Merge with data
data_select <- merge(data,mig_histories,by='id',all.x=T)

# Keep only individuals who arrived before 1995.
# Note that no individual should have a migration year after 1995,
# otherwise they could not be in the 1995 file, but it might be that
# their migration record is missing.

# Remove because arrived after 1995
nrow(data_select[!is.na(mig_year_in) & mig_year_in>1995])
data_select <- data_select[is.na(mig_year_in) | mig_year_in<=1995]

# Drop those who left before 2000, because we don't have enough data to
# understand whether they are disease free
nrow(data_select[mig_year_out<2000 & !is.na(mig_year_out)])
data_select <- data_select[mig_year_out>=2000 | is.na(mig_year_out)]

write_feather(data_select,here(out_dir,'002_native_data_all.arrow'))

# Keep just 500k
data_sample <- data_select[sample(1:.N,size=500E3)]

write_feather(data_sample,here(out_dir,'002_native_data.arrow'))

