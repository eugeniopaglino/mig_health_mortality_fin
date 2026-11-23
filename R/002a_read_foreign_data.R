# Loading packages
library(here)
library(arrow)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/002a_read_foreign_data.R')

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
  
  chunk <- chunk[origin==22 & (id %chin% sample_ids)]
  
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

# Bring in migration histories
mig_histories <- read_feather(here(out_dir,'001_mig_histories.arrow'))
setDT(mig_histories)

sample_ids <- unique(mig_histories[!is.na(mig_year_in) & mig_year_in>=1990 & mig_year_in<=2000]$id)

# We only need data for foreign-born residents with a migration record, and
# we will extract data for the year 5 year after immigration. For example,
# we will use data for 1995 for someone who arrived in 1990. This should 
# increase the sample size, definitely in terms of person-years, but
# also in terms of individuals since we can keep people who, say, arrived in 1990
# but left in 2000, which would be excluded with a fixed starting year for
# everyone.

data <- read.dta.batched(files,start = 35E6, stop = 80E6)

# Merge with data
data_select <- merge(data,mig_histories,by='id',all.x=T)

# Keep only individuals who arrived between 1990 and 2000.
# Note that no individual should have a migration year after 2000,
# otherwise they could not be in the 2000 file, but it might be that
# their migration record is missing. We also need to drop those without
# a migration record

# Remove because missing migration record (should be 0)
sum(is.na(data_select$mig_year_in))
data_select <- data_select[!is.na(mig_year_in)]

# Remove because arrived before 1990 (should be 0)
nrow(data_select[mig_year_in<1990])
data_select <- data_select[mig_year_in>=1990]

# Remove because arrived after 2000 (should be 0)
nrow(data_select[mig_year_in>2000])
data_select <- data_select[mig_year_in<=2000]

# Select the last available stat_year for each individual
data_select <- data_select[data_select[,.I[stat_year==max(stat_year)], by=.(id)]$V1]

# Drop those who stayed fewer than 5 years, because we don't have enough data to
# understand whether they are disease free
nrow(data_select[(mig_year_out-mig_year_in)<5 & !is.na(mig_year_out)])
data_select <- data_select[(mig_year_out-mig_year_in)<5 | is.na(mig_year_out)]

write_feather(data_select,here(out_dir,'002_foreign_data.arrow'))
