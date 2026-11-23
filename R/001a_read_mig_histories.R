# Loading packages
library(here)
library(arrow)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/001a_read_mig_histories.R')

in_dir <- 'W:/Non-continuously updated folk modules'
out_dir <- here('output')

# Get list of files with migration records
files <- list.files(
  file.path(in_dir,'FOLK_MUUTT_C'),
  pattern = '.dta',
  full.names = T
)

# Define functions to read the files in chuck to save RAM

read.chunk <- function(path,start_row,end_row) {
  chunk <- haven::read_dta(
    path,
    skip = start_row-1,
    n_max = (end_row-start_row+1),
    col_select = c(
      id=shnro,
      mig_year=vuosi,
      mig_type=muuttolaji,
      mig_date=muuttopv,
      municipality_departure=lkunta,
      municipality_arrival=tkunta,
      code_departure=lahtomaakoodi,
      code_arrival=tulomaakoodi,
      marital_status=sivs_m,
      citizenship=kansa1_m,
      code_birth=svaltio_m
    )
  )
  
  setDT(chunk)
  chunk
}

clean.chunk <- function(chunk) {
  setnames(
    chunk,
    c('shnro','vuosi','muuttolaji','muuttopv',
      'lkunta','tkunta','lahtomaakoodi','tulomaakoodi',
      'sivs_m','kansa1_m','svaltio_m'),
    c("id","mig_year","mig_type","mig_date",
      "municipality_departure","municipality_arrival","code_departure","code_arrival",
      "marital_status","citizenship","code_birth")
  )
  
  chunk <- chunk[mig_type==41 | mig_type==42]
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

# Read data in chucks and reassemble
data <- rbindlist(lapply(files,read.dta.batched,stop = 80E6))

# Convert country codes to numeric
data[,code_departure:=as.integer(code_departure)]
data[,code_arrival:=as.integer(code_arrival)]
data[,code_birth:=as.integer(code_birth)]

# Get file with country names
country_codes <- fread(here('data','country_codes.txt'))

# Match codes to names
data <- country_codes[,.(code,country_departure=country)][data,on=.(code=code_departure)]
setnames(data,'code','code_departure')

data <- country_codes[,.(code,country_arrival=country)][data,on=.(code=code_arrival)]
setnames(data,'code','code_arrival')

data <- country_codes[,.(code,country_birth=country)][data,on=.(code=code_birth)]
setnames(data,'code','code_birth')

# Drop all the Finnish-born
data <- data[country_birth!='Finland']

# Drop all migration events after 2019
data <- data[mig_year <=2019]

# Create a variable with the total number of observations for each person
data[,nobs_ind:=.N,by=.(id)]

# Drop all those with more than 6 migration records are these are likely
# seasonal migrants
nrow(data[nobs_ind>6])
data <- data[nobs_ind<=6]

# Create an indicator for migration sequence
setorder(data,id,mig_year)
data[,mig_num:=(1:.N),by=.(id)]

# Create two separate indicators for in-migration and out-migration events
data[,mig_type_num:=(1:.N),by=.(id,mig_type)]

# Identify last in- and out-migration events
data[,last_event:=(mig_type_num==max(mig_type_num)),by=.(id,mig_type)]

# For every individual, keep only the last immigration and emigration
# records
data <- data[last_event==T]

# Get last immigration and last emigration date (note: if_else is extremely
# slow here so if there is a way to use a math trick like TRUE evaluating to 
# 1 and FALSE to 0 better to use it).
data[,last_in_date:=max(fifelse(mig_type==41,mig_date,as.Date(0))),by=.(id)]
data[,last_out_date:=max(fifelse(mig_type==42,mig_date,as.Date(0))),by=.(id)]

# Remove records corresponding to out-migration events that took place
# before the last in-migration
data <- data[!(mig_type==42 & last_in_date>last_out_date)]

# Let's run some checks. Get remaining number of obs for each individual, it
# should be 1 or 2:
data[,nrecords:=.N,by=.(id)]
data[,.(.N),by=.(nrecords)]

# Remove people we see emigrating but for whom we lack an immigration record
data <- data[!(nrecords==1 & mig_type==42)]

# Check that all individuals with one record now have an immigration record and
# that those with two records have one immigration and one emigration record
data[,.(.N),by=.(nrecords,mig_type)]

# Create separate tables for immigration and emigration records
data_in <- data[
  mig_type==41,
  .(id,country_birth_in=country_birth,
    mig_date_in=mig_date,mig_year_in=mig_year,
    country_departure,municipality_arrival
    )]

data_out <- data[
  mig_type==42,
  .(id,country_birth_out=country_birth,
    mig_date_out=mig_date,mig_year_out=mig_year,
    country_arrival,municipality_departure)
  ]

rm(data)

# Combine again to get final data
data <- merge(data_in,data_out,by='id',all = T)

# There are 562 individuals whose birth country does not match between their 
# immigration and emigration records. There is some useful information here
# because in some cases the emigration record is more precise (e.g. former 
# soviet union becomes estonia) but some cases are the opposite so there is
# no easy fix. We might be able to get the same information in the base files
# and in principle it might be more accurate for everyone.
nrow(data[,has_left:=!is.na(mig_date_out)])

# Let us look at the top 10 origin countries
in_mig_countries <- data_in[,.(nobs=.N),by=.(country_departure)]
setorder(in_mig_countries,-nobs)
in_mig_countries[1:10]

# Same for destinations
out_mig_countries <- data_out[,.(nobs=.N),by=.(country_arrival)]
setorder(out_mig_countries,-nobs)
out_mig_countries[1:10]

# We compute the number of days between in- and out-migration
data[,time_in_fi:=as.integer(difftime(mig_date_out,mig_date_in,units = 'days'))]
data[,time_in_fi:=fifelse(has_left==F,as.integer(difftime(as.Date('2019-12-31'),mig_date_in,units = 'days')),time_in_fi)]

# Average number of years spent in Finland
mean(data$time_in_fi/365)
# Much higher for those who stay
mean(data[has_left==F]$time_in_fi/365)
# Than for whose who leave
mean(data[has_left==T]$time_in_fi/365)

# At this point we can save the data. Aside from merging information on each
# individual from the basic files and the health variables, the data is ready 
# to be analysed.
write_feather(data,here(out_dir,'001_mig_histories.arrow'))
