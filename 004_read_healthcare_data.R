# Loading packages
library(here)
library(arrow)
library(data.table)
library(tidyverse)

rm(list=ls())

i_am('R/004_read_healthcare_data.R')

in_dir <- 'D:/d31/external/u1259_al5_thl_aineisto_hilmot'
out_dir <- here('output')

files <- list.files(
  file.path(in_dir),
  pattern = '^u1259_hilmo_paa_[0-9_a-z]+.dta$',
  full.names = T
  )

# Remove older files
files <- files[files!="D:/d31/external/u1259_al5_thl_aineisto_hilmot/u1259_hilmo_paa_1970_1986_s.dta"]
#files <- files[files!="D:/d31/external/u1259_al5_thl_aineisto_hilmot/u1259_hilmo_paa_1987_1993_s.dta"]
#files <- files[files!="D:/d31/external/u1259_al5_thl_aineisto_hilmot/u1259_hilmo_paa_9495_s.dta"]

# As timed below, it takes 1.9 minutes to read the first file, with 37 million
# rows, it is also not too large, 2.4 GB
#start_time <- Sys.time()
#test <- haven::read_dta(
#  files[1],
#  col_select = c(
#    id=shnro,
#    date_of_visit=tupva,
#    diagnosis_ICD10=pdge,
#    externl_cause_ICD10=ulksyy
#    )
#  )
#end_time <- Sys.time()
#end_time-start_time

chunk <- tryCatch(
  chunk <- haven::read_dta(
    files[1],
    n_max = 100,
    col_select = c(
      id=shnro,
      date_of_visit=tupva,
      main_ICD10=pdgo
    )
  ), 
  error = function(e) {
    chunk <- haven::read_dta(
      files[1],
      n_max = 100,
      col_select = c(
        id=shnro,
        date_of_visit=tupva,
        main_ICD10=pdg
      )
    )
  }, 
  finally = function(chunk) {
    setDT(chunk)
    chunk
  }
  )

chunk <- haven::read_dta(
  files[5],
  n_max = 100,
  col_select = c(
    id=shnro,
    date_of_visit=tupva,
    main_ICD10=pdg
  )
)

read.chunk <- function(path,start_row,end_row) {
  
  chunk <- tryCatch(
    chunk <- haven::read_dta(
      path,
      skip = start_row-1,
      n_max = (end_row-start_row+1),
      col_select = c(
        id=shnro,
        date_of_visit=tupva,
        main_ICD10=pdgo
      )
    ),
    error = function(e) {
      chunk <- haven::read_dta(
        path,
        skip = start_row-1,
        n_max = (end_row-start_row+1),
        col_select = c(
          id=shnro,
          date_of_visit=tupva,
          main_ICD9=pdg
        )
      )
    },
    finally = function(chunk) {
      setDT(chunk)
      chunk
    }
  )
  
  chunk
  
}

clean.chunk <- function(chunk,end_year) {
  
  chunk <- tryCatch(
    setnames(
      chunk,
      c('shnro','tupva','pdgo'),
      c('id','date_of_visit','main_ICD10')
    ),
    error = function(e) {
      setnames(
        chunk,
        c('shnro','tupva','pdg'),
        c('id','date_of_visit','main_ICD9')
      )
    },
    finally = function(chunk) {
      chunk[main_ICD10!='' | mainICD9!='']
      chunk
    }
  )

  chunk  
  
}

read.clean.chunk <- function(path,start_row,end_row,end_year) {
  chunk <- read.chunk(path,start_row,end_row)
  chunk <- clean.chunk(chunk,end_year)
  chunk
}

read.dta.batched <- function(path,end_year,batch_size=1E6,stop=3E6) {
  start_rows <- seq(1,stop,batch_size)
  end_rows <- start_rows + batch_size -1
  
  n_batches <- length(start_rows)
  
  chunks <- vector('list',n_batches)
  pb <- progress::progress_bar$new(
    format='reading [:bar] :percent in :elapsed',
    total=n_batches, clear=F, width=60
  )
  
  for (i in 1:n_batches) {
    chunk <- read.clean.chunk(path,start_rows[i],end_rows[i],end_year)
    if (nrow(chunk)==0) {break}
    chunks[[i]] <- chunk
    pb$tick()
  }
  
  data <- data.table::rbindlist(chunks)
  data
}

data <- map(
  files,
  ~ read.dta.batched(.x,batch_size = 1E7,stop = 5E8),
  .progress = T
  ) %>%
  rbindlist(.,fill=T)

# For testing
#mini_data <- data[sample(1:.N,size=1000)]

write_feather(data,here(out_dir,'004_visits.arrow'))
gc()

data[,main_ICD10_3:=str_sub(main_ICD10,1,3)]
main_ICD10 <- data[,.(.N),by=.(main_ICD10_3)]

write_feather(main_ICD10,here(out_dir,'004_main_ICD10.arrow'))
