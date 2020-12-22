##################################################################
#                 1. Renaming/Reformatting Raw Files
# Here we make copies of relevant files from the raw FARS data, 
# (which we downloaded from the USDOT FTP site as .csv files) to:
# i. Ensure we don't inadvertently modify the original raw files,
# ii. Rename these files to a uniform naming convention, and
# iii. Save the files as .RData rather than .csv, for faster
#      opening/processing.
#
# P.S. Acharya, 2020
##################################################################
library(readr)
library(tidyverse)

##################################################################

start.time <- Sys.time()
# just to show progress

for (i in 1975:2018) {
  now <- Sys.time()
  cat('\014')
  print(paste0(
    'Saving files for year ',
    i,
    '. Time since start is ',
    as.integer(difftime(now, start.time, units = 'mins')),
    ' minutes.'
  ))
  loc <- paste0('Raw/FARS', i, 'NationalCSV/')
  files <- dir(loc)
  acc.loc <- match('accident.csv', tolower(files))
  veh.loc <-  match('vehicle.csv', tolower(files))
  per.loc <- match('person.csv', tolower(files))
  fac.loc <- match('factor.csv', tolower(files))
  
  if (is.na(acc.loc) == FALSE) {
    #if this file exists for this year
    acc <- read_csv(paste0(loc, files[acc.loc]))
  }
  
  if (is.na(veh.loc) == FALSE) {
    #if this file exists for this year
    veh <- read_csv(paste0(loc, files[veh.loc]))
  }
  
  
  if (is.na(fac.loc) == FALSE) {
    #if this file exists for this year
    fac <- read_csv(paste0(loc, files[fac.loc]))
  }
  
  if (is.na(per.loc) == FALSE) {
    #if this file exists for this year
    per <- read_csv(paste0(loc, files[per.loc]))
  }
  
  to.save <- c('acc', 'veh', 'fac', 'per')
  #files we want to save
  
  to.save <- to.save[to.save %in% ls() == TRUE]
  #check if that file exists for this year
  
  save(list = to.save,
       file = paste0('Renamed/files_', i, '.RData'))
  # Saving all objects for that year into a common
  # RData object (which, when loaded, should bring up
  # all these files).
}

##################################################################
rm(list = ls())
cat('\014')
