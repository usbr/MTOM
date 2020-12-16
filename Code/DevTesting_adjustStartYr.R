# ==============================================================================
# Roll Dev Testing Tool a year forward 
#
#    This script will read current input data and overwrite current excel files.
#    It may be desirable to save a copy of the files incase this script doesnt 
#    work as intended. There is a copy saved on GitHub so this is not completely
#    necessary. Only necessary files are changed
#
# S. Baker, Sept 2020
# ==============================================================================
rm(list=ls())
library(openxlsx)
library(tidyverse)
library(lubridate)

## INPUT
WY_new = 2021 # change which WY year is desired

## For Testing only
fl_out_test = '' # add characters to beginning of file name to test before making change


## ---- DO NOT change script below! Run entire script to document change!

## Dir
data_dir = paste0(Sys.getenv("MTOM_DIR"), "\\Input Data\\DevTesting\\")

## Files to change - dont change order!
fls = c("DevTesting_Inflows.xlsx",
        "DevTesting_IC.avg.xlsx",
        "DevTesting_IC.dry.xlsx",
        "DevTesting_IC.wet.xlsx")

for (i in 1:length(fls)) {
  sheets_i = openxlsx::getSheetNames(fls[i])

  ls_out = list()
  for (j in 1:length(sheets_i)) {
    df_j = openxlsx::read.xlsx(fls[i], sheet = j, detectDates = T, 
                               check.names = FALSE, sep.names = " ")

    # Check that dates havent been updated already - only works for Inflow file
    if (year(df_j$Date[1]) == WY_new - 1 & fls[i] == "DevTesting_Inflows.xlsx") {
      stop(paste("The Excel input files are already setup for WY", WY_new, 
                 "... Check the Dev Testing Tool before proceeding!!"))
    }
    
    # convert to numeric
    df_j = readr::type_convert(df_j, col_types = cols())
    
    # For  writing xlsx NaN need to be NA, will be converted back in function
    df_j[is.na(df_j)] = NA
    
    # read Date and add a year
    df_j$Date = df_j$Date %m+% years(1)
    
    # collect data in list to output
    ls_out[[sheets_i[j]]] <- df_j
  }
  
  fl_out = paste0(fl_out_test, fls[i])

  openxlsx::write.xlsx(ls_out, file = fl_out, keepNA = TRUE, na.string = 'NaN')
  print(paste0('... saving ', fl_out))
}

## ReadMe Edit - add line that excel sheets have been converted for new start year
addMessage = paste('Adding year to input data. New files for is for running water year', WY_new, 'on', Sys.Date())

storeLines <- readLines("README.md")
storeLines <- c(storeLines, addMessage)
log_fl <- file("README.md", open = "w")
writeLines(storeLines, con = log_fl)
close(log_fl)
