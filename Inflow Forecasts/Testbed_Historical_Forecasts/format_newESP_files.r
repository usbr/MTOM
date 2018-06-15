# ================================================================================ 
#  Process ESP forecasts into correct format for ESP Hindcast Simulation
#
# Created by S. Baker, June 2018
# ================================================================================  
rm(list=ls())

## === Load libraries
library(xlsx)

## === Directories
dir_in = 'C:/Users/sabaker/Documents/2.MTOM Testbed/MTOM model/Inflow Forecasts/ESP_Hindcasts/New forecasts/'
dir_out = 'C:/Users/sabaker/Documents/2.MTOM Testbed/MTOM_research/Inflow Forecasts/Testbed_Historical_Forecasts/ESP_Hindcasts/'


## === load file names
setwd(dir_in)
flist = list.files(".")
flist_dir = paste0(flist, '/ESP Traces')

traces = c("Date",paste0(rep("Trace",times = 35),1:35)) #name of columns

## names of forecast locations
loc_short = c("DRGC2","BMDC2","CLSC2","GRNU1","GBRW4","MPSC2",
              "NVRN5","GJLOC","GLDA3","TPIC2","VCRC2","YDLC2") #name of locations in text files
files_in = paste0(loc_short, '.espmvol.txt')

for (i in 1:length(flist)) {
  
  # output file name
  file_out = paste0('ESP_35Trace-fcst-',gsub('_','-', flist[i]),'.xlsx')

  for (j in 1:length(files_in)) {
    
    # read in forecast file
    setwd(paste0(dir_in,flist_dir[i]))
    df = read.table(files_in[j], header = F, fill = T)
    df = df[4:ncol(df),1:36]
    colnames(df) <- traces
    df$Date = format(as.Date(paste0('01/', as.character(df$Date)), '%d/%m/%Y'), '%m/%d/%Y')
    
    # save fcst to worksheet
    setwd(dir_out)
    if (j == 1) {
      write.xlsx(df, file=file_out, sheetName=paste(loc_short[j],sep=""), row.names = FALSE)
    } else {
      write.xlsx(df, file=file_out, sheetName=paste(loc_short[j],sep=""), row.names = FALSE, append=TRUE)
    }
  }
}
 