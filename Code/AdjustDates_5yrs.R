# messages written to stdout and stderr are displayed
# in the RiverSMART diagnostic window.
info <- function(...) cat(sprintf(...), sep='', file=stdout());
error <- function(...) cat(sprintf(...), sep='', file=stderr())

# RiverSMART calls the function adjustDates() with two
# arguments - the path to the input file and the path
# to the output file.
adjustDates <- function(inputFilePath, outputFilePath) {
  
  info("This message is to stdout\n")
  error("This message is to stderr\n")
  
  # inputFilePath = paste0("'",inputFilePath,"'")
  # outputFilePath = paste0("'",outputFilePath,"'")
  
  # read in file
  df = read.csv(inputFilePath, header = T)
  
  #change end date and run length
  for (i in c(1:12)) {
    #change finish timestep to September for Runs in February through September
    df <- within(df, end_month[start_month == i] <- 12)
    df <- within(df, end_day[start_month == i] <- 31)
    df <- within(df, num_timesteps[start_month == i] <- (61 - start_month[start_month == i]))
  }
  
  # for (i in c(10:12)) {
  #   #change finish timestep to September for Runs in February through September
  #   df <- within(df, end_month[start_month == i] <- 9)
  #   df <- within(df, end_day[start_month == i] <- 30)
  #   df <- within(df, end_year[start_month == i] <- (end_year[start_month == i] + 1))
  #   df <- within(df, num_timesteps[start_month == i] <- (70 - start_month[start_month == i]))
  # }
  
  
  #write output
  write.csv(df, outputFilePath, row.names = F, quote = F)
  
}

# ####  TEST  ####
# inputFilePath = "C:/Users/sabaker/Documents/2.Testbed/MTOM model/Working/Run_Range/RscriptInput.txt"
# outputFilePath = "C:/Users/sabaker/Documents/2.Testbed/MTOM model/Working/Run_Range/RscriptOutput.txt"
# 
# adjustDates(inputFilePath, outputFilePath)