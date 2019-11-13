# =================================================================== #
#   Development Testing Tool Post-processing Script
#   compare_modelDevelopment.R
#      - compare base and development mdl/rules
#      - outputs pdf with plots of desired slots
#
#   S. Baker, Nov 2019
# =================================================================== #

## -- Libraries
library(RWDataPlyr)
library(tidyverse)
library(lubridate)

## -- Load graphing functions from script
setwd(Sys.getenv('MTOM_DIR')) # get base folder location
source(paste0(getwd(), '/Code/graphFuncs.R'))

## -- Fuction: get WY or CY from a list of dates
YrOfWYCY <- function(fcst_dates, WYorCY) {
  if (WYorCY == 'CY') { # add year to df
    # yr_v = as.numeric(strftime(sort(unique(fcst_dates)), "%Y"))
    yr_v = as.integer(format(fcst_dates, '%Y'))
  } else {
    yr_v = as.integer(format(fcst_dates, '%Y')) + 
      ifelse(as.integer(format(fcst_dates, '%m')) < 10, 0, 1)
  }
  return(yr_v)
}

## -- Fuction
compare_modelDev <- function(scenario_dir,
                             output_dir,
                             base_scen_nm = c("ModelBase,RulesBase", "Base"),
                             dev_scen_nm = c("ModelDev,RulesDev", "Dev"),
                             # slots = c("Powell.Pool Elevation"),
                             # slot_period = c("EOCY"),
                             # plot_type = c("exceedance0"),
                             slot_period_plot = "Mead.Outflow;EOCY;boxplot",
                             data_files,
                             out_fl_nm = "model_comparision",
                             hydroGroups = c("Wet", "Avg", "Dry")) {
  
  # only get scenarios for desired groups
  scenarios <- list.files(scenario_dir)
  base_scenarios <- scenarios[grep(base_scen_nm[1], scenarios)]
  dev_scenarios <- scenarios[grep(dev_scen_nm[1], scenarios)]
  all_scenarios <- c(base_scenarios, dev_scenarios)
  
  # get inputs from string
  slot_period_plot = matrix(unlist(strsplit(slot_period_plot, ';')), 
                            ncol = 3, byrow = T)
  if (anyNA(slot_period_plot)) { 
    stop('Check slot_period_plot inputs!') } # stop if input is wrong
  
  # set-up inputs for script use
  slots = slot_period_plot[,1]
  slot_period = slot_period_plot[,2]
  plot_type = slot_period_plot[,3]
  
  ## == Create df of desired slots from input
  # loop through scenarios
  df <- NULL
  for (scenario_i in all_scenarios) {
    # read and append all data_files based on file type
    for (data_j in unique(data_files)) {
      fl_type_j <- last(unlist(strsplit(data_j, ".", fixed = TRUE)))
      fl_nm_j <- paste0(scenario_dir,  scenario_i, "/", data_j)
      if (fl_type_j == "rdf") {
        df_j <- rdf_to_rwtbl2(fl_nm_j)
      } else {
        df_j <- read_rw_csv(fl_nm_j)
      }
      # filter for slots and add scenario name
      df_j <- df_j %>%
        filter(ObjectSlot %in% slots | ObjectSlot %in% gsub(" ", "", slots))
      df <- rbind(df, cbind(scenario_i, df_j))
    }
  }
  
  ## == Add scenario groups
  
  # Base vs. Dev
  df$ScenarioGroup <- NA
  df$ScenarioGroup[grep(base_scen_nm[1], df$scenario_i)] <- base_scen_nm[2]
  df$ScenarioGroup[grep(dev_scen_nm[1], df$scenario_i)] <- dev_scen_nm[2]
  
  # Hydrology initial conditions
  df$hydroGroup <- NA
  for (i in hydroGroups) {
    df$hydroGroup[grep(i, df$scenario_i)] <- i
  }
  
  # add columns for grouping - may be a function in RWDataPlyr..
  df <- df %>% mutate(
    year = year(Timestep),
    month = month(Timestep),
    ScenarioGroup = factor(ScenarioGroup),
    hydroGroup = factor(hydroGroup),
    Timestep = as.Date(Timestep)
  )
  
  
  ## === Figures based on slots and slot_period vectors
  pdf(paste0(output_dir, out_fl_nm, ".pdf"))
  
  for (i in 1:length(slots)) {
    
    # filter for desired slot
    df_i = df %>% filter(ObjectSlot == slots[i] | 
                           ObjectSlot == gsub(" ", "", slots[i]))
    y_lab = paste(slots[i], '[', unique(df_i$Unit)) # lab with units
    
    ## ==== TIME PERIOD ==== ##
    
    # -- EOCY or EOWY 
    if (slot_period[i] == 'EOCY' | slot_period[i] == 'EOWY') {
      ey_mon = ifelse(slot_period[i] == 'EOCY', 12, 10)
      
      df_i <- df_i %>%
        filter(month == ey_mon) %>%
        mutate(time_per = year)
      
      # -- monthly values 
    } else if (slot_period[i] == 'month') {
      df_i <- df_i %>%
        mutate(time_per = Timestep)
      
      # -- monthly valueson an annual scale
    } else if (slot_period[i] %in% c('12monthWY','12monthCY')) {
      if(slot_period[i] =='12monthWY') {
        mon_order = month.abb[c(10:12, 1:9)] } else {
          mon_order = month.abb }
      
      df_i <- df_i %>%
        mutate(time_per = factor(month.abb[month], levels = mon_order))
      
      # -- annual - sum only if full year of data
    } else if (slot_period[i] %in% c('annualWY', 'annualCY')) {
      yrType = ifelse(slot_period[i] == 'annualWY', 'WY', 'CY')
      df_i <- df_i %>%
        mutate(time_per = YrOfWYCY(Timestep, yrType)) %>%
        group_by(scenario_i, ScenarioGroup, hydroGroup, time_per, 
                 TraceNumber) %>%
        filter(n() == 12) %>%
        summarise(Value = sum(Value))
      y_lab = paste(y_lab, 'per year') # lab with units
      
    } else {
      stop('check slot_period inputs')
    }
    
    ## ==== PLOT TYPE ==== ##
    
    # -- Boxplots for Base vs. Dev
    if (plot_type[i] == 'boxplot') {
      g <- compareBoxplot(df_i, paste(y_lab, ']'))
      
      # -- Exceedance figures for Base vs. Dev 
    } else if (plot_type[i] %in% c('exceedance0', 'exceedance10')) {
      
      # priont error if user specified periodic fig
      if (slot_period[i] %in% c('12monthWY','12monthCY')) {
        plot.new()
        txt = 'Exceedance figures do not work with slot_periods
                                         12monthWY or 12monthCY.'
        text(.5, .5, txt, font=2, cex=0.75)
        g <- NULL
        
      # -- Exceedance figures for Base vs. Dev 
      } else {
        g <- exceedPlot(df_i, paste(y_lab, ']'), plot_type = plot_type[i])
      }
      
    } else {
      stop('check plot_type input')
    }
    print(g)
  }
  
  dev.off()
}
