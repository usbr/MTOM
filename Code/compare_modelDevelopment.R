# Libraries
library(RWDataPlyr)
library(tidyverse)
library(lubridate)

# # TEST - function inputs - how to do this with RiverSMART ?
# scenario_dir <- paste0(Sys.getenv("MTOM_DIR"), "\\Scenario")
# base_scen_nm <- c("ModelBase_RulesBase", "Base") # naming convention of scenarios
# dev_scen_nm <- c("ModelDev_RulesDev", "Dev")
# slots <- c("Powell.Outflow", "Powell.Pool Elevation")
# slot_period <- c("eocy", "eocy")
# data_files <- "ReservoirOutput.csv"

## get WY or CY from a list of dates
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


compare_modelDev <- function(scenario_dir,
                             output_dir,
                             base_scen_nm = c("ModelBase_RulesBase", "Base"),
                             dev_scen_nm = c("ModelDev_RulesDev", "Dev"),
                             slots = c("Powell.Pool Elevation"),
                             slot_period = c("EOCY"),
                             plot_type = c("exceedance0"),
                             data_files = "ReservoirOutput.csv",
                             out_fl_nm = "model_comparision",
                             hydroGroups = c("Wet", "Avg", "Dry")) {
  
  # only get scenarios for desired groups
  scenarios <- list.files(scenario_dir)
  base_scenarios <- scenarios[grep(base_scen_nm[1], scenarios)]
  dev_scenarios <- scenarios[grep(dev_scen_nm[1], scenarios)]
  all_scenarios <- c(base_scenarios, dev_scenarios)
  
  ## == Create df of desired slots from input
  # loop through scenarios
  df <- NULL
  for (scenario_i in all_scenarios) {
    # read and append all data_files based on file type
    for (data_j in unique(data_files)) {
      fl_type_j <- last(unlist(strsplit(data_j, ".", fixed = TRUE)))
      fl_nm_j <- paste0(scenario_dir, "\\", scenario_i, "\\", data_j)
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
  pdf(paste0(output_dir, "\\", out_fl_nm, ".pdf"))
  
  for (i in 1:length(slots)) {
    
    # filter for desired slot
    df_i = df %>% filter(ObjectSlot == slots[i] | 
                           ObjectSlot == gsub(" ", "", slots[i]))
    y_lab = paste(slots[i], '[', unique(df_i$Unit)) # lab with units
    
    ## ---- TIME PERIOD ---- ##
    
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
      
    # -- annual - sum only if full year of data
    } else if (slot_period[i] %in% c('annualWY', 'annualCY')) {
      yrType = ifelse(slot_period[i] == 'annualWY', 'WY', 'CY')
      df_i <- df_i %>%
        mutate(time_per = YrOfWYCY(Timestep, yrType)) %>%
        group_by(scenario_i, ScenarioGroup, hydroGroup, time_per, TraceNumber) %>%
        filter(n() == 12) %>%
        summarise(Value = sum(Value))
      y_lab = paste(y_lab, 'per year') # lab with units
    
    } else {
      stop('check slot_period inputs')
    }
    
    ## ---- PLOT TYPE ---- ##
    
    # -- Boxplots for Base vs. Dev
    if (plot_type[i] == 'boxplot') {
      g <- ggplot(df_i, aes(factor(time_per), Value, fill = ScenarioGroup)) +
        geom_boxplot() +
        theme_bw() +
        labs(x = NULL, y = paste(y_lab, ']')) +
        facet_grid(hydroGroup ~ ., scales = "free_y")
      
    # -- Exceedance figures for Base vs. Dev
    } else if (plot_type[i] %in% c('exceedance0', 'exceedance10')) {
      
      df_ib <- df_i %>%
        group_by(ScenarioGroup, hydroGroup, time_per) %>%
        summarise(
          q0 = quantile(Value, 0),
          q10 = quantile(Value, 0.1),
          q30 = quantile(Value, 0.3),
          q50 = quantile(Value, 0.5),
          q70 = quantile(Value, 0.7),
          q90 = quantile(Value, 0.9),
          q100 = quantile(Value, 1)
        )
      
      # add 0% & 100% exceedance or not
      if (plot_type[i] == 'exceedance0') {
        g <- ggplot(df_ib, aes(time_per, q10, fill = ScenarioGroup)) +
          geom_ribbon(aes(ymin = q0, ymax = q100, fill = ScenarioGroup), 
                      alpha = 0.1) +
          geom_line(aes(time_per, q0, color = ScenarioGroup), 
                    linetype = 'dotted', size =0.6) +
          geom_line(aes(time_per, q100, color = ScenarioGroup), 
                    linetype = 'dotted', size =0.6) 
        cap = 'Note: Exceedances displayed are 0%, 10%, 30%, 50%, 70%, 90% & 100%.'
      } else {
        g <- ggplot(df_ib, aes(time_per, q10, fill = ScenarioGroup))
        cap = 'Note: Exceedances displayed are 10%, 30%, 50%, 70% & 90%.'
      }
      
      g <- g +
        geom_ribbon(aes(ymin = q10, ymax = q90, fill = ScenarioGroup), 
                    alpha = 0.2) +
        geom_ribbon(aes(ymin = q30, ymax = q70, fill = ScenarioGroup),
                    alpha = 0.2) +
        geom_line(aes(time_per, q10, color = ScenarioGroup), 
                  linetype = 'longdash', size =0.6) +
        geom_line(aes(time_per, q30, color = ScenarioGroup), 
                  linetype = 'longdash', size =0.6) +
        geom_line(aes(time_per, q70, color = ScenarioGroup), 
                  linetype = 'longdash', size =0.6) +
        geom_line(aes(time_per, q90, color = ScenarioGroup), 
                  linetype = 'longdash', size =0.6) +
        geom_line(aes(time_per, q50, color = ScenarioGroup), 
                  linetype = 'longdash', size =0.6) +
        theme_bw() +
        labs(x = NULL, y = paste(y_lab, ']'), caption = cap) +
        facet_grid(hydroGroup ~ ., scales = "free_y")
      
      
    } else {
      stop('check plot_type input')
    }
    print(g)
  }
  
  dev.off()
}
