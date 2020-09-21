## Helper functions for comparing_modelDev function


## check that slot_period_plot variable has correct format
process_PerSlotPlot_Input <- function(slot_period_plot) {
  
  # check if semicolon is used in each input
  cnt_semi = length(slot_period_plot)
  if (cnt_semi != length(grep(";", slot_period_plot))) {
    stop("slot_period_plot inputs must be separated by semicolon")
  }
  
  # check if divisible by 3
  unlistedInput = unlist(strsplit(slot_period_plot, ";"))
  cnt_in = length(unlistedInput)
  
  if (cnt_in %% 3 != 0) {
    stop("slot_period_plot inputs must have inputs for a slot, period, and plot 
         type all separated by semicolons in quotations: slot;timePeriod;plotType")
  }

  slot_mat = matrix(unlist(strsplit(slot_period_plot, ";")), ncol = 3, byrow = T)
  return(slot_mat)
}


# get scenarios with only slots input
getScenarios <- function(slots, 
                         all_scenarios, 
                         data_files, 
                         scenario_dir) {
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
      
      if (nrow(df_j) < 1) {
        stop("the slot specified is not in the specified output file")
      }
      
      df <- rbind(df, cbind(scenario_i, df_j))
    }
  }
  return(df)
}


## timeperiod function
processTimeperiodDF <- function(df_i,
                                period_i) {
  
  if (period_i %in% c("EOWY", "EOCY")) {
    
    # EOWY or EOCY
    df_plot <- df_i %>% 
      filter(month == ifelse(period_i == 'EOWY', 10, 12))  %>%
      mutate(time = year)
    
  } else if (period_i %in% c("annualWY", "annualCY")) {
    
    # annual WY or CY sum with 12 values
    if (period_i == "annualWY") {
      df_i <- df_i %>% mutate(year = year + ifelse(month < 10, 0, 1)) 
    }
    df_plot <- df_i %>% 
      group_by(scenario_i, TraceNumber, ScenarioGroup, hydroGroup, year) %>% 
      filter(n() == 12) %>% #check groups have 12 values
      summarize(Value = sum(Value)) %>% ungroup() %>%
      mutate(time = year)
    
  } else if (period_i %in% c("12monthWY", "12monthCY")) {
    
    ## monthly distribution arrange by cy or wy
    if (period_i == "12monthWY") {
      levels_in = c(10:12, 1:9)
    } else {
      levels_in = 1:12
    }
    
    df_plot <- df_i %>% 
      mutate(time = factor(month, levels = levels_in, 
                           labels = month.abb[levels_in]))
    
  } else if (period_i == 'month') {
    
    # show all months in run period
    df_plot <- df_i %>% 
      mutate(time = Timestep)
    
  } else {
    stop('timeperiod must be one of the following: EOWY,EOCY,month,annualWY,annualCY,12monthWY,12monthCY.')
  }
  return(df_plot)
}


## Get units 
unit_processing <- function(df_in, units, period_i) {
  # units = unique(df_in$Unit)
  
  # convert flow/volume in acre-ft to MAF
  if (units %in% c("acre-ft", "acre-ft/month") &
      median(df_in$Value, na.rm = T) > 2*10^6) {
    df_in$Value = df_in$Value / 10^6

    if (period_i %in% c("annualWY", "annualCY")) {
      units = "maf / yr"
    } else {
      units = gsub("acre-ft", "maf ", units)
    }
    
  }
  
  ## kaf/mon to maf/yr or kaf/yr
  if (units %in% c("1000 acre-ft/month") & 
      period_i %in% c("annualWY", "annualCY")) {
    
    ## kaf/mon to maf/yr
    if (median(df_in$Value, na.rm = T) > 2*10^3) {
      df_in$Value = df_in$Value / 10^3
      units = "maf / yr"
    } else {
      units = "kaf / yr"
    }
  }
  
  return(list(df = df_in, units = units))
}


