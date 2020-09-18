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
