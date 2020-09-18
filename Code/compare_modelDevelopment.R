# Libraries
library(RWDataPlyr)
library(tidyverse)
library(lubridate)

# # TEST - function inputs - how to do this with RiverSMART ?
scenario_dir <- paste0(Sys.getenv("MTOM_DIR"), "\\Scenario")
base_scen_nm <- c("ModelBase,RulesBase", "Base")
dev_scen_nm <- c("ModelDev,RulesDev", "Dev")
slot_period_plot = c("Powell.Pool Elevation;EOCY;boxplot",
                     "Mead.Pool Elevation;EOCY;boxplot")
data_files <- "ReservoirOutput.csv"
out_fl_nm = "test"


compare_modelDev <- function(scenario_dir,
                             output_dir,
                             base_scen_nm, #= c("ModelBase,RulesBase", "Base"),
                             dev_scen_nm, #= c("ModelDev,RulesDev", "Dev"),
                             slot_period_plot,
                             data_files = "ReservoirOutput.csv",
                             out_fl_nm = "model_comparision") {
  hydroIC <- c("Wet", "Avg", "Dry")
  
  # get scenarios for desired groups
  scenarios <- list.files(scenario_dir)
  base_scenarios <- scenarios[grep(base_scen_nm[1], scenarios)]
  dev_scenarios <- scenarios[grep(dev_scen_nm[1], scenarios)]
  all_scenarios <- c(base_scenarios, dev_scenarios)
  
  # extract slot_period_plot
  slot_mat <- process_PerSlotPlot_Input(slot_period_plot)
  slots <- unique(slot_mat[,1])
  
  # loop through scenarios
  df <- getScenarios(slots, all_scenarios, data_files, scenario_dir)
  
  # add scenario group
  df$ScenarioGroup <- NA
  df$ScenarioGroup[grep(base_scen_nm[1], df$scenario_i)] <- base_scen_nm[2]
  df$ScenarioGroup[grep(dev_scen_nm[1], df$scenario_i)] <- dev_scen_nm[2]
  
  # add hydrology IC column
  df$hydroGroup <- NA
  for (i in hydroIC) {
    df$hydroGroup[grep(i, df$scenario_i)] <- i
  }
  
  # filter by eocy data - may be a function in RWDataPlyr..
  df <- df %>% mutate(
    year = year(Timestep),
    month = month(Timestep),
    ScenarioGroup = factor(ScenarioGroup),
    hydroGroup = factor(hydroGroup),
    Timestep = as.Date(Timestep)
  ) 
  
  # EOCY PE for Base vs. Dev
  pdf(paste0(output_dir, "\\", out_fl_nm, ".pdf"))
  for (i in 1:nrow(slot_mat)) {
    slot_i = slot_mat[i,1]
    period_i = slot_mat[i,2]
    plot_i = slot_mat[i,3]
    
    # filter for slot
    df_i <- df %>%
      filter(ObjectSlot %in% slot_i | ObjectSlot %in% gsub(" ", "", slot_i))
    
    # units for graph
    unit_in = unique(df_i$Unit)
    if (period_i %in% c("annualWY", "annualCY")) {
      unit_in = paste(unit_in, "/ yr")
    }
    
    # changes plot type for plot type that doesnt work with exceedance
    if (period_i %in% c("12monthWY", "12monthCY") & plot_i %in% c("exceedance0", "exceedance10")) {
      plot_i = 'boxplot'
      print('Changing plot type exceednace to boxplot for 12monthWY time period!')
    }
    
    # agg/filter data based on slot time period type
    df_plot <- processTimeperiodDF(df_i, period_i)
    
    ## PLOT TYPE: boxplot,exceedance0,exceedance10
    if (plot_i == "boxplot") {
      df_ploti <- df_plot %>% mutate(time = factor(time))
      
      g <- ggplot(df_plot, aes(factor(time), Value, fill = ScenarioGroup)) +
        geom_boxplot() +
        theme_bw() +
        labs(x = NULL, y = paste0(slot_i, " (", unit_in, ")")) +
        facet_grid(hydroGroup ~ ., scales = "free_y")
      
    } else if(plot_i %in% c("exceedance0", "exceedance10")) {
      # doesnt work for: 12monthWY or 12monthCY
      
      # Exceedance for Base vs. Dev
      df_ploti <- df_plot %>%
        group_by(ScenarioGroup, hydroGroup, time) %>% na.omit() %>%
        summarise(
          q0 = quantile(Value, 0),
          q10 = quantile(Value, 0.1),
          q30 = quantile(Value, 0.3),
          q50 = quantile(Value, 0.5),
          q70 = quantile(Value, 0.7),
          q90 = quantile(Value, 0.9),
          q100 = quantile(Value, 1)
        ) %>% ungroup() 
      
      # start plot
      g <- ggplot(df_ploti, aes(time, q10, fill = ScenarioGroup)) 
      
      # add exceedance0 if type
      if (plot_i == "exceedance0") {
        g <-  g + 
          geom_line(aes(time, q0, color = ScenarioGroup), 
                    linetype = 'dotted', size = 0.5) +
          geom_line(aes(time, q100, color = ScenarioGroup), 
                    linetype = 'dotted', size = 0.5) +
          geom_ribbon(aes(ymin = q0, ymax = q100, fill = ScenarioGroup),
                      alpha = 0.1)
      }
      
      # complete plot
      g <-  g + 
        geom_ribbon(aes(ymin = q10, ymax = q90, fill = ScenarioGroup), 
                    alpha = 0.2) +
        geom_ribbon(aes(ymin = q30, ymax = q70, fill = ScenarioGroup), 
                    alpha = 0.2) +
        geom_line(aes(time, q10, color = ScenarioGroup),
                  linetype = 'dotted', size = 0.6) +
        geom_line(aes(time, q30, color = ScenarioGroup), 
                  linetype = 'longdash', size = 0.6) +
        geom_line(aes(time, q70, color = ScenarioGroup), 
                  linetype = 'longdash', size = 0.6) +
        geom_line(aes(time, q90, color = ScenarioGroup),
                  linetype = 'dotted', size = 0.6) +
        geom_line(aes(time, q50, color = ScenarioGroup), 
                  linetype = 1, size = 0.5) +
        theme_bw() +
        labs(x = NULL, y = paste0(slot_i, " (", unit_in, ")")) +
        facet_grid(hydroGroup ~ ., scales = "free_y")
    }
    print(g)
    
  }
  dev.off()
}



### HELPER FUNCTIONS




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

