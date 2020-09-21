# Libraries
library(RWDataPlyr)
library(tidyverse)
library(lubridate)

# source helper functions
source(paste0(Sys.getenv("MTOM_DIR"), "\\Code\\compare_plots_helperFuncs.R"))

# # # TEST - function inputs - how to do this with RiverSMART ?
scenario_dir <- paste0(Sys.getenv("MTOM_DIR"), "\\Scenario")
base_scen_nm <- c("ModelBase,RulesBase", "Base")
dev_scen_nm <- c("ModelDev,RulesDev", "Dev")
slot_period_plot = c("PowellData.ActualAnnualReleaseVolume;sumCY;boxplot",
                     "Mead.Outflow;sumWY;boxplot")
  # c("Powell.Pool Elevation;EOCY;boxplot",
  #                    "Mead.Pool Elevation;EOCY;boxplot")
# data_files <- "ReservoirOutput.csv" 
data_files <- c("OpsUse.csv", "ReservoirOutput.csv")
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
  
  # get data and add scenario group
  df <- getScenarios(slots, all_scenarios, data_files, scenario_dir)
  df$ScenarioGroup <- NA
  df$ScenarioGroup[grep(base_scen_nm[1], df$scenario_i)] <- base_scen_nm[2]
  df$ScenarioGroup[grep(dev_scen_nm[1], df$scenario_i)] <- dev_scen_nm[2]
  
  # add hydrology IC column
  df$hydroGroup <- NA
  for (i in hydroIC) {
    df$hydroGroup[grep(i, df$scenario_i)] <- i
  }
  
  # reformat df
  df <- df %>% mutate(
    year = year(Timestep),
    month = month(Timestep),
    ScenarioGroup = factor(ScenarioGroup),
    hydroGroup = factor(hydroGroup, levels = hydroIC),
    Timestep = as.Date(Timestep)
  ) 
  
  # EOCY PE for Base vs. Dev
  pdf(paste0(output_dir, "\\", out_fl_nm, ".pdf"))
  for (i in 1:nrow(slot_mat)) {
    slot_i = slot_mat[i,1]
    period_i = slot_mat[i,2]
    plot_i = slot_mat[i,3]
    
    # changes plot type for plot type that doesnt work with exceedance
    if (period_i %in% c("12monthWY", "12monthCY") & plot_i %in% c("exceedance0", "exceedance10")) {
      plot_i = 'boxplot'
      warning('Changing plot type exceednace to boxplot for 12monthWY time period!')
    }
    
    # filter for slot
    df_i <- df %>%
      filter(ObjectSlot %in% slot_i | ObjectSlot %in% gsub(" ", "", slot_i))
    
    # agg/filter data based on slot time period type
    df_agg <- processTimeperiodDF(df_i, period_i)
    
    # units for graph
    units_i <- unique(df_i$Unit)
    df_ls <- unit_processing(df_agg, units_i, period_i)
    
    ## PLOT TYPE: boxplot,exceedance0,exceedance10
    if (plot_i == "boxplot") {
      df_ploti <- df_ls$df %>% mutate(time = factor(time))
      
      g <- ggplot(df_ploti, aes(factor(time), Value, fill = ScenarioGroup)) +
        geom_boxplot() +
        theme_bw() +
        labs(x = NULL, y = paste0(slot_i, " (", df_ls$units, ")")) +
        facet_grid(hydroGroup ~ ., scales = "free_y")
      
    } else if(plot_i %in% c("exceedance0", "exceedance10")) {
      # doesnt work for: 12monthWY or 12monthCY
      
      # Exceedance for Base vs. Dev
      df_ploti <- df_ls$df %>%
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
        labs(x = NULL, y = paste0(slot_i, " (", df_ls$units, ")")) +
        facet_grid(hydroGroup ~ ., scales = "free_y")
    }
    print(g)
    
  }
  dev.off()
}



### HELPER FUNCTIONS






