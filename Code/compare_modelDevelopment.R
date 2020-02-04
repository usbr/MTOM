# Libraries
library(RWDataPlyr)
library(tidyverse)
library(lubridate)

# # TEST - function inputs - how to do this with RiverSMART ?
# scenario_dir <- paste0(Sys.getenv("MTOM_DIR"), "\\Scenario")
# base_scen_nm <- c("ModelBase_RulesBase", "Base")
# dev_scen_nm <- c("ModelDev_RulesDev", "Dev")
# slots <- c("Mead.Pool Elevation", "Powell.Pool Elevation")
# slot_period <- c("eocy", "eocy")
# data_files <- "ReservoirOutput.csv"


compare_modelDev <- function(scenario_dir,
                        output_dir,
                        base_scen_nm = c("ModelBase_RulesBase", "Base"),
                        dev_scen_nm = c("ModelDev_RulesDev", "Dev"),
                        slots = c("Mead.Pool Elevation"),
                        slot_period = c("eocy"),
                        data_files = "ReservoirOutput.csv",
                        out_fl_nm = "model_comparision") {
  hydroIC <- c("Wet", "Avg", "Dry")

  # only geg scenarios for desired groups
  scenarios <- list.files(scenario_dir)
  base_scenarios <- scenarios[grep(base_scen_nm[1], scenarios)]
  dev_scenarios <- scenarios[grep(dev_scen_nm[1], scenarios)]
  all_scenarios <- c(base_scenarios, dev_scenarios)

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
  for (slot_i in slots) {
    df_i <- df %>%
      filter(month == 12) %>%
      filter(ObjectSlot %in% slot_i | ObjectSlot %in% gsub(" ", "", slot_i))

    # EOCY Mead PE for Base vs. Dev
    g <- ggplot(df_i, aes(factor(year), Value, fill = ScenarioGroup)) +
      geom_boxplot() +
      theme_bw() +
      labs(x = NULL, y = slot_i) +
      facet_grid(hydroGroup ~ ., scales = "free_y")
    print(g)


    # Exceedance for Base vs. Dev
    df_i <- df %>%
      filter(ObjectSlot %in% slot_i | ObjectSlot %in% gsub(" ", "", slot_i)) %>%
      group_by(ScenarioGroup, hydroGroup, Timestep) %>%
      summarise(
        q10 = quantile(Value, 0.1),
        q30 = quantile(Value, 0.3),
        q50 = quantile(Value, 0.5),
        q70 = quantile(Value, 0.7),
        q90 = quantile(Value, 0.9)
      )

    g <- ggplot(df_i, aes(Timestep, q10, fill = ScenarioGroup)) +
      geom_ribbon(aes(ymin = q10, ymax = q90, fill = ScenarioGroup), 
                  alpha = 0.2) +
      geom_ribbon(aes(ymin = q30, ymax = q70, fill = ScenarioGroup), 
                  alpha = 0.2) +
      geom_line(aes(Timestep, q10, color = ScenarioGroup), 
                linetype = 'longdash', size =0.6) +
      geom_line(aes(Timestep, q30, color = ScenarioGroup), 
                linetype = 'longdash', size =0.6) +
      geom_line(aes(Timestep, q70, color = ScenarioGroup), 
                linetype = 'longdash', size =0.6) +
      geom_line(aes(Timestep, q90, color = ScenarioGroup), 
                linetype = 'longdash', size =0.6) +
      geom_line(aes(Timestep, q50, color = ScenarioGroup), 
                linetype = 'longdash', size =0.6) +
      theme_bw() +
      labs(x = NULL, y = slot_i) +
      facet_grid(hydroGroup ~ ., scales = "free_y")
    print(g)
  }
  dev.off()
}
