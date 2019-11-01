## Plotting functions for compare_modelDevelopment.R

# df for these scripts need the following columns:
#  Value  - numerical data
#  ScenarioGroup - Base vs dev
#

compareBoxplot <- function(df, y_lab) {
  g <- ggplot(df, aes(factor(time_per), Value, fill = ScenarioGroup)) +
    geom_boxplot() +
    theme_bw() +
    labs(x = NULL, y = y_lab) +
    facet_grid(hydroGroup ~ ., scales = "free_y")
  
  return(g)
}


## == Exceedance Figures
exceedPlot <- function(df, y_lab, plot_type = 'exceedance0') {
  df_ib <- df %>%
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
  if (plot_type == 'exceedance0') {
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
    labs(x = NULL, y = y_lab, caption = cap) +
    facet_grid(hydroGroup ~ ., scales = "free_y")
  
  return(g)
}
