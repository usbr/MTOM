# Libraries
library(RWcheck)

check_mtom_output <- function(scenarios,
                              yaml_rule_files,
                              scenario_dir,
                              output_dir,
                              out_fl_nm = "verification_output") {
  
  yaml_dir <- paste0(dirname(scenario_dir), '/Code/')
  
  check_rw_output(scenarios, yaml_rule_files, scenario_dir, output_dir, yaml_dir, out_fl_nm)
}