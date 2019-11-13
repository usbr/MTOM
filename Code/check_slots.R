# Libraries
library(RWDataPlyr)
library(validate)
library(tidyverse)
# library(miniCRAN)
# library(data.table)
library(yaml)

# # TEST - function inputs - how to do this with RiverSMART ?
# scenario_dir <- paste0(Sys.getenv("MTOM_DIR"), '\\Scenario')
# scenarios <- list.files(paste0(study_dir, "\\", "Scenario"))
# yaml_dir <- "C:/Users/sabaker/Projects/Models/MTOM/MTOM_Dev_Testing_Tool/Dev_Tool_scripts/crmdiag/inst/"
# yaml_rule_files = paste0(yaml_dir, c("check_MTOM_ub_outflow.yaml", "check_MTOM_ub_storage.yaml"))
# yaml_rule_files = c("check_MTOM_ub_outflow.yaml", "check_MTOM_ub_storage.yaml")



check_slots <- function(scenario_dir,
                        output_dir,
                        scenarios,
                        yaml_rule_files,
                        out_fl_nm = 'verification_output') {

  # open log file
  log_fl <- file(paste0(output_dir, "\\log_file.txt"), open = 'w')

  # read yamls to check for data files to read
  data_files <- NULL
  for (yaml_i in yaml_rule_files) {
    yaml_i <- paste0(dirname(scenario_dir), "/Code/", yaml_i)
    fl_i <- yaml.load_file(yaml_i)
    for (j in 1:length(fl_i$rules)) {
      data_files <- c(data_files, fl_i$rules[[j]]$in_file)
    }
  }

  # loop through scenarios
  out_summ <- out_summ_i <- summ_err <- NULL
  for (scenario_i in scenarios) {
    cat(paste("Scenario -", scenario_i), file = log_fl, sep="\n")

    # read and append all data_files based on file type
    df <- NULL
    for (data_j in unique(data_files)) {
      fl_type_j <- last(unlist(strsplit(data_j, ".", fixed = TRUE)))
      fl_nm_j   <- paste0(scenario_dir, "\\", scenario_i, "\\", data_j)
      if (fl_type_j == "rdf") {
        df_j <- rdf_to_rwtbl2(fl_nm_j)
      } else {
        df_j <- read_rw_csv(fl_nm_j)
      }
      df <- rbind(df, df_j)
    }

    # select important columns and spread data
    df <- df %>%
      select(Timestep, TraceNumber, ObjectSlot, Value) %>%
      spread(ObjectSlot, Value)

    # loop through yaml rule files and collect summary output
    scen_err = NULL # probably a more efficient way of doing this...
    for (yaml_j in yaml_rule_files) {
      yaml_j <- paste0(dirname(scenario_dir), "/Code/", yaml_j)
      rules_j <- validator(.file = yaml_j)
      vv <- confront(df, rules_j)
      out_summ_i <- rbind(out_summ_i, summary(vv))
      yaml_rules <- last(unlist(strsplit(yaml_j, "/", fixed = TRUE)))

      # print errors or passes
      if (length(errors(vv)) > 0) {
        cat(paste('  ... fails in', yaml_rules), file = log_fl, sep="\n")
        cat(errors(vv), file = log_fl, sep="\n")
        scen_err = c(scen_err, 1)
      } else {
        cat(paste('  ... all passes in', yaml_rules), file = log_fl, sep="\n")
        scen_err = c(scen_err, 0)
      }
    }

    # add scenario column to output
    out_summ = rbind(out_summ,
                     cbind(scenario_i, out_summ_i))
    
    # check if scenario produced errors
    if (scen_err > 0) { summ_err = c(summ_err, 1) } else {
      summ_err = c(summ_err, 0)
    }
    
  }

  # write output to text file
  write.table(out_summ, paste0(output_dir, "\\", out_fl_nm, ".txt"),
              sep = "\t", row.names = FALSE)
  close(log_fl)
  
  # add summary to beginning of log file
  nscen = length(summ_err)
  npass = nscen - sum(summ_err)
  fl_nm = paste0(output_dir, "\\log_file.txt")
  fConn <- file(fl_nm,'r+')
  Lines <- readLines(fl_nm)
  writeLines(c(paste(npass, "/", nscen,
                     "scenarios passed all tests\n"), Lines), con = fl_nm)
  close(fl_nm)
}

# test
# check_slots(study_dir, scenarios, yaml_rule_files)


# # read data and remove unit scheme to limit NAs
# df_i <- fread(paste0(study_dir, '/Scenarios/', scenario_i, '/', data_i)) %>%
#   select(-Unit) %>%
#   spread(Object.Slot, `Slot Value`)
#
# # Run yaml rules - note NAs in table (not sure if we canre about this...)
# ub_rules <- validator(.file = paste0(yaml_dir, "check_MTOM_ub_outflow.yaml"))
# vv <- confront(df_i, ub_rules)
# summary(vv)
#
# summary(vv)
# aggregate(vv, by = "record")
# sort(vv)
# errors(vv)
#
# failed <- df_i[!values(vv)[,2], ] %>%
#   select(Timestep, `Trace Number`, MorrowPoint.Outflow)
#
# ## add rules for storage, lb res
