# ===========================================
#   read in plot data from rdf 
#   Contents
## 1. Set Up ##
## 2. look at a single slot ##
## 3. compare an ensemble  worth of data ##
## 4. compare two ensembles ##
#   Created by C. Felletter 5/2018
# ===========================================


## 1. Set Up ##
#Install all the following if you don't have
# install.packages(ggplot2)
# install.packages(tidyr)
# install.packages(devtools)

#load libraries 
library(ggplot2)
library(tidyr) #dplyr

#Install RWDataPlyr
# library('devtools')
# devtools::install_github("rabutler/RWDataPlyr", build_vignettes = TRUE)
library('RWDataPlyr')
#see https://github.com/BoulderCodeHub/RWDataPlyr
#look at sample work flow
vignette("rwdataplyr-workflow", package = "RWDataPlyr")

## directories set up
#Use env variable 
MTOMDIR <- Sys.getenv("MTOM_DIR") 
# setwd(paste0(MTOMDIR,"/rdfOutput/")) #set to the folder containing the sub folders for each ensemble
setwd(paste0(MTOMDIR,"/Output Data/RDF Process/")) #set to the folder containing the sub folders for each ensemble

#OR just point to the folder 
#rdf_dir = "C:/Users/cfelletter/Documents/MTOM/rdfOutput" run folder
# setwd(rdf_dir) #set to the folder containing the sub folders for each ensemble

## input
scenarios = c("Apr_FGdev", "FGdev") #scenarios are folder names for the rdfs from your different runs
my_scens = c("Apr", "FGdev") #names for your senarios to be plotted
#see RWDATPlyr Workflow for more information 


## 2. look at a single slot ##
slot = "FlamingGorge.Outflow"
file = "res.rdf" #"MTOM.rdf"
# variable = "FGCYOutflow" #short name for output

#read in the rdf
rdf <- read_rdf(iFile = paste0(scenarios[1],"/",file))

# ensure the slot you want is in the rdf:
rdf_slot_names(rdf)

# then get the minimum annual outflow for all 5 years and traces
rdf %>% 
  rdf_get_slot(slot) %>%
  rwslot_annual_sum()


## 3. look at results from one ensemble ##

#specify the rwd_agg object which defines what summaries will be created 
rwa1 <- rwd_agg(read.csv("rw_agg_FG.csv", stringsAsFactors = FALSE)) 
?rwd_agg #function name: cy, wy, eocy, eowy, full month name, asis 
#see RWDATPlyr Workflow for more information 


# #After the rwd_agg is specified the object is passed to rdf_aggregate() along with a few parameters specifying where the data are stored, and a tbl_df is returned:
rdf_aggregate(
  rwa1,
  rdf_dir = scenarios[1]
)


## 4. compare two ensembles ##
scen_dir = paste0(MTOMDIR,"/Output Data/RDF Process/") #set to the folder containing the sub folders for each ensemble
names(my_scens) = my_scens #naming #must name these

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  my_scens,
  agg = rwa1,
  scen_dir = scen_dir
)

unique(scen_res$Variable) #check variable names 

## plot 
pdf(paste0('FG_MTOM_Example.pdf'), width=9, height=6)

variable = "FGMin"
y_lab = "Pool Elevation"
title = "Flaming Gorge CY Min. PE"

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2022) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Year, Value, color = Scenario)) + 
  geom_line() +
  labs(title = title, y = y_lab) 

#lower min pool elev in FGdev

variable = "FGEocy"
y_lab = "Pool Elevation"
title = "Flaming Gorge EOCY PE"

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2022) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Year, Value, color = Scenario)) + 
  geom_line() +
  labs(title = title, y = y_lab) 

#Little change to EOCY PE

variable = title  = "FGCyRel"
y_lab = "Outflow"
title = "Flaming Gorge CY Release"

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2022) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Year, Value, color = Scenario)) + 
  geom_line() +
  labs(title = title, y = y_lab) 
  
  # labs(title = df1$label[1]) + #, x="Count", y="Observed Rank") +
  # theme(plot.title = element_text(size=12)) + 
  # theme(axis.title.y=element_text("Pool Elevation (ft)")) #,axis.title = element_text(variable)) # +

variable = "FGWyRel"
y_lab = "Outflow"
title = "Flaming Gorge WY Release"

#CY release is nearly the same 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2022) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Year, Value, color = Scenario)) + 
  geom_line() +
  labs(title = title, y = y_lab) 

variable = "FGMayRel"
y_lab = "Outflow"
title = "Flaming Gorge May Release"

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2022) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Year, Value, color = Scenario)) + 
  geom_line() +
  labs(title = title, y = y_lab) 

dev.off()


