# ===========================================
#   Contents
## 1. Set Up ##
## 2. User Input ##
## 3. Compare results for two ensemble simulations using 
#     predifined plots and 5 yr table ## 

#   Created by C. Felletter 5/2018
# ===========================================

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment 

## a) Install all the following if you don't have
# install.packages(tidyverse)
# install.packages(devtools)

#load libraries 
library(tidyverse) #ggplot2,dplyr,tidyr
library('devtools')

## b) Directory Set Up
MTOMDIR <- Sys.getenv("MTOM_DIR") #Use env variable 
scen_dir = file.path(MTOMDIR,"Output Data","RDF Process") 
#where scenarios are folder are kept
setwd(file.path(scen_dir)) #set to the folder 
#containing the sub folders for each ensemble

#OR just point to the folder 
#rdf_dir = "C:/Users/cfelletter/Documents/MTOM/rdfOutput" run folder
# setwd(rdf_dir) #set to the folder containing the sub folders for each ensemble

#see RWDATPlyr Workflow for more information 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

scenarios = my_scens =  c("PreviousRun", "CurrentRun") #scenarios are folder names for the rdfs from your different runs
# this is the order they will show up in the table & plot, so list the newest 
#run second there should only be 2 scenarios
# my_scens = c("PreviousRun", "CurrentRun") #names for your senarios to be plotted, 
#KEEP THESE SAME AS SCENARIOS, otherwise something is erroring
names(scenarios) = my_scens #naming 

first_ensemble = c(4,2) #filter out Most,Min,Max. For 38 trace offical = 4, 
#36 trace month w Most = 2. Same order as for scenarios  

# "switches" to create/not create different figures
makeFiguresAndTables <- TRUE
createSimple5yrTable <- TRUE

#Additional inputs 
yrs2show <- 2018:2022 # years to show the crit stats figures
peYrs <- 2017:2022 # years to show the Mead/Powell 10/50/90 figures for
mainScenGroup <- "CurrentRun" # the mainScenGroup is the scenario to use when creating 
#the current month's 5-year table, etc. In the plots, we want to show the 
#previous months runs, but in the tables, we only want the current month run. 

annText <- 'Results from CurrentRun MTOM Run' # text that will be added to figures
yy5 <- 2018:2022 # years to use for the simple 5-year table
tableFootnote <- NA # for the simple 5-year table, this should either be a 
#footnote corresponding to one of the my_scens names or NA

#Seldom modified inputs
colorLabel <- 'Scenario' #color plots based on 
legendWrap <- 20 # setting to NULL will not wrap legend entries at all

#file names 
sysCondTable <- paste0('SysTableFull',yrs2show[1],'_',tail(yrs2show,1),'.csv') 
# file name for the system conditions procssed file
eocyFigs <- 'MPEOCY.pdf' 
critStatsProc <- 'CritStats.csv'
critFigs <- 'CritFigs2022.pdf'
simple5YrFile <- '5yrSimple.pdf'
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Compare results for two ensemble simulations using predifined plots 
#     and 5 yr table ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Additional plotting functions and libraries 
#Install RWDataPlyr

if("RWDataPlyr" %in% installed.packages()){
  library("RWDataPlyr")
  #see https://github.com/BoulderCodeHub/RWDataPlyr
  #look at sample work flow
  vignette("rwdataplyr-workflow", package = "RWDataPlyr")
} else {
  devtools::install_github("BoulderCodeHub/RWDataPlyr", build_vignettes = TRUE)
  library("RWDataPlyr")
  #see https://github.com/BoulderCodeHub/RWDataPlyr
  #look at sample work flow
  vignette("rwdataplyr-workflow", package = "RWDataPlyr")
}

if("CRSSIO" %in% installed.packages()){
  library(CRSSIO)
} else {
  devtools::install_github("BoulderCodeHub/CRSSIO")
  library(CRSSIO)
}

if(packageVersion("CRSSIO") < "0.6.0"){
  detach("package:CRSSIO")
  devtools::install_github("BoulderCodeHub/CRSSIO")
  library(CRSSIO)
}
source('plottingFunctions.R') #plotEOCYElev #added csVarNames()

# some sanity checks that UI is correct:
if(!(mainScenGroup %in% names(scenarios))) 
  stop(mainScenGroup, ' is not found in scens.')

# check folders
if(!file.exists(scenarios[1]) | !file.exists(scenarios[2]))
  stop('Scenarios folder(s) do not exist or scen_dir is set up incorrectly. 
       Please ensure Scenarios is set correctly.')

if (!file.exists('results')) {
  message(paste(file.path(scen_dir, 'results'),
                'does not exist. Creating this folder...'))
  dir.create(file.path(scen_dir, 'results'))
}

oFigs <- file.path(scen_dir,'results',mainScenGroup) 
if (!file.exists(oFigs)) {
  message(paste('Creating folder:', oFigs))
  dir.create(oFigs)
}

message('Figures and tables will be saved to: ', oFigs)

# *****************************************************************************
#       Process results --------------
# *****************************************************************************
# should be have set scenario colm names above 
# names(scenarios) = my_scens #set scenarios names equal to my_scens 

## Create tables, figures, and data behind figures
if(makeFiguresAndTables){
  
  message('starting getSysCondData')
  #read the MTOM rwd_agg table #### VERIFY LB Surplus, only found Flood Control Slot
  sys_rwa <- rwd_agg(read.csv("MTOM_sys_rwa.csv", stringsAsFactors = FALSE)) 
  # aggregate for system conditions table
  sysCond <- rw_scen_aggregate(
    scenarios = scenarios,
    agg = sys_rwa,
    scen_dir = scen_dir#,
    # scen_names = my_scens
  ) %>% 
  
    #filter out Most,Min,Max
    dplyr::filter((Scenario == scenarios[1] & TraceNumber >= first_ensemble[1]) |
                    (Scenario == scenarios[2] & TraceNumber >= first_ensemble[2])) 
  
  
  message("creating system conditions table")
  
  sysCond_Curr <-  sysCond %>%
  # trim to specified years and the current main scenario group 
  dplyr::filter(Year %in% yrs2show & Scenario == mainScenGroup)
  # create the system cond. table
  sysTable <- CRSSIO::crsso_get_sys_cond_table(sysCond_Curr, yrs2show)
  # save the sys cond table
  data.table::fwrite(
    as.data.frame(sysTable[['fullTable']]), 
    file.path(oFigs,sysCondTable), 
    row.names = TRUE
  )
  
  # 2) Plot Mead, Powell EOCY elvations and include previous month's results too
  #read the agg table 
  message('starting getPeData')
  
  pe_rwa <- rwd_agg(read.csv("MPPEStats.csv", stringsAsFactors = FALSE)) 
  # aggregate for system conditions table
  pe <- rw_scen_aggregate(
    scenarios = scenarios,
    agg = pe_rwa,
    scen_dir = scen_dir#,
    # scen_names = my_scens
  ) %>% 
    
    #filter out Most,Min,Max
    dplyr::filter((Scenario == scenarios[1] & TraceNumber >= first_ensemble[1]) |
                    (Scenario == scenarios[2] & TraceNumber >= first_ensemble[2])) %>%
  
    #The StartMonth column is used as the color variable in plotEOCYElev, and 
    # the names that should show up in the legend/differentiate scenario groups
    # are stored in the Scenario Varaible. So easiest to just copy it from Scenario to 
    # StartMonth for now
    dplyr::mutate(StartMonth = Scenario)
  
  message("EOCY elevation figures")
  
  #plot
  powellPE <- plotEOCYElev(pe, peYrs, 'Powell.Pool Elevation', 
                           'Powell End-of-December Elevation', colorLabel,
                           legendWrap = legendWrap)
  
  meadPE <- plotEOCYElev(pe, peYrs, 'Mead.Pool Elevation', 
                         'Mead End-of-December Elevation', colorLabel, 
                         legendWrap = legendWrap)
  
  pdf(file.path(oFigs,eocyFigs), width = 8, height = 6)
  print(powellPE)
  print(meadPE)
  dev.off()
  
  rm(powellPE, meadPE)
  
  
  # 3) Critical elevation thresholds; figures and data table
  # have sysCond for some, and read in crit stats for others
  message("starting critical stats")
  
  # compare crit stats for all scenarios
  # call once each for powell LT 3490, shortage, and surplus
  # get the necessary variables by filtering from the pe and syscond data files
  cs <- pe %>%
    filter(
      Variable %in% c('meadLt1000', 'meadLt1020', 'powellLt3490', 
                      'powellLt3525', 'meadLt1025')
    ) %>%
    mutate(AggName = Scenario) %>% #adds new variable AggName and preserve existing
    select(-StartMonth)
  
  # rm(pe) # don't need pe any longer
  
  cs <- sysCond %>%
    mutate(AggName = Scenario) %>%
    filter(Variable %in% c('lbSurplus', 'lbShortage')) %>%
    mutate(AggName = Scenario) %>%
    rbind(cs)
  
  # unique(cs$Variable) #check I got all the variables 
  
  ptitle <- paste(
    'Powell: Percent of Traces Less than Power Pool', 
    "(elevation 3,490\') in Any Water Year",
    sep = "\n"
  )
  
  p3490Fig <- compareCritStats(
    cs, 
    yrs2show, 
    'powellLt3490', 
    '', 
    ptitle, 
    colorLabel, 
    legendWrap = legendWrap
  )
  
  shortTitle <- 'Lower Basin: Percent of Traces in Shortage Conditions'
  shortFig <- compareCritStats(cs, yrs2show, 'lbShortage', '', shortTitle, 
                               colorLabel, legendWrap = legendWrap)
  surpTitle <- 'Lower Basin: Percent of Traces in Surplus Conditions'
  surpFig <- compareCritStats(cs, yrs2show, 'lbSurplus', '', surpTitle, 
                              colorLabel, legendWrap = legendWrap)
  
  # now create figures only for the current "main scenario"
  # defaults are ok for legendTitle, legLoc, nC, and annSize
  # drop Mead LT 1025 from one plot and Mead LT 1020 from 
  # the other plot
  
  critStatsFig1 <- plotCritStats(dplyr::filter(
    cs, 
    Scenario == mainScenGroup, 
    !(Variable %in% c('meadLt1020','lbSurplus'))
  ), 
  yrs2show, 
  annText
  )
  
  critStatsFig2 <- plotCritStats(dplyr::filter(
    cs, 
    Scenario == mainScenGroup, 
    !(Variable %in% c('meadLt1025','lbSurplus'))
  ), 
  yrs2show, 
  annText
  )
  
  csVars <- csVarNames()
  # create data table to save crit stats
  cs_tbl <- cs %>%
    dplyr::filter(
      Year %in% yrs2show, 
      Scenario == mainScenGroup, 
      Variable != 'lbSurplus'
    ) %>%
    # compute the percent of traces by averaging values 
    group_by(Year,Variable) %>%
    summarise(Value = mean(Value)) %>%
    dplyr::mutate(vName = csVars[Variable]) %>%
    # reshape to be easier to print out
    ungroup() %>%
    select(-Variable) %>%
    tidyr::spread(vName, Value)
  
  # shortage surplus figure
  # defaults ok for legendTitle, nC, and legLoc
  ssPlot <- plotShortageSurplus(
    dplyr::filter(
      sysCond, 
      Variable %in% c('lbShortage', 'lbSurplus'),
      Scenario == mainScenGroup
    ), 
    yrs2show, 
    mainScenGroup
  )
  
  # stacked barplot of different shortage tiers
  # default for annSize is ok
  shortStack <- plotShortStackedBar(
    dplyr::filter(
      sysCond, 
      Variable %in% c('lbShortageStep1','lbShortageStep2','lbShortageStep3'),
      Scenario == mainScenGroup
    ), 
    yrs2show, 
    annText
  )
  
  # save figures and table
  message("creating critFigs pdf")
  pdf(file.path(oFigs,critFigs),width = 8, height = 6)
  print(p3490Fig)
  print(shortFig)
  print(surpFig)
  print(critStatsFig1)
  print(critStatsFig2)
  print(ssPlot)
  print(shortStack)
  dev.off()
  data.table::fwrite(cs_tbl,file.path(oFigs,critStatsProc),row.names = F)
}

# 5 year simple table -------------------------
if(createSimple5yrTable){
  ## create the 5-yr simple table that compares to the previous run
  message("creating 5-year simple table")
  zz <- cs %>%
  mutate(Agg = Scenario)  #adds new variable AggName and preserve existing
  simple5Yr <- create5YrSimpleTable(zz, names(scenarios), yy5, tableFootnote)
  pdf(file.path(oFigs,simple5YrFile),width = 8, height = 8)
  print(simple5Yr)
  dev.off()
  rm(zz)
}
