# ===========================================
#   read in plot data from rdf 
#   Contents
## 1. Set Up ##
## 2. Results for a single slot in an ensemble ##
## 3. Results for multiple slots in an ensemble ##
## 4. Compare results for two ensemble simulations for custom slots ##
## 5. Compare results for two ensemble simulations using predifined plots and 5 yr table ## 

#   Created by C. Felletter 5/2018
# ===========================================

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Install all the following if you don't have
# install.packages(ggplot2)
# install.packages(tidyr)
# install.packages(devtools)

#load libraries 
library(tidyverse) #ggplot2,dplyr,tidyr


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
# setwd(paste0(MTOMDIR,"/rdfOutput/")) #set to the folder containing the 
# sub folders for each ensemble
setwd(file.path(MTOMDIR,"Output Data","RDF Process")) #set to the folder 
#containing the sub folders for each ensemble

#OR just point to the folder 
#rdf_dir = "C:/Users/cfelletter/Documents/MTOM/rdfOutput" run folder
# setwd(rdf_dir) #set to the folder containing the sub folders for each ensemble

## input
scenarios = c("April", "May") #scenarios are folder names for the rdfs from your different runs
# this is the order they will show up in the table & plot, so list the newest 
#run second there should only be 2 scenarios
my_scens = c("April", "May") #names for your senarios to be plotted, 
#KEEP THESE SAME AS SCENARIOS, otherwise something is erroring
names(scenarios) = my_scens #naming #must name these for code in Sect 4 & 5 

scen_dir = file.path(MTOMDIR,"Output Data","RDF Process") 
#where scenarios are folder are kept
#see RWDATPlyr Workflow for more information 

first_ensemble = c(4,2) #filter out Most,Min,Max. For 38 trace offical = 4, 
#36 trace month w Most = 2. Same order as for scenarios  


### SEE SECTION 5 FOR ADDITIONAL INPUTS ###

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. Results for a single slot in an ensemble ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
slot = "Powell.Outflow"
file = "res.rdf" #"MTOM.rdf"

#read in the rdf
rdf <- read_rdf(iFile = paste0(scenarios[1],"/",file))

# ensure the slot you want is in the rdf:
rdf_slot_names(rdf)

# then get the minimum annual outflow for all 5 years and traces
res_tbl <- rdf %>% #this means "pipe" the data to the next function 
  rdf_to_rwtbl() %>%
  filter(ObjectSlot == slot) %>% 
  #filter out Most,Min,Max
  filter(TraceNumber >= first_ensemble[1]) %>% 
  group_by(Year) %>% 
  summarise(Value = sum(Value))

print(res_tbl) #look at output

rm(res_tbl) #delete

#data starting in January can use the below code but 
#currently not set up for starts in months other than January
# rdf %>% 
#   rdf_get_slot(slot) %>%
#   rwslot_annual_sum()

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Results for multiple slots in an ensemble ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#specify the rwd_agg object which defines what summaries will be created 

rwa1 <- rwd_agg(read.csv("rw_agg_PowMead.csv", stringsAsFactors = FALSE)) 
?rwd_agg #function name: cy, wy, eocy, eowy, full month name, asis 
#see RWDATPlyr Workflow for more information 

# #After the rwd_agg is specified the object is passed to rdf_aggregate() 
#along with a few parameters specifying where the data are stored, and a tbl_df is returned:
res <- rdf_aggregate(
  rwa1,
  rdf_dir = scenarios[1] #which senario/run folder?
) %>% 
  
  #filter out Most,Min,Max
  dplyr::filter(TraceNumber >= first_ensemble[1]) 
  
View(res) #look at output

rm(res) #delete

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Compare results for two ensemble simulations for custom slots ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
names(scenarios) = my_scens #naming #must name these

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, 
#essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() 
#it relies on a user specified rwd_agg object to know how to summarize and 
#process the scenarios. The scen_dir argument should point to the directory were 
#rdf files from different month runs are stored. rw_scen_aggregate() also 
#needs to know the scenario folders and allows the user to specify scenario names 
#that may differ from the folder name so they are easier to use in R. 

scen_res <- rw_scen_aggregate(
  scenarios = scenarios,
  agg = rwa1,
  scen_dir = scen_dir
) %>% 
  
  #filter out Most,Min,Max
  dplyr::filter((Scenario == scenarios[1] & TraceNumber >= first_ensemble[1]) |
                  (Scenario == scenarios[2] & TraceNumber >= first_ensemble[2])) 

unique(scen_res$Variable) #check variable names 

## plot 
pdf(paste0('results/May/Powell_MTOM_Example.pdf'), width=9, height=6)

variable = "PowellEocy"
y_lab = "Pool Elevation"
title = "Powell EOCY PE"

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2022) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Year, Value, color = Scenario)) + 
  geom_line() +
  labs(title = title, y = y_lab) 

variable = "MeadEocy"
y_lab = "Pool Elevation"
title = "Powell EOCY PE"

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2022) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Year, Value, color = Scenario)) + 
  geom_line() +
  labs(title = title, y = y_lab) 

dev.off()

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. Compare results for two ensemble simulations using predifined plots and 5 yr table ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# "switches" to create/not create different figures
makeFiguresAndTables <- TRUE
createSimple5yrTable <- TRUE

#Additional inputs 
yrs2show <- 2018:2022 # years to show the crit stats figures
peYrs <- 2017:2022 # years to show the Mead/Powell 10/50/90 figures for
mainScenGroup <- "May" # the mainScenGroup is the scenario to use when creating 
#the current month's 5-year table, etc. In the plots, we want to show the 
#previous months runs, but in the tables, we only want the current month run. 
colorLabel <- 'Scenario' #color plots based on 
legendWrap <- 20 # setting to NULL will not wrap legend entries at all
annText <- 'Results from May 2018 MTOM Run' # text that will be added to figures
yy5 <- 2018:2022 # years to use for the simple 5-year table
tableFootnote <- NA # for the simple 5-year table, this should either be a 
#footnote corresponding to one of the my_scens names or NA

#file names 
sysCondTable <- paste0('SysTableFull',yrs2show[1],'_',tail(yrs2show,1),'.csv') 
# file name for the system conditions procssed file
eocyFigs <- 'MPEOCY.pdf' 
critStatsProc <- 'CritStats.csv'
critFigs <- 'CritFigs2022.pdf'
# condProbFile <- 'CondProbs.csv'
# shortCondFig <- 'shortConditionsFig.pdf'
simple5YrFile <- '5yrSimple.pdf'
# traceMap <- read.csv('data/Trace2IcMap.csv')
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#Additional plotting functions and libraries 
library(CRSSIO)
if(packageVersion("CRSSIO") < "0.6.0"){
  detach("package:CRSSIO")
  devtools::install_github("BoulderCodeHub/CRSSIO")
  library(CRSSIO)
}
source('plottingFunctions.R') #plotEOCYElev #added csVarNames()
# source('getCondProbs.R')
# source('plotFirstYearShortCond.R')
# source('getScenarioData.R')
# source('dataTaggingFunctions.R')
# source('getICPEData.R')

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
                    (Scenario == scenarios[2] & TraceNumber >= first_ensemble[2]))     %>%
  
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
  
  unique(cs$Variable) #check I got all the variables 
  
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


# NOT ENABLED -------------------------


# if(computeConditionalProbs){
#   ## CONDITIONAL PROBABILITIES
#   # use sysCond
#   if(is.na(match('sysCond',ls()))){
#     sysCond <- read.table(paste0(oFigs,mainScenGroup,sysCondTable),header = T) 
#     sysCond <- dplyr::filter(sysCond, Year %in% yrs2show & Agg == 1)
#     sysTable <- CRSSIO::createSysCondTable(sysCond, yrs2show)
#   }
#   
#   sysCond <- sysCond %>% 
#   mutate(Trace = TraceNumber) #adds new variable and preserve existing
#     
#   
#   cp1 <- getConditionalProbs(
#     sysCond, 
#     yrs2show[1], 
#     yrs2show[1], 
#     'lbShortage',
#     'mer748'
#   )
#   cp2 <- getConditionalProbs(
#     sysCond, 
#     yrs2show[1], 
#     yrs2show[1], 
#     'lbShortage',
#     'ueb823'
#   )
#   cp3 <- getConditionalProbs(
#     sysCond, 
#     yrs2show[1],
#     yrs2show[1], 
#     'lbShortage',
#     c('eq','uebGt823')
#   )
#   cp4 <- getConditionalProbs(
#     sysCond, 
#     yrs2show[2], 
#     yrs2show[1], 
#     c('lbShortage','lbShortageStep1','lbShortageStep2', 'lbShortageStep3'), 
#     'mer748'
#   )
#   cp5 <- getConditionalProbs(
#     sysCond, 
#     yrs2show[2], 
#     yrs2show[1], 
#     c('lbShortage','lbShortageStep1','lbShortageStep2', 'lbShortageStep3'), 
#     'ueb823'
#   )
#   cp6 <- getConditionalProbs(
#     sysCond, 
#     yrs2show[2], 
#     yrs2show[1], 
#     c('lbShortage','lbShortageStep1','lbShortageStep2', 'lbShortageStep3'), 
#     c('eq','uebGt823')
#   )
#   
#   # create data table from the above values
#   cpt1 <- data.frame(
#     'ChanceOf' = c(paste(yrs2show[1],names(cp1)),paste(yrs2show[2],names(cp4))),
#     'PrctChance' = c(cp1,cp4)
#   )
#   rr <- which(
#     rownames(sysTable$fullTable) == 
#       'Mid-Elevation Release Tier - annual release = 7.48 maf'
#   )
#   cc <- which(colnames(sysTable$fullTable) == yrs2show[1])
#   cpt1$PowellWYRel <- paste('7.48 MAF;',sysTable$fullTable[rr,cc])
#   
#   cpt2 <- data.frame(
#     'ChanceOf' = c(paste(yrs2show[1],names(cp2)),paste(yrs2show[2],names(cp5))),
#     'PrctChance' = c(cp2,cp5)
#   )
#   rr <- which(
#     rownames(sysTable$fullTable) == 
#       "Upper Elevation Balancing - annual release = 8.23 maf"
#   )
#   cpt2$PowellWYRel <- paste('8.23 MAF;',sysTable$fullTable[rr,cc])
#   
#   cpt3 <- data.frame(
#     'ChanceOf' = c(paste(yrs2show[1],names(cp3)),paste(yrs2show[2],names(cp6))),
#     'PrctChance' = c(cp3,cp6)
#   )
#   rr <- which(
#     rownames(sysTable$fullTable) == 
#       "Upper Elevation Balancing - annual release > 8.23 maf"
#   )
#   rr2 <- which(
#     rownames(sysTable$fullTable) == "Equalization - annual release > 8.23 maf"
#   )
#   cpt3$PowellWYRel <- paste(
#     '> 8.23 MAF;',
#     sysTable$fullTable[rr,cc] + sysTable$fullTable[rr2,cc]
#   )
#   
#   cpt1 <- rbind(cpt1,cpt2,cpt3)
#   
#   # rearrange columns
#   cpt1 <- cpt1[c('PowellWYRel','ChanceOf','PrctChance')]
#   cpt1$PrctChance <- cpt1$PrctChance*100
#   data.table::fwrite(cpt1,paste0(oFigs,condProbFile),row.names = F)
# }

# # conditions leading to shortage ---------------------------------
# # pulled annotation out of generic function
# if (createShortConditions) {
#   if (length(resFile) > 1)
#     stop("conditions leading to shortage is only designed to work with 1 scenario of data, at this point")
#   
#   message(
#     'Using hard coded values for the arrow in the shortage conditions figure.\n',
#     'You may need to update the values and re-run main.R'
#   )
#   # filterOn being set to pe shows results for traces that are <= 1077
#   shortCond <- plotFirstYearShortCond(
#     conditionsFrom, 
#     resFile, 
#     scenario, 
#     filterOn = 'pe', 
#     yearToAnalyze
#   )
#   shortCond <- shortCond + 
#     annotate('segment', x = 5.55, xend = 4.3, y = 1069.6, yend = 1068.85, 
#              arrow = grid::arrow(length = unit(.3,'cm')),size = 1) +
#     annotate('text', x = 5.65, y = 1069.6,label = lbLabel, size = 4, hjust = 0) +
#     ggtitle(shortCondTitle, subtitle = shortCondSubTitle) +
#     theme(legend.title = element_text(size = 10))
#   
#   pdf(file.path(oFigs,shortCondFig),width = 9, height = 6)
#   print(shortCond)
#   dev.off()
# }



