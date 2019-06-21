## Reservoir Capacity - based on elevation
rm(list=setdiff(ls()))

library(openxlsx)
library(tidyverse)
library(lubridate)

data_dir = 'C:/Users/sabaker/Projects/Models/MTOM/MTOM_Dir/Input Data'
plot_dir = 'C:/Users/sabaker/Projects/Models/MTOM/MTOM_Dir/OpsTesting'

# resV = c('Fontenelle', 'FlamingGorge', 'TaylorPark', 'BlueMesa', 'MorrowPoint',
#          'Crystal', 'Vallecito', 'Navajo', 'Powell', 'Mead', 'Mohave', 'Havasu')


# source('C:/Users/SABaker/Projects/2.MTOM Testbed/Analysis/Rscripts/0_inputVars.R')

## === Input
# type_vec = c('CY', 'WY')
# res_vec = c('Powell')
# score_vec = c('RPSS','CRPSS')
# yrs = 1981:2016      # years analyzed
# rpss_yrs = 1981:2010 # climo - annual flows for comparison in RPSS

## overwrite input file
fcstType = 'ESP'
resValue = 'StreamflowForecast'
res_i = "PowellInflow.Unregulated"; timePeriod = 'annual'; yr_type_i = c('CY')
# res_i = "TaylorPark.Inflow"; timePeriod = 4:7; type_vec = 'CY'

## read ESP data
rawData_dir =  "C:/Users/sabaker/Projects/2.MTOM Testbed/Analysis/data/Testbed_output/1_rawRDS/"
setwd(rawData_dir)
df_in = readRDS(paste0('1_raw_', resValue, '_',fcstType, '.rds'))

df_ann = df_in %>% mutate(year = year(Timestep)) %>%
  group_by(Run.Date, year, Trace.Number) %>%
  summarise(annFlow = sum(PowellInflow.Unregulated))


## graph pdf of flows for imonth
imon = 1
df_annI = df_ann %>% filter(month(Run.Date) == imon)

breaksX = seq(0, 30000, 5000)
ggplot(df_annI) + geom_density(aes(annFlow)) + theme_bw() + 
  scale_x_continuous(breaks = breaksX, expand = c(0,0)) 

# quantiles of imon for run date and year
df_quant = df_annI %>% 
  summarise(q0 = quantile(annFlow, 0),
            q10 = quantile(annFlow, 0.1),
            q30 = quantile(annFlow, 0.3),
            q50 = quantile(annFlow, 0.5),
            q70 = quantile(annFlow, 0.7),
            q90 = quantile(annFlow, 0.9),
            q100 = quantile(annFlow, 1.0))

df_annMin = df_annI %>% filter(annFlow < quantile(annFlow, 0.1))
table(df_annMin[c('year', 'Trace.Number')])

sort(table(df_annMin[c('Trace.Number')]))
sort(table(df_annMin[c('year')]))  





## === Filter to 3 years of potential IC
yrsIC = c(1983,2002,2015) #high PE, transitional year (low to high), low PE year
df_lim = df_IC %>% #mutate(yr = Date %m+% months(1))
  filter(year(Date %m+% months(1)) %in% yrsIC)

# get breaks and labels
breaksX = NULL
for (i in 1:(nrow(df_lim)/12)) {breaksX = c(breaksX, (i-1)*12+1) }

## plot PE of all reservoirs of IC
plist = list()
for (res_i in resV) {
  df_i = df_lim[c('Date', paste0(res_i, '.Pool.Elevation'))]
  colnames(df_i)[2] <- 'PE'
  df_i$Date_in = 1:nrow(df_i)
  PE.max = as.numeric(res_cap[[res_i]][3]); PE.min = as.numeric(res_cap[[res_i]][2]) #PE constraints
  
  plist[[res_i]] <- ggplot(df_i, aes(x = Date_in, y = PE)) + theme_bw() + 
    geom_line() + ylab(res_i) + xlab('') +
    scale_x_continuous(breaks = breaksX + 6, labels = year(df_i$Date[breaksX + 1])) +
    scale_y_continuous(limits = c(PE.min, PE.max), expand = c(0,0)) +
    geom_vline(xintercept = breaksX, color = 'red', linetype = 'dashed') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}

# combine and save figure
# gall = grid.arrange(grobs = plist)
# setwd(plot_dir)
# ggsave(gall, filename = 'IC_3yr.png', width = 8, height = 11)


