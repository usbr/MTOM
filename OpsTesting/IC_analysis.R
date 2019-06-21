## Reservoir Capacity - based on elevation
rm(list=setdiff(ls()))

library(openxlsx)
library(tidyverse)
library(lubridate)

data_dir = 'C:/Users/sabaker/Projects/Models/MTOM/MTOM_Dir/Input Data'
plot_dir = 'C:/Users/sabaker/Projects/Models/MTOM/MTOM_Dir/OpsTesting'

# resV = c('Fontenelle', 'FlamingGorge', 'TaylorPark', 'BlueMesa', 'MorrowPoint',
#          'Crystal', 'Vallecito', 'Navajo', 'Powell', 'Mead', 'Mohave', 'Havasu')

res_cap = list(
  Fontenelle = c('Fontenelle', 6408, 6519),
  FlamingGorge =c('FlamingGorge', 5704, 6050),
  TaylorPark = c('TaylorPark', 9180, 9344),
  BlueMesa = c('BlueMesa', 7358, 7528),
  MorrowPoint = c('MorrowPoint', 6808, 7200),
  Crystal = c('Crystal', 6670, 6800),
  Vallecito = c('Vallecito', 7582.5, 7665),
  Navajo = c('Navajo',5775, 6107.5 ), 
  Powell = c('Powell', 3370, 3711.5),
  Mead = c('Mead', 895, 1250), 
 Mohave = c('Mohave', 570, 650), 
  Havasu = c('Havasu', 400, 451)
)

## Read in IC data
setwd(data_dir)
df_IC = read.xlsx('MTOM_OpsTesting_IC.xlsx', sheet = 'Reservoirs')
df_IC$Date = as.POSIXct(df_IC$Date*86400, origin = '1900-01-01') #date in correct format

# get breaks and labels
breaksX = NULL
for (i in 1:(nrow(df_IC)/12)) {breaksX = c(breaksX, (i-1)*12+1) }

## plot PE of all reservoirs of IC
plist = list()
for (res_i in resV) {
  df_i = df_IC[c('Date', paste0(res_i, '.Pool.Elevation'))]
  colnames(df_i)[2] <- 'PE'
  df_i$Date_in = 1:nrow(df_i)
  PE.max = as.numeric(res_cap[[res_i]][3]); PE.min = as.numeric(res_cap[[res_i]][2]) #PE constraints
  
  plist[[res_i]] <- ggplot(df_i, aes(x = Date_in, y = PE)) + theme_bw() + 
    geom_line() + ylab(res_i) + xlab('') +
    scale_x_continuous(breaks = breaksX + 6, labels = year(df_i$Date[breaksX + 1])) +
    scale_y_continuous(limits = c(PE.min, PE.max)) +
    geom_vline(xintercept = breaksX, color = 'red', linetype = 'dashed') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}

# combine and save figure
gall = grid.arrange(grobs = plist)
setwd(plot_dir)
ggsave(gall, filename = 'IC_fullPE.png', width = 20, height = 8)


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
gall = grid.arrange(grobs = plist)
setwd(plot_dir)
ggsave(gall, filename = 'IC_3yr.png', width = 8, height = 11)


