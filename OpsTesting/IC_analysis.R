## Reservoir Capacity - based on elevation

res_cap = list(
  c('Fontenelle', 6408, 6519),
  c('FlamingGorge', 5704, 6050),
  c('Taylor Park', 9180, 9344),
  c('BlueMesa', 7358, 7528),
  c('MorrowPoint', 6808, 7200),
  c('Crystal', 6670, 6800),
  c('Vallecito', 7582.5, 7665),
  c('Navajo',5775, 6107.5 ), 
  c('Powell', 3370, 3711.5),
  c('Mead', 895, 1250)
)

## Read in IC data
setwd('C:\Users/sabaker/Projects/Models/MTOM/MTOM_Dir/Input Data')
df_IC = read.xlsx('MTOM_OpsTesting_IC.xlsx', sheet = 'Reservoirs')