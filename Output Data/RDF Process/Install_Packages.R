install.packages('ggplot2')
install.packages('tidyverse')

#Install RWDataPlyr & CRSSIO
install.packages('devtools')
devtools::install_github('BoulderCodeHub/CRSSIO')
devtools::install_github("BoulderCodeHub/RWDataPlyr", build_vignettes = TRUE)
#see https://github.com/BoulderCodeHub/RWDataPlyr
#look at sample work flow
vignette("rwdataplyr-workflow", package = "RWDataPlyr")