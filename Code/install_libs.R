listOfPackages <- c("RWDataPlyr", "rlang","validate","devtools","tidyverse", "lubridate", "RWcheck")

for (i in listOfPackages){
  if( !(i %in% installed.packages()) )
    if (i == 'RWcheck' ) {
      devtools::install_github("BoulderCodeHub/RWcheck")
    } else {
      install.packages(i, dependencies = TRUE)
    }
}
