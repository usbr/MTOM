## check which file aborts in RiverSMART scenarios???


## specific scenario
scen_nm = 'UB.ClimoISM_LB.Hist_Forced_MTOM_Rules_fcst-1982-05'
dir = file.path(Sys.getenv('MTOM_DIR'), "Working", "RiverWare", scen_nm)
setwd(dir)
fls = list.files()

for (i in 1:length(fls)) {
  if (any(grepl("ABORT", readLines(fls[i])))) {
    print(fls[i])
    flRead = readLines(fls[i])
    # flRead[which(grepl("Abort", flRead))]
    print(tail(flRead, n = 13))
  }
}



## all scenarios in working
workdir = file.path(Sys.getenv('MTOM_DIR'), "Working", "RiverWare")
dirs = list.dirs(workdir)

for (j in 2:length(dirs)) {
  setwd(dirs[j])
  print('---------------------'); print(dirs[j])
  fls = list.files()
  
  for (i in 1:length(fls)) {
    if (any(grepl("ABORT", readLines(fls[i])))) {
      print(fls[i])
      flRead = readLines(fls[i])
      # flRead[which(grepl("Abort", flRead))]
      print(tail(flRead, n = 13))
    }
  }
}
