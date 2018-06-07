Created by Conor Felletter on 6/2018.
Instructions for using RDF Processing Scripts
1. Run MTOM ensembles. New control file generates RDFs in folder ‘rdfOutput.’ Files are not tracked in rdfOutput so you must move them manually to a sub folder in ‘\Output Data\RDF Process’. R code is currently setup to use files from ‘RDF Process\CurrentRun’ & ‘PreviousRun’ but use of other folders is possible by modifying the R code. 
2. Open ‘MTOM_Compare_5Yr_RWDataPlyr.R’ in ‘\Output Data\RDF Process’ in RStudio. Ensure libraries are installed. 
3. Modify User Inputs section pay attention to:
a. Names of Scenarios variable
b. first_ensemble variable which indicates if ensemble has Most,Min,Max traces to ignore
c. Years to show on plots
Note: Comments in R are indicated by a # and provide useful information about code. 
4. Run R Code
a. If haven’t installed packages open ‘Install_Packages.R’ and run (ctrl + alt + r). 
b. Run ‘MTOM_Compare_5Yr_RWDataPlyr.R’ (ctrl + alt + r). 
5. Results are store in ‘results’ folder. 
6. Questions: cfelletter@usbr.gov
