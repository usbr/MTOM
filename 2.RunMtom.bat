:: This batch file is meant to be used to run MTOM in batch mode using preconfigured input files 
::		and an initialized RiverWare model
:: Run RiverWare in Batch Mode and generate a log of model run diagnostic messages 

@echo off
echo ------------------------------------------------
echo This script runs MTOM 
echo ------------------------------------------------
:: Ask user input on whether to run model in MRM or single trace mode
set /p modelRunMode=Run MRM(1) or Single Trace(2) [1 or 2] ?: 
echo.
:: Write a textfile that controls how RiverWare is run in Batch Mode
echo Building RiverWare batch mode run script...
@echo off
@echo # Load the model> rwBatchScript.txt
@echo OpenWorkspace {RW Files/MTOM_Model.mdl}>> rwBatchScript.txt
@echo # Run the MRM simulation>> rwBatchScript.txt
:: Logic based on whether to run model in MRM or single trace
if %modelRunMode%==1 goto :mrmRun
if %modelRunMode%==2 goto :singleRun
:mrmRun
@echo StartController !MRM {Run_CBRFC_Ensemble_Fcst_RFC}>> rwBatchScript.txt
goto continueRWtext
:singleRun
:: Ask user input on which single trace year to run
set /p traceYear=Select desired trace year (1981 - 2010):  
set /A traceNum = %traceYear% - 1977
@echo StartController !MRM {Run_CBRFC_Ensemble_Fcst_RFC} {firstTrace=%traceNum%} {numTrace=1}>> rwBatchScript.txt
goto continueRWtext
:continueRWtext
@echo # Close MTOM and exit RiverWare>> rwBatchScript.txt
@echo CloseWorkspace>> rwBatchScript.txt
echo Done!
echo ------------------------------------------------
echo.
cd "Output Data"
del "EnsembleOutput.xlsx">nul 2>nul
del "MtomToCrss_Annual.xlsx">nul 2>nul
del "MtomToCrss_Monthly.xlsx">nul 2>nul
cd..
:: Ask user input on whether to send diagnostic messages to the console or a text file
set /p modelRunOutput=Output diagnostic to console(1) or text file(2) [1 or 2] ?: 
if %modelRunOutput%==1 goto :consoleOut
if %modelRunOutput%==2 goto :textOut
:textOut
echo Running MTOM please wait, this takes roughly 8 seconds per trace...
echo.
:: Line below runs RiverWare in Batch mode and outputs diagnostic messages to a text file
"C:\Program Files (x86)\CADSWES\RiverWare 6.6.3\riverware.exe" --batch rwBatchScript.txt --log "Output Data/batchLog.txt"
goto endMessage
:consoleOut
echo Running MTOM please wait, this takes roughly 8 seconds per trace...
echo.
:: Line below runs RiverWare in Batch mode and outputs diagnostic messages to the console window
"C:\Program Files (x86)\CADSWES\RiverWare 6.6.3\riverware.exe" --batch rwBatchScript.txt --log console
goto endMessage
:endMessage
del rwBatchScript.txt
echo MTOM run complete!
pause