:: This batch file is meant to be used to run MTOM in batch mode using preconfigured input files 
::		and an initialized RiverWare model
:: Set MTOM environment variable MTOM_DIR to current directory

@echo off
echo ------------------------------------------------
echo This script sets MTOM_DIR to the directory that 
echo   currently contains this script file. 
echo ------------------------------------------------
echo.
echo Setting local MTOM_DIR environment variable to:
echo %cd%
echo.
echo ------------------------------------------------
setx MTOM_DIR "%cd%
echo.
echo ------------------------------------------------
pause