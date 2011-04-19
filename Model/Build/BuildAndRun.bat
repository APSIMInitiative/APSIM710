@echo off
rem ------------------------------------------------------------------------------
rem Top level batch file for doing a build and run.
rem ------------------------------------------------------------------------------
pushd ..\..
set APSIM=%CD%


rem ------------------------------------------------------------------------------
rem Create a simple HTML file that will go on the desktop and display info about
rem this build.
rem ------------------------------------------------------------------------------
echo Build and run start time: %DATE% (%TIME%) > %APSIM%\Model\Build\Build.log


rem ----------------------------------------------
rem Clean out everything
rem ----------------------------------------------
del /S %APSIM%\*.out 2>nul >nul
del /S %APSIM%\*.sum 2>nul >nul
del /S %APSIM%\*.conversions 2>nul >nul


rem ----------------------------------------------
rem Compile everything
rem ----------------------------------------------
cd %APSIM%\Model\Build
call BuildAll.bat

rem ----------------------------------------------
rem Run Plant2Documentation on all PLANT 2
rem model configurations
rem ----------------------------------------------
cd %APSIM%\Build\
call DoPlant2Documentation.bat


rem ----------------------------------------------
rem Probe all models for variables.
rem ----------------------------------------------
cd %APSIM%\Model
UpdateDotNetProxy


rem ----------------------------------------------
rem Create documentation index.
rem ----------------------------------------------
cd %APSIM%\Documentation
call CreateIndex.bat


rem ----------------------------------------------
rem Create a release installation
rem ----------------------------------------------
cd %APSIM%\Release\
call Release.bat

rem ----------------------------------------------
rem Run everything
rem ----------------------------------------------
cd %APSIM%\Model\Build\
call RunAll.bat

rem ------------------------------------------------------------------------------
rem Finish creating a simple HTML file that will go on the desktop
rem ------------------------------------------------------------------------------
cd %APSIM%\Model\Build
echo Build and run finish time: %DATE% (%TIME%) >> Build.log
echo. >> Build.log
echo. >> Build.log
copy /Y Build.log+Build.out Build.tmp >nul
del /Q Build.out 2> nul
del /Q Build.log 2> nul
ren Build.tmp Build.out

set APSIM=
popd
