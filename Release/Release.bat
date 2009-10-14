@echo off

cd %APSIM%\Model\Build

rem ----------------------------------------------
rem Run Plant2Documentation on all PLANT 2
rem model configurations
rem ----------------------------------------------
call DoPlant2Documentation.bat


rem ----------------------------------------------
rem Probe all models for variables.
rem ----------------------------------------------
call ProbeAll


rem ----------------------------------------------
rem Create documentation index.
rem ----------------------------------------------
cd %APSIM%\Documentation
call CreateIndex.bat


rem ----------------------------------------------
rem Create a release installation.
rem ----------------------------------------------
cd %APSIM%\Release
"C:\Program Files\Wise Installation System\wise32" /s /c apsim.wse
