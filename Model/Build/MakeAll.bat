@echo off
rem -------------------------------------------------------------
rem This batch file compiles everything
rem -------------------------------------------------------------
pushd ..\..
set APSIM=%CD%

rem -------------------------------------------------------------
rem Remove the old build.out file.
rem -------------------------------------------------------------
cd %APSIM%\Model\Build
del /Q build.out 2> nul

rem -------------------------------------------------------------
rem Get runtime dlls for bootstrap progs
rem -------------------------------------------------------------
call MakeProject RunTime

rem -------------------------------------------------------------
rem Remove old VersionInfo files
rem -------------------------------------------------------------
del /Q VersionInfo.* 2> nul

rem ----------------------------------------------
rem Put version info everywhere
rem ----------------------------------------------
"%APSIM%\Model\TclLink\bin\tclsh85.exe" VersionStamper.tcl
 
call MakeApsim

call MakeGUI

popd
