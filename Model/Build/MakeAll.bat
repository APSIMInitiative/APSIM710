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
rem Remove old VersionInfo files
rem -------------------------------------------------------------
del /Q VersionInfo.* 2> nul

rem ----------------------------------------------
rem Put version info in apsim.xml
rem ----------------------------------------------
"%APSIM%\..\BuildLibraries\tcl\ASTcl\bin\tclsh84.exe" VersionStamper.tcl
 
rem -------------------------------------------------------------
rem Copy runtime dlls for bootstrap progs
rem -------------------------------------------------------------
call MakeProject RunTime

call MakeApsim

call MakeGUI

popd
