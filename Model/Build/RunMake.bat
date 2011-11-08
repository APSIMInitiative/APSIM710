@echo off
rem ----- Usage: RunMake Directory
rem -----    e.g. RunMake [%APSIM%\Model\CSGeneral]

rem ----- Set the %APSIM% variable based on the directory where this batch file is located
set PATHSAVED=%CD%
cd %~dp0..\..
set APSIM=%CD%

rem return to where we were called
cd %PATHSAVED%
set PATHSAVED=

rem ----- Setup the Visual Studio 2010 compiler tools
if "%LIBPATH%" == "" call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat"

rem ----- Set the APSIM versioning info
if EXIST "%APSIM%\Model\Build\VersionInfo.bat" call "%APSIM%\Model\Build\VersionInfo.bat"

rem ----- Run MAKE
rem ----- The working directory is specified by %1 (may be blank)
rem NB. Make sure make.exe is the last thing executed in this script to that errors are noticed on exit
if "%1"=="" (
	make
	goto end
	)

make -C "%1" %2 %3 %4
:end