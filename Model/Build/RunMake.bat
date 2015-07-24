@echo off
rem ----- Usage: RunMake Directory
rem -----    e.g. RunMake [%APSIM%\Model\CSGeneral]

setlocal enabledelayedexpansion
rem ----- Set the %APSIM% variable based on the directory where this batch file is located
set PATHSAVED=%CD%
cd %~dp0..\..
set APSIM=%CD%

rem return to where we were called
cd %PATHSAVED%
set PATHSAVED=

if NOT "%2" == "" (
  SET VSTOOLPATH=VS%10COMNTOOLS
  goto forceVersion
)

rem ----- Determine the highest installed version of Visual Studio on the current system
rem ----- The set of numbers in the FOR statement indicate the VS version that we will look for
rem ----- 10 = VS 2010
rem ----- 11 = VS 2012
rem ----- 12 = VS 2013
rem ----- 14 = VS 2015
rem ----- (There is no 13!)
rem ----- If you want to force building with a specific version, just delete the numbers of the
rem ----- versions you don't want to consider
set VSTOOLPATH=""
for %%a in (10 11 12 14) DO IF DEFINED VS%%a0COMNTOOLS (
  SET VSTOOLPATH=VS%%a0COMNTOOLS
)
if "%VSTOOLPATH%" == "" (
  echo Cannot locate the Visual Studio compiler!
  popd
  exit /B
) 
:forceVersion
set VSTOOLPATH=!%VSTOOLPATH%!

rem ----- Set up the Visual Studio compiler tools
if "%LIBPATH%" == "" call "%VSTOOLPATH%..\..\VC\vcvarsall.bat"

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