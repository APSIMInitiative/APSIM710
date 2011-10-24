@echo off
rem ----- Usage: RunMake Directory
rem -----    e.g. RunMake %APSIM%\Model\CSGeneral


rem ----- Set the %APSIM% variable based on the directory where this batch file is located
cd %~dp0..\..
set APSIM=%CD%

rem ----- Change the working directory to that specified by %1
cd %1

rem ----- Setup the Visual Studio 2010 compiler tools
if "%LIBPATH%" == "" call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat"

rem ----- Set the APSIM versioning info
if EXIST "%APSIM%\Model\Build\VersionInfo.bat" call "%APSIM%\Model\Build\VersionInfo.bat"

rem ----- Run MAKE
%APSIM%\Model\Build\make %2 %3 %4
