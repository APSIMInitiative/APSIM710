@echo off
rem ----------------------------------------------
rem Create a release installation. 
rem    %1 = Name of file to create.
rem    %2 = Name of .iss file.
rem ----------------------------------------------

rem ----- Set the %APSIM% variable based on the directory where this batch file is located
cd %~dp0..
set APSIM=%CD%
cd %APSIM%\Release

set InnoSetup="C:\Program Files (x86)\Inno Setup 5\ISCC.exe"

mkdir "%2"

%InnoSetup% /Q /O"%2" /F"%1" "%2.iss"

rem ----- Temporarily disable signing while we sort out new certificate
rem call "C:\SignCsiro.bat" %2\%1.exe