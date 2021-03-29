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

set InnoSetup="C:\Program Files (x86)\Inno Setup 6\ISCC.exe"

mkdir "%2"

%InnoSetup% /Q /O"%2" /F"%1" "%2.iss"

set TIMESTAMP="http://timestamp.comodoca.com/?td=sha256"
set CERTIFICATE="C:\apsim.p12"
set "TARGET=%2\%1.exe"

rem ----- This requires SignTool.exe to be on PATH.
rem ----- Also assumes that APSIM_CERT_PWD is an existing environment variable (it's set by jenkins)
SignTool sign /q /as /fd sha256 /tr %TIMESTAMP% /td sha256 /f %CERTIFICATE% /p %APSIM_CERT_PWD% %TARGET%
SignTool verify /pa /v /d %TARGET%