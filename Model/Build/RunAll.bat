@echo off
rem -------------------------------------------------------------
rem This batch file runs everything
rem -------------------------------------------------------------
pushd ..\..
set APSIM=%CD%

del /S %APSIM%\Tests\*.out 2>nul >nul
del /S %APSIM%\Tests\*.sum 2>nul >nul
del /S %APSIM%\Tests\*.conversions 2>nul >nul
	
start /WAIT %APSIM%\Model\ApsimRun /auto /autoclose %APSIM%\Tests

start /WAIT %APSIM%\Model\ApsimRun /auto /autoclose %APSIM%\Examples

rem -------------------------------------------------------------
rem Go find all PostRunAll.bat files and execute them.
rem -------------------------------------------------------------
for /R %APSIM%\Tests %%d in (.) do (
   if EXIST %%d\PostRunAll.bat (
      pushd %%d
      call PostRunAll.bat
      popd
      )
   )

popd
call RunAllUnitTests.bat


