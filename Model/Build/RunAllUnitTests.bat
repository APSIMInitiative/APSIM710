@echo off
rem -------------------------------------------------------------
rem This batch file runs everything
rem -------------------------------------------------------------
pushd ..\..
set APSIM=%CD%

cd %APSIM%\Tests\UnitTests
del /Q UnitTests.out 2>nul

rem -------------------------------------------------------------
rem Go find all unit tests and execute them.
rem -------------------------------------------------------------
for /D %%d in (*) do (
   cd %APSIM%\Tests\UnitTests\%%d
   echo ----------------------------------------------------- >> ..\UnitTests.out
   echo %%d >> ..\UnitTests.out
   echo ----------------------------------------------------- >> ..\UnitTests.out
   echo -----------------------------------------------------
   echo %%d
   echo -----------------------------------------------------
   if EXIST Test.sln (
      "%VS100COMNTOOLS%..\IDE\devenv" Test.sln /ReBuild debug
      if ERRORLEVEL 1 echo Compile ERRORS FOUND >> ..\UnitTests.out
      if ERRORLEVEL 0 (
         cmd /c "C:\Program Files (x86)\NUnit 2.5.7\bin\net-2.0\nunit-console-x86" bin\debug\Test.dll /nologo /noshadow >> ..\UnitTests.out
         if ERRORLEVEL 1 echo ERRORS FOUND >> ..\UnitTests.out
         )
      echo. >> ..\UnitTests.out
      echo. >> ..\UnitTests.out
      )
   if EXIST Makefile (
      call %APSIM%\Model\Build\RunMake -B
      if ERRORLEVEL 1 echo ERRORS FOUND >> ..\UnitTests.out
      if ERRORLEVEL 0 (
         %APSIM%\Model\Test.exe >> ..\UnitTests.out
         if ERRORLEVEL 1 echo ERRORS FOUND >> ..\UnitTests.out
         )
      del /Q %APSIM%\Model\Test.* 2>nul
      echo. >> ..\UnitTests.out
      echo. >> ..\UnitTests.out
      )
   )

popd
"%APSIM%\..\BuildLibraries\tcl\ASTcl\bin\tclsh84.exe" RemoveUnitTestTimeStamp.tcl

