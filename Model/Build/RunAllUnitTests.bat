@echo off
rem -------------------------------------------------------------
rem This batch file runs everything
rem -------------------------------------------------------------
pushd ..\..
set APSIM=%CD%

cd %APSIM%\Tests\UnitTests

rem -------------------------------------------------------------
rem Go find all unit tests and execute them.
rem -------------------------------------------------------------
for /D %%d in (*) do (
   cd %APSIM%\Tests\UnitTests\%%d
   if EXIST Test.sln (
      "%VS100COMNTOOLS%..\IDE\devenv" Test.sln /ReBuild debug
      if ERRORLEVEL 0 (
         "C:\Program Files (x86)\NUnit 2.5.7\bin\net-2.0\nunit-console-x86" bin\debug\Test.dll /nologo /noshadow > UnitTest.out
         )
      )
   if EXIST Makefile (
      call %APSIM%\Model\Build\RunMake -B
      if ERRORLEVEL 0 %APSIM%\Model\Test.exe > UnitTest.out
      del /Q %APSIM%\Model\Test.* 2>nul
      )
   if EXIST UnitTest.out "%APSIM%\..\BuildLibraries\tcl\ASTcl\bin\tclsh84.exe" %APSIM%\Model\Build\RemoveUnitTestTimeStamp.tcl
   )

popd

