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
   	  "%FrameworkDIR32%%FrameworkVersion32%\msbuild" Test.sln /target:ReBuild /p:Configuration=Debug
      if ERRORLEVEL 0 (
         "C:\Program Files (x86)\NUnit.org\nunit-console\nunit3-console" /nologo /shadow bin\debug\Test.dll > UnitTest.tmp
         )
      )
   if EXIST Makefile (
      call %APSIM%\Model\Build\RunMake %APSIM%\Tests\UnitTests\%%d -B 
      if ERRORLEVEL 0 %APSIM%\Model\Test.exe > UnitTest.tmp
      del /Q %APSIM%\Model\Test.* 2>nul
      )
      
   cd %APSIM%\Tests\UnitTests\%%d      
   if EXIST UnitTest.tmp (
      "%APSIM%\Model\TclLink\bin\tclsh85.exe" %APSIM%\Model\Build\RemoveUnitTestTimeStamp.tcl
	  del /Q UnitTest.tmp
	  )
   )

popd

