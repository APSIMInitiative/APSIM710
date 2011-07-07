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
         "C:\Program Files (x86)\NUnit 2.5.7\bin\net-2.0\nunit-console-x86" bin\debug\Test.dll /nologo /noshadow > UnitTest.tmp
         )
      )
   if EXIST Makefile (
      call %APSIM%\Model\Build\RunMake -B
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

