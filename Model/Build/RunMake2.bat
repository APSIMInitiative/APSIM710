@echo off
cd %1
cd ..\..
set APSIM=%CD%
cd %1
if "%LIBPATH%" == "" (
   if "%ProgramFiles(x86)%" == "" (
      set Path=C:\Progra~1\LF9556\Bin;c:\Windows\System32
      call "c:\Program Files\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
   ) else (
      set Path=C:\Progra~2\LF9556\Bin;c:\Windows\System32
      call "c:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
   )
)
if "%1" == "APSIM" set APSIM=%2
%APSIM%\Model\Build\make
