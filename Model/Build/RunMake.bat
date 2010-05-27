@echo off
if "%LIBPATH%" == "" (
   set Path=C:\Progra~1\LF9556\Bin;c:\Windows\System32
   call "c:\Program Files\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
)
%APSIM%\Model\Build\make %1 %2 %3 %4
