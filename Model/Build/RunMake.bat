rem @echo off
set PATHSAVED=%PATH%

if "%LIBPATH%" == "" (
   if "%ProgramFiles(x86)%" == "" (
      set Path=C:\Progra~1\gfortran\libexec\gcc\i586-pc-mingw32\4.5.0;C:\Progra~1\gfortran\bin;C:\Progra~1\LF9556\Bin;c:\Windows\System32
      call "c:\Program Files\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"
   ) else (
      set Path=C:\Progra~2\gfortran\libexec\gcc\i586-pc-mingw32\4.5.0;C:\Progra~2\gfortran\bin;C:\Progra~2\LF9556\Bin;c:\Windows\System32
      call "c:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"
   )
   
)
if "%1" == "APSIM" set APSIM=%2
if EXIST "%APSIM%\Model\Build\VersionInfo.bat" call "%APSIM%\Model\Build\VersionInfo.bat"

%APSIM%\Model\Build\make %3

set PATH=%PATHSAVED%