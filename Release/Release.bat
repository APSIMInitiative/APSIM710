
set dest=Temp
mkdir %dest%
copy  %APSIM%\Apsim.xml   %dest%\Apsim.xml
mkdir %dest%\Model
copy  %APSIM%\Model\ApsimRun.sh %dest%\Model
copy  %APSIM%\Model\*.xml %dest%\Model
copy  %APSIM%\Model\*.dll %dest%\Model
copy  %APSIM%\Model\*.exe %dest%\Model
copy  %APSIM%\Model\*.x   %dest%\Model
mkdir %dest%\Model\TclLink
copy  %APSIM%\Model\TclLink\CIDataTypes.tcl %dest%\Model\TclLink
mkdir %dest%\Model\TclLink\bin
xcopy  /e %APSIM%\Model\TclLink\bin %dest%\Model\TclLink\bin
mkdir %dest%\Model\TclLink\lib
xcopy  /e %APSIM%\Model\TclLink\lib %dest%\Model\TclLink\lib

mkdir %dest%\UserInterface
copy  %APSIM%\UserInterface\*.xml %dest%\UserInterface
mkdir %dest%\UserInterface\ToolBoxes
copy  %APSIM%\UserInterface\ToolBoxes\*.xml %dest%\UserInterface\ToolBoxes

"C:\Program Files\7-Zip\7z.exe" a -mx=7 -mmt=on Temp.7z %dest%

if [%1] == [] goto noargs
   copy /b "C:\Program Files (x86)\7-Zip\7zCon.sfx" + 7zWinConfig.txt + Temp.7z %1.binaries.WINDOWS.INTEL.exe
   copy /b "C:\Program Files\7-Zip\7zCon.sfx" + 7zWinConfig.txt + Temp.7z %1.binaries.WINDOWS.X86_64.exe
   goto done

:noargs
   call %APSIM%\Model\Build\VersionInfo.bat
   copy /b "C:\Program Files (x86)\7-Zip\7zCon.sfx" + 7zWinConfig.txt + Temp.7z Apsim%MAJOR_VERSION%.%MINOR_VERSION%-r%BUILD_NUMBER%.binaries.WINDOWS.INTEL.exe
   copy /b "C:\Program Files\7-Zip\7zCon.sfx" + 7zWinConfig.txt + Temp.7z Apsim%MAJOR_VERSION%.%MINOR_VERSION%-r%BUILD_NUMBER%.binaries.WINDOWS.X86_64.exe

:done

del /s /q /f Temp.7z
del /s /q /f Temp
rmdir /s /q Temp