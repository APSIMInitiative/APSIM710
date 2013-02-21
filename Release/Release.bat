
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
xcopy  /e %APSIM%\Model\TclLink\bin %dest%\Model\TclLink
xcopy  /e %APSIM%\Model\TclLink\lib %dest%\Model\TclLink

mkdir %dest%\UserInterface
copy  %APSIM%\UserInterface\*.xml %dest%\UserInterface
mkdir %dest%\UserInterface\ToolBoxes
copy  %APSIM%\UserInterface\ToolBoxes\*.xml %dest%\UserInterface\ToolBoxes

if [%1] == [] goto noargs
  "C:\Program Files\7-Zip\7z.exe" a -mx=7 -mmt=on -sfx %1.binaries.Win32.exe %dest%
   goto done

:noargs
  %APSIM%\Model\Build\VersionInfo.bat
  "C:\Program Files\7-Zip\7z.exe" a -mx=7 -mmt=on -sfx Apsim%MAJOR_VERSION%%MINOR_VERSION%-r%BUILD_NUMBER%.binaries.Win32.exe %dest%

:done
del /s /q /f Temp
rmdir /s /q Temp