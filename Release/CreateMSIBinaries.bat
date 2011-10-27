
rem Now a self extracting "binaries only" for cluster runs
set dest=Temp
mkdir %dest%
copy  ..\Apsim.xml %dest%\Apsim.xml >nul
mkdir %dest%\Model
copy  ..\Model\*.xml %dest%\Model >nul
copy  ..\Model\*.dll %dest%\Model >nul
copy  ..\Model\*.exe %dest%\Model >nul
mkdir %dest%\Model\TclLink
copy  ..\Model\TclLink\CIDataTypes.tcl %dest%\Model\TclLink >nul
xcopy /s /i ..\Model\TclLink\bin %dest%\Model\TclLink\bin >nul
xcopy /s /i ..\Model\TclLink\lib %dest%\Model\TclLink\lib >nul
mkdir %dest%\UserInterface
copy ..\UserInterface\*.xml %dest%\UserInterface >nul
mkdir %dest%\UserInterface\ToolBoxes
copy ..\UserInterface\ToolBoxes\*.xml %dest%\UserInterface\ToolBoxes >nul

"c:\Program Files\7-Zip\7z.exe" a -sfx -mx=9 -mmt=on %1.exe %dest%

rmdir /s /q %dest%

copy %1.exe C:\inetpub\wwwroot\Files\%1.exe
