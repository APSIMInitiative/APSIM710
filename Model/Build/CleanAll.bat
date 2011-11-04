@echo off
rem -------------------------------------------------------------
rem Remove all unwanted files.
rem -------------------------------------------------------------
pushd ..
del /Q /S *.map >nul 2>nul
del /Q /S *.obj >nul 2>nul
del /Q /S *.o   >nul 2>nul
del /Q /S *.lib >nul 2>nul
del /Q /S *.mod >nul 2>nul
del /Q /S *.bak >nul 2>nul
del /Q /S *.dsk >nul 2>nul
del /Q /S *.~*  >nul 2>nul
del /Q /S dll.res  >nul 2>nul
del /Q /S exe.res  >nul 2>nul
del /Q /S *.pch >nul 2> nul

del /Q *.dll >nul 2>nul
del /Q *.exe >nul 2>nul
del /Q *.tds >nul 2>nul
del /Q *.lib >nul 2>nul
del /Q *.a   >nul 2> nul
del /Q *.pdb >nul 2>nul
del /Q *.imp >nul 2>nul
del /Q *.map >nul 2>nul

rmdir /Q /S TclLink\bin >nul 2>nul
rmdir /Q /S TclLink\doc >nul 2>nul
rmdir /Q /S TclLink\include >nul 2>nul
rmdir /Q /S TclLink\lib >nul 2>nul
del /Q /S TclLink\CIDataTypes.tcl >nul 2>nul

popd