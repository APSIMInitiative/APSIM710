@echo off
call CleanAll
call MakeAll
copy /Y %APSIM%\Model\RunTime %APSIM%\Model > nul