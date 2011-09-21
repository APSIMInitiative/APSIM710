@echo off

rem ----- Endless loop to continually restart the JobScheduler.
:Start

call BobMain.bat

rem ----- Go back to start if JobScheduler returns code 0
if ERRORLEVEL 1 goto End
if ERRORLEVEL 0 goto Start

:End
rem Stop at end so that we can see screen.
pause