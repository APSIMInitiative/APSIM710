@echo off
%APSIM%\Model\apsimui.exe Potato.apsim Export ..\..\..\Graphs\Potato .gif
%APSIM%\Model\apsimui.exe PotatoValidation.apsim Export ..\..\..\Graphs\Potato .gif
