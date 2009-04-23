@echo off

rem -------------------------------------------------------------------
rem Merge all outputs files into a single file.
rem -------------------------------------------------------------------
%APSIM%\Model\MergeOutputFiles .\

rem -------------------------------------------------------------------
rem Clean up all unwanted files.
rem -------------------------------------------------------------------
ren all.out all.txt
del /Q *.sum 2>nul
del /Q *.sim 2>nul
del /Q *.out 2>nul

rem -------------------------------------------------------------------
rem Rename All.txt to All.out. Couldn't do this earlier as it would
rem have been deleted.
rem -------------------------------------------------------------------
ren All.txt All.out
