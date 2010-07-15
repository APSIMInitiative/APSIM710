@echo off
rem -------------------------------------------------------------
rem This batch file compiles the GUI. 
rem -------------------------------------------------------------
call MakeProject Excel

call MakeProject CSGeneral
call MakeProject ApsimFile


call MakeProject Actions
call MakeProject ApsimUI
call MakeProject ApsimRun

call MakeProject MergeOutputFiles
call MakeProject ShellExtensions

call MakeProject RunEditor
