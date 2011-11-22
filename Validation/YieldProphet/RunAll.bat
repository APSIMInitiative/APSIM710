@echo off

pushd %~dp0..\..
set APSIM=%CD%
popd

%APSIM%\Model\ApsimRun.exe .\