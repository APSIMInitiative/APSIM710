@echo off

pushd %~dp0..\..
set APSIM=%CD%
popd

%APSIM%\Model\MergeOutputFiles .\2004
%APSIM%\Model\MergeOutputFiles .\2005
%APSIM%\Model\MergeOutputFiles .\2006
%APSIM%\Model\MergeOutputFiles .\2007
%APSIM%\Model\MergeOutputFiles .\2008
%APSIM%\Model\MergeOutputFiles .\2009
