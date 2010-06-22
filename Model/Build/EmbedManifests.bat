@echo off

mt.exe -manifest %APSIM%\Model\Apsim.exe.manifest -outputresource:%APSIM%\Model\Apsim.exe;1
mt.exe -manifest %APSIM%\Model\ApsimShared.dll.manifest -outputresource:%APSIM%\Model\ApsimShared.dll;2
mt.exe -manifest %APSIM%\Model\General.dll.manifest -outputresource:%APSIM%\Model\General.dll;2
mt.exe -manifest %APSIM%\Model\Protocol.dll.manifest -outputresource:%APSIM%\Model\Protocol.dll;2
mt.exe -manifest %APSIM%\Model\ComponentInterface.dll.manifest -outputresource:%APSIM%\Model\ComponentInterface.dll;2
