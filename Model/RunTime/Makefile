
include $(APSIM)/Model/Build/Platform.make

PROJECT = RunTime

PROJECTTYPE = bin

dummy:
	copy /Y $(APSIM)\Model\RunTime\*.* $(APSIM)\Model
ifeq ($(PLATFORM),WIN32)
	copy /Y c:\progra~1\Borland\CBuilder6\Components\libxml2\win32\bin.bcb\libxml2.dll $(APSIM)\Model
else
	copy /Y $(APSIM)\..\BuildLibraries\libxml2-2.7.7.win32\lib\libxml2.dll $(APSIM)\Model
	copy /Y $(APSIM)\..\BuildLibraries\libiconv-1.11.1.win32\bin\iconv.dll $(APSIM)\Model
endif

