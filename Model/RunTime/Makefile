
include $(APSIM)/Model/Build/Platform.make

PROJECT = RunTime

PROJECTTYPE = bin

dummy:
ifeq ($(PLATFORM),Linux) 
else
	cmd /c if NOT EXIST "$(APSIM)\Model\TclLink\lib\tclstub85.lib" "$(APSIM)\Model\RunTime\Tcl8.5.9.exe" x -o"$(APSIM)\Model\TclLink" -y
	cmd /c if NOT EXIST "$(APSIM)\Model\cppunit_dll.dll" copy "$(APSIM)\..\BuildLibraries\cppunit-1.13.2\lib\cppunit_dll.dll" "$(APSIM)\Model\cppunit_dll.dll"
endif
