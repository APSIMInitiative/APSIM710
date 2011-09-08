
include $(APSIM)/Model/Build/Platform.make

PROJECT = RunTime

PROJECTTYPE = bin

dummy:
	if NOT EXIST "$(APSIM)\Model\libxml2.dll" copy /Y "$(APSIM)\Model\RunTime\*.dll" "$(APSIM)\Model"
	if NOT EXIST "$(APSIM)\Model\TclLink\bin\tclsh85.exe" "$(APSIM)\Model\RunTime\Tcl8.5.9.exe" x -o"$(APSIM)\Model\TclLink" -y
	attrib -R "$(APSIM)\Model\TclLink\*.*" /S