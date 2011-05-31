
include $(APSIM)/Model/Build/Platform.make

PROJECT = RunTime

PROJECTTYPE = bin

dummy:
	copy /Y $(APSIM)\Model\RunTime\*.dll $(APSIM)\Model
	$(APSIM)\Model\RunTime\Tcl8.5.9.exe x -o"$(APSIM)\Model\TclLink" -y
