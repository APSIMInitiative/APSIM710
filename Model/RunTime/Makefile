
include $(APSIM)/Model/Build/Platform.make

PROJECT = RunTime

PROJECTTYPE = bin

dummy:
ifeq ($(PLATFORM),Linux) 
	cp $(APSIM)/Model/RunTime/*.dll $(APSIM)/Model/
else
	cmd /c if NOT EXIST "$(APSIM)\Model\libxml2.dll" copy /Y "$(APSIM)\Model\RunTime\*.dll" "$(APSIM)\Model"
	cmd /c if NOT EXIST "$(APSIM)\Model\TclLink\lib\tclstub85.lib" "$(APSIM)\Model\RunTime\Tcl8.5.9.exe" x -o"$(APSIM)\Model\TclLink" -y
endif
