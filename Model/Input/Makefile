include $(APSIM)/Model/Build/Platform.make

PROJECT = Input

SRC = Input.cpp InputComponent.cpp StringVariant.cpp 
LIBS = General ApsimShared ComponentInterface 
ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.o
else
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.obj
endif

PROJECTTYPE = dll

POSTBUILD = makelink

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

ifeq ($(PLATFORM),Linux)
# a few (patchInput, siloInput) are going to try and link against libInput.so
makelink: $(APSIM)/Model/$(PROJECT).so
	/bin/ln -sf $(APSIM)/Model/$(PROJECT).so $(APSIM)/Model/lib$(PROJECT).so
endif
