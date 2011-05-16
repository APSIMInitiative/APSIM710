include $(APSIM)/Model/Build/Platform.make

PROJECT = Input

SRC = Input.cpp InputComponent.cpp StringVariant.cpp 
LIBS = General ApsimShared ComponentInterface 
ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.o
else
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.obj
endif

PROJECTTYPE = libdll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

