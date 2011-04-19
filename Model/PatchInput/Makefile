include $(APSIM)/Model/Build/Platform.make

PROJECT = PatchInput

SRC = PatchInput.cpp PatchInputComponent.cpp
LIBS = General ApsimShared ComponentInterface Input
ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.o
else
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.obj
endif

PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

