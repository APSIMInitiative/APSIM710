include $(APSIM)/Model/Build/Platform.make

PROJECT = Log

SRC = Log.cpp LogComponent.cpp
LIBS = General ApsimShared Protocol ComponentInterface

PROJECTTYPE = dll

ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.so
else
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.obj
endif

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

