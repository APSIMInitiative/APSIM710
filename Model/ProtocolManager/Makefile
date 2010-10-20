include $(APSIM)/Model/Build/Platform.make

PROJECT = ProtocolManager

SRC = ProtocolManager.cpp Coordinator.cpp ComponentAlias.cpp
LIBS = General ApsimShared Protocol ComponentInterface

ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.o
else
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.obj
endif

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

