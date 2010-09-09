include $(APSIM)/Model/Build/Platform.make

PROJECT = ProtocolManager

SRC = ProtocolManager.cpp Coordinator.cpp ComponentAlias.cpp
LIBS = General ApsimShared Protocol ComponentInterface
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.obj

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

