include $(APSIM)/Model/Build/Platform.make

PROJECT = Tracker

SRC = Tracker.cpp TrackerComponent.cpp TrackerVariable.cpp
LIBS = General ApsimShared ComponentInterface
ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.so
else
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.obj
endif

PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
