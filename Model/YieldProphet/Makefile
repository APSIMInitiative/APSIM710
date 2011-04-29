include $(APSIM)/Model/Build/Platform.make

PROJECT = YieldProphet

SRC = YPComponent.cpp YPCalc.cpp
LIBS = General ApsimShared ComponentInterface
ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.o
else
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.obj
endif

PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
