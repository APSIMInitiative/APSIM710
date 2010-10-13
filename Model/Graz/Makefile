include $(APSIM)/Model/Build/Platform.make

PROJECT = Graz

SRC = Graz.cpp GrazComponent.cpp
STATICLIBS = FortranInfrastructure
LIBS = General ApsimShared ComponentInterface2

PROJECTTYPE = dll

ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.o
else
OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.obj
endif

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

