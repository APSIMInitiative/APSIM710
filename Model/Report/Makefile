include $(APSIM)/Model/Build/Platform.make

PROJECT = Report

SRC = Report.cpp ReportComponent.cpp
LIBS = ComponentInterface2 ApsimShared General 

ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.o
else
OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.obj
endif

PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

