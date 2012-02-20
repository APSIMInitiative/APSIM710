# Makefile for Embedded R component

include $(APSIM)/Model/Build/Platform.make

PROJECT = RLink
PROJECTTYPE = dll

ifeq ($(PLATFORM),WIN32VS)
 LIBS = General ApsimShared ComponentInterface2 
 OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.obj
endif

ifeq ($(PLATFORM),Linux)
 LIBS = General ApsimShared ComponentInterface2 
 OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.o
endif

SRC = RComponent.cpp 

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

