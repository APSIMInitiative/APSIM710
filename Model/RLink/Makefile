# Makefile for Embedded R component

include $(APSIM)/Model/Build/Platform.make

PROJECT = RLink
PROJECTTYPE = dll

ifeq ($(PLATFORM),WIN32VS)
 LIBS = General ApsimShared ComponentInterface2 
 OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.obj
 SRC = RComponent.cpp RDataTypes.cpp
 OBJEXT = obj
endif

ifeq ($(PLATFORM),Linux)
 LIBS = General ApsimShared ComponentInterface2 
 OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.o
 SRC = RComponent.cpp REmbedder.cpp RDataTypes.cpp RDataTypesRcpp.cpp

 ## include headers and libraries for R
 RCPPFLAGS := 		$(shell R $(R_ARCH) CMD config --cppflags)
 RLDFLAGS := 		$(shell R $(R_ARCH) CMD config --ldflags)

 ## include headers and libraries for Rcpp interface classes
 RCPPINCL := 		$(shell echo 'Rcpp:::CxxFlags()' | R $(R_ARCH) --vanilla --slave)
 RCPPLIBS := 		$(shell echo 'Rcpp:::LdFlags()'  | R $(R_ARCH) --vanilla --slave)

 ## include headers and libraries for RInside embedding classes
 RINSIDEINCL := 	$(shell echo 'RInside:::CxxFlags()' | R $(R_ARCH) --vanilla --slave)
 RINSIDELIBS := 	$(shell echo 'RInside:::LdFlags()'  | R $(R_ARCH) --vanilla --slave)
 INCLUDES := $(RCPPFLAGS) $(RCPPINCL) $(RINSIDEINCL)
 EXTRALIBS := $(RLDFLAGS) $(RINSIDELIBS) $(RCPPLIBS) 
 OBJEXT = o
endif

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

RComponent.$(OBJEXT): RComponent.cpp RDataTypes.h

RDataTypes.$(OBJEXT): RDataTypes.cpp RDataTypes.h

REmbedder.$(OBJEXT): REmbedder.cpp REmbedder.h apsimPublish.cpp

apsimPublish.cpp RDataTypes.cpp RDataTypes.h RDataTypesRcpp.cpp: ../DataTypes/Datatypes.xml RDataTypes.macro
	$(APSIM)/Model/ProcessDataTypesInterface.exe ../DataTypes/Datatypes.xml RDataTypes.macro

