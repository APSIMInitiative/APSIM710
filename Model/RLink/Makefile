# Makefile for R component

# On windows, there are 2 dlls produced - one built by msvc, the other by gcc.
# On unix, only one is built.

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
 SRC = RComponent.cpp REmbedder.cpp RDataTypes.cpp RDataTypesEmbedded.cpp

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

CFLAGS := $(CFLAGS) -DBUILDING_APSIMDLL

RComponent.$(OBJEXT): RComponent.cpp RDataTypes.h

RDataTypes.$(OBJEXT): RDataTypes.cpp RDataTypes.h

REmbedder.$(OBJEXT): REmbedder.cpp REmbedder.h 

RDataTypes.cpp RDataTypes.h RDataTypesEmbedded.cpp RDataTypesEmbedded.h RLink.def: ../DataTypes/Datatypes.xml RDataTypes.macro
	$(APSIM)/Model/ProcessDataTypesInterface.exe ../DataTypes/Datatypes.xml RDataTypes.macro
