# Makefile for R component

# There are 2 dlls produced - one loaded by apsim; the other once the location of
# the R installation is known.

# On windows, the first is built by msvc, the other by gcc.
# 
include $(APSIM)/Model/Build/Platform.make

PROJECT = RLink
PROJECTTYPE = dll

ifeq ($(PLATFORM),WIN32VS)
  OBJEXT = obj
  DLLEXT = dll
  R_MAKE=.\RMakeCmd.bat
endif

ifeq ($(PLATFORM),Linux)
  OBJEXT = o
  DLLEXT = so
  R_MAKE=make
endif

LIBS = General ApsimShared ComponentInterface2
OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.$(OBJEXT)
SRC = RComponent.cpp RDataTypes.cpp

PREBUILD = ../REmbed.$(DLLEXT)

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

CFLAGS := $(CFLAGS) 

RComponent.$(OBJEXT): RComponent.cpp RComponent.h RDataTypes.h

RDataTypes.$(OBJEXT): RDataTypes.cpp RDataTypes.h

RDataTypes.cpp RDataTypes.h RDataTypesEmbedded.cpp RDataTypesEmbedded.h RLink.def: ../DataTypes/Datatypes.xml RDataTypes.macro
	$(APSIM)/Model/ProcessDataTypesInterface.exe ../DataTypes/Datatypes.xml RDataTypes.macro

REmbedder.o: REmbedder.cpp REmbedder.h 
	$(R_MAKE) -f Makefile.rgcc $@

RDataTypesEmbedded.o: RDataTypesEmbedded.cpp
	$(R_MAKE) -f Makefile.rgcc $@

dllProcAddressWrapper.o: dllProcAddressWrapper.cpp
	$(R_MAKE) -f Makefile.rgcc $@

../REmbed.dll: REmbedder.o RDataTypesEmbedded.o dllProcAddressWrapper.o libRLink.a
	$(R_MAKE) -f Makefile.rgcc $@

../REmbed.so: REmbedder.o RDataTypesEmbedded.o
	$(CXX) $(CFLAGS) -shared -s -o $@ REmbedder.o  RDataTypesEmbedded.o $(RLDFLAGS) $(RLDLIBS)

libRLink.a: RLink.def
	dlltool -d RLink.def -l libRLink.a

