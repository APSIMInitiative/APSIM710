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
  # NB. You will need RTools from CRAN (http://cran.r-project.org/mirrors.html.) to build 
  #     the embedding dll. If there is a source tree here, the make file will try to 
  #     build the secondary dll with the r gnu toolchain, which is different from the apsim one.
  R_HOME = c:/R-3.1.0
  RINSTALLED:=$(shell $(R_HOME)\bin\R.exe --version)
endif

ifeq ($(PLATFORM),Linux)
  OBJEXT = o
  DLLEXT = so
  R_HOME = /usr
  RINSTALLED:=$(shell $(R_HOME)/bin/R --version)
endif

ifneq ($(RINSTALLED),)
  PREBUILD = ../REmbed.$(DLLEXT)
  RCPPFLAGS := 		$(shell $(R_HOME)/bin/R $(R_ARCH) CMD config --cppflags)
  RCPPFLAGS := 	$(RCPPFLAGS) $(shell echo 'Rcpp:::CxxFlags()' | $(R_HOME)/bin/R $(R_ARCH) --vanilla --slave)
  RLDFLAGS := 	$(RLDFLAGS)	$(shell echo 'Rcpp:::LdFlags()'  | $(R_HOME)/bin/R $(R_ARCH) --vanilla --slave)
  RCPPFLAGS := 	$(RCPPFLAGS)	$(shell echo 'RInside:::CxxFlags()' | $(R_HOME)/bin/R $(R_ARCH) --vanilla --slave)
  RLDFLAGS := 	$(RLDFLAGS)	$(shell echo 'RInside:::LdFlags()'  | $(R_HOME)/bin/R $(R_ARCH) --vanilla --slave)
  RCPPFLAGS := 	$(RCPPFLAGS)	-Wall
  RCXXFLAGS := 	$(RCPPFLAGS) -I $(APSIM)/Model
  RCXX := 		$(shell $(R_HOME)/bin/R $(R_ARCH) CMD config CXX)
  RLD := 			$(shell $(R_HOME)/bin/R $(R_ARCH) CMD config SHLIB_LD)
endif

LIBS = General ApsimShared ComponentInterface2
OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.$(OBJEXT)
SRC = RComponent.cpp RDataTypes.cpp

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

RComponent.$(OBJEXT): RComponent.cpp RComponent.h RDataTypes.h

RDataTypes.$(OBJEXT): RDataTypes.cpp RDataTypes.h

RComponent.h RDataTypes.cpp RDataTypes.h RDataTypesEmbedded.cpp RDataTypesEmbedded.h RLink.def: ../DataTypes/Datatypes.xml RDataTypes.macro
	$(APSIM)/Model/ProcessDataTypesInterface.exe ../DataTypes/Datatypes.xml RDataTypes.macro

# Stuff for REmbedder.cpp here
REmbedder.o: REmbedder.cpp REmbedder.h RDataTypes.h 
	$(RCXX) -fPIC -c $(RCXXFLAGS) $(RCXXFLAGS)  REmbedder.cpp 

RDataTypesEmbedded.o: RDataTypesEmbedded.cpp RDataTypes.h 
	$(RCXX) -fPIC -c $(RCXXFLAGS) $(RCXXFLAGS)  RDataTypesEmbedded.cpp 

dllProcAddressWrapper.o: dllProcAddressWrapper.cpp
	$(RCXX) -c $(CXXFLAGS) dllProcAddressWrapper.cpp

ifneq ($(RINSTALLED),)
../REmbed.dll: REmbedder.o RDataTypesEmbedded.o dllProcAddressWrapper.o libRLink.a
	$(RLD) $(RLDFLAGS) -shared -s -static-libgcc -o $@ REmbedder.o  RDataTypesEmbedded.o dllProcAddressWrapper.o libRLink.a $(RLDLIBS) -lstdc++
endif

../REmbed.so: REmbedder.o RDataTypesEmbedded.o
	$(RLD) $(RLDFLAGS) -shared -s -o $@ REmbedder.o  RDataTypesEmbedded.o $(RLDLIBS)

libRLink.a: RLink.def
	dlltool -d RLink.def -l libRLink.a

clean:
	rm -f RComponent.h RDataTypes.cpp RDataTypes.h RDataTypesEmbedded.cpp RDataTypesEmbedded.h RLink.def ../REmbed.so *.o 