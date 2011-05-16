include $(APSIM)/Model/Build/Platform.make

PROJECT= FortranComponentInterface

ifeq ($(PLATFORM),Linux)
DEF = FortranComponentInterface.ldscript
else
DEF = FortranComponentInterface.def
endif

SRC = 	 FortranComponentInterface.cpp \
          FortranDataTypes.cpp       \
          EntryPoints.cpp            \
          FORTRANComponentWrapper.cpp


LIBS = General ApsimShared ComponentInterface
PROJECTTYPE = libdll
include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
