include $(APSIM)/Model/Build/Platform.make

PROJECT= FortranComponentInterface2

ifeq ($(PLATFORM),Linux)
DEF = FortranComponentInterface2.ldscript
else
DEF = FortranComponentInterface2.def
endif

SRC = 	 FortranComponentInterface.cpp     \
          EntryPoints.cpp                   \
          DataTypes.cpp	                    \
          FortranComponentWrapper.cpp       \
          ScienceAPI2.cpp


LIBS = General ApsimShared Protocol ComponentInterface2
PROJECTTYPE = libdll
include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
