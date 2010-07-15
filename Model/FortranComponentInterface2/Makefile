include $(APSIM)/Model/Build/Platform.make

PROJECT= FortranComponentInterface2

SRC = 	 FortranComponentInterface.cpp     \
          EntryPoints.cpp                   \
          DataTypes.cpp	                    \
          FortranComponentWrapper.cpp       \
          ScienceAPI2.cpp


LIBS = General ApsimShared Protocol ComponentInterface2
PROJECTTYPE = dll
include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
