include $(APSIM)/Model/Build/Platform.make

PROJECT= FortranComponentInterface

SRC = 	 FortranComponentInterface.cpp     \
          EntryPoints.cpp                   \
	       DataTypes.cpp	                    \
          FortranArray.cpp                  \
          FortranString.cpp                 \
          FortranComponentWrapper.cpp       \
          ScienceAPI.cpp


LIBS = General ApsimShared Protocol ComponentInterface2
PROJECTTYPE = dll
include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
