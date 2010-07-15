include $(APSIM)/Model/Build/Platform.make

PROJECT= FortranComponentInterface

SRC = 	 FortranComponentInterface.cpp \
          FortranDataTypes.cpp       \
          EntryPoints.cpp            \
          FORTRANComponentWrapper.cpp


LIBS = General ApsimShared ComponentInterface
PROJECTTYPE = dll
include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
