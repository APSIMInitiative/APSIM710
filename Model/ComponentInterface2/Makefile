include $(APSIM)/Model/Build/Platform.make

PROJECT= ComponentInterface2

SRC = 	  ComponentInterface.cpp           \
          message.cpp                       \
          TypeConverter.cpp                 \
          ArraySpecifier.cpp                \
          CMPData.cpp                       \
          EntryPoints.cpp                   \
          CMPComponentEntryPoints.cpp       \
          CMPComponentInterface.cpp         \
          CreateComponent.cpp               \
          Bounded.cpp                       \
          Variant.cpp                       \
          DataTypes.cpp	                    \
          ScienceAPI2.cpp                    \
          ScienceAPI2Impl.cpp                \
          Messages.cpp                      \
          FortranString.cpp                 \
          FortranArray.cpp                  \
          StructureConverter.cpp

LIBS = General ApsimShared 

ifeq ($(PLATFORM),Linux)
PROJECTTYPE = dll
else
PROJECTTYPE = libdll
endif

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
