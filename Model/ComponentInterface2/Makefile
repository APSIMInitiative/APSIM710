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
          Variant.cpp                       \
	       DataTypes.cpp	                    \
          ScienceAPIImpl.cpp                \
          Messages.cpp                      \
          StructureConverter.cpp

LIBS = General ApsimShared Protocol
PROJECTTYPE = dll
include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
