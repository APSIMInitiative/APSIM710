include $(APSIM)/Model/Build/Platform.make

PROJECT= ComponentInterface

SRC = ComponentInterface.cpp  \
      Component.cpp \
      message.cpp \
      Type.cpp \
      TypeConverter.cpp \
      Variant.cpp \
      Variants.cpp \
      MessageData.cpp \
      ApsimVariant.cpp \
      ArraySpecifier.cpp \
      ProtocolVector.cpp   \
      Variable.cpp \
      VariableRef.cpp \
      ScienceAPI.cpp \
      DataTypes.cpp \
      MessageTypes.cpp

LIBS = General ApsimShared
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
