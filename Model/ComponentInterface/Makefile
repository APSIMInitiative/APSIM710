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

ifeq ($(PLATFORM),Linux)
everything: $(SRC) $(APSIM)/Model/$(PROJECT).so EntryPoints.o
else
everything: $(SRC) $(PROJECT).dll EntryPoints.obj
endif

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
