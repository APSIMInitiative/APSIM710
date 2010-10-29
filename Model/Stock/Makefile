include $(APSIM)/Model/Build/Platform.make

PROJECT = StockScienceConverter

SRC =    StockScienceConverter.cpp \
         ScienceConverterComponent.cpp \
         PlantPool.cpp SeedPool.cpp \
         ConverterBase.cpp \
         HerbageBase.cpp \
         HerbageConverter.cpp \
         NonHerbageConverter.cpp \
         PlantHerbage.cpp \
         ResidueHerbage.cpp \
         ResiduePool.cpp \
         Constants.cpp
          
LIBS = General ApsimShared ComponentInterface
ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.o
else
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.obj
endif

PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
