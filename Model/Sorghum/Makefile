include $(APSIM)/Model/Build/Platform.make

PROJECT = Sorghum

SRC = PlantInterface.cpp \
      PlantComponents.cpp \
      Phosphorus.cpp \
      PlantActions.cpp \
      Leaf.cpp \
      Stem.cpp \
      Nitrogen.cpp \
      Rachis.cpp \
      Grain.cpp \
      Water.cpp \
      Phenology.cpp  \
      Plant.cpp  \
      Roots.cpp \
      Biomass.cpp \
      Utilities.cpp \
      Dll.cpp
      
LIBS = General ApsimShared ComponentInterface2
OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.obj

PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make


