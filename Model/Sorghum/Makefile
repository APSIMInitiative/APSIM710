include $(APSIM)/Model/Build/Platform.make

PROJECT = Sorghum

SRC = PlantInterface.cpp \
      PlantComponents.cpp \
      Phosphorus.cpp \
      PlantActions.cpp \
      Leaf.cpp \
      LeafCulms.cpp \
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
ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.o
else
OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.obj
endif

PROJECTTYPE = dll


include $(APSIM)/Model/Build/$(PLATFORM)CPP.make



