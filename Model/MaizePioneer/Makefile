include $(APSIM)/Model/Build/Platform.make

PROJECT = Maize

SRC = PlantInterface.cpp \
      PlantComponents.cpp \
      Phosphorus.cpp \
      PlantActions.cpp \
      Leaf.cpp \
      LeafLER.cpp \
      LeafLimitedTranspiration.cpp \
      LeafSlowWilting.cpp \
      NaturalSpline.cpp \
      Stem.cpp \
      StemCM.cpp \
      StemSM.cpp \
      Nitrogen.cpp \
      Rachis.cpp \
      Grain.cpp \
      GrainCM.cpp \
      GrainSilking.cpp \
      LayerModel.cpp \
      Water.cpp \
      Phenology.cpp \
      Plant.cpp  \
      Roots.cpp \
      Biomass.cpp \
      BiomassCM.cpp \
      BiomassSM.cpp \
      CanopyPhotosynthesis.cpp \
      Utilities.cpp \
      Dll.cpp \

LIBS = General ApsimShared ComponentInterface2
ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.o
else
OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.obj
endif

PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make


