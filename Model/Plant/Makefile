include $(APSIM)/Model/Build/Platform.make

PROJECT = Plant

SRC = PlantComponent.cpp \
      Plant.cpp \
      PlantPart.cpp \
      PlantBiomass.cpp \
      PlantCanopy.cpp \
      PlantLibrary.cpp \
      PlantNitrogen.cpp \
      PlantP.cpp \
      Population.cpp \
      PlantStress.cpp \
      Co2Modifier.cpp \
      StageBasedInterpolationFunction.cpp \
      Environment.cpp \
      Fixation.cpp \
      Stem.cpp \
      CompositePart.cpp \
      SimplePart.cpp \
      PlantSpatial.cpp \
      Pool.cpp \
      CompositePool.cpp \
      CompositeDelta.cpp \
      Delta.cpp \
      Biomass.cpp \
      ThingFactory.cpp \
      Parts.cpp \
      Utility/PlantUtility.cpp \
      Utility/Data.cpp \
      Utility/ExternalFunction.cpp \
      Utility/InterpolationFunction.cpp \
      Utility/LookupFunction.cpp \
      Utility/Science.cpp \
      Utility/Date.cpp \
      Utility/Observers.cpp \
      Utility/Output.cpp \
      Utility/OutputVariable.cpp \
      Storage/StoragePart.cpp \
      ReproductivePart.cpp \
      Reproductive/PodPart.cpp \
      Reproductive/PodPartFN.cpp \
      Reproductive/FloretPart.cpp \
      Reproductive/MealPart.cpp \
      Reproductive/OilPart.cpp \
      Reproductive/GrainPart.cpp \
      Reproductive/GrainPartHI.cpp \
      Reproductive/GrainPartGN.cpp \
      Reproductive/GrainPartFN.cpp \
      Reproductive/FruitCohort.cpp \
      Reproductive/FruitCohortFN.cpp \
      Reproductive/PlantFruitCohorting.cpp \
      Root/RootBase.cpp \
      Root/RootPart.cpp \
      Root/Soil.cpp \
      Root/NoRoot.cpp \
      Root/MultiRoot.cpp \
      Root/RootGrowthOption1.cpp \
      Root/RootGrowthOption2.cpp \
      Leaf/Leaf.cpp \
      Leaf/Culm.cpp \
      Leaf/IndividualLeaf.cpp \
      Leaf/GenericLeaf.cpp \
      Leaf/CohortingLeaf.cpp \
      Arbitrators/Arbitrator.cpp \
      Arbitrators/GenericArbitrator.cpp \
      Arbitrators/GenericArbitratorXY.cpp \
      Arbitrators/WholePlantGenericArbitratorXY.cpp \
      Arbitrators/WholePlantSeasonalArbitrator.cpp \
      Arbitrators/WholePlantSeasonalPpArbitrator.cpp \
      Phenology/Phenology.cpp \
      Phenology/Phase.cpp \
      Phenology/CompositePhase.cpp \
      Phenology/FixedPhase.cpp \
      Phenology/InductivePhase.cpp \
      Phenology/VernalPhase.cpp \
      Phenology/PhotoPhase.cpp \
      Phenology/EmergentPhase.cpp \
      Phenology/LeafAppPhase.cpp \
      Phenology/SowingPhase.cpp \
      Phenology/WaitingPhase.cpp \
      Phenology/CWEmergentPhase.cpp \
      Phenology/CWVernalPhase.cpp \
      Phenology/CWInductivePhase.cpp \
      Phenology/CWSowingPhase.cpp \
      Phenology/CWFixedPhase.cpp \
      Phenology/Zadok.cpp \
      Photosynthesis/PhotosynthesisModel.cpp \
      Photosynthesis/RUEModel.cpp \
      Photosynthesis/RUEWholePlantModel.cpp \
      Photosynthesis/Weather.cpp \
      Photosynthesis/SUCROSModel.cpp

LIBS = General ComponentInterface ApsimShared
ifeq ($(PLATFORM),Linux)
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.o
else
OBJS = $(APSIM)/Model/ComponentInterface/EntryPoints.obj
endif

#PRECOMPILE = StdPlant.h

PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
