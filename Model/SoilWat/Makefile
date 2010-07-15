include $(APSIM)/Model/Build/Platform.make

PROJECT = SoilWat

SRC = Evap.for Lateral.for SoilWat.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface
DEF = SoilWat.def

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

