include $(APSIM)/Model/Build/Platform.make

PROJECT = SoilWat

SRC = Evap.for Lateral.for SoilWat.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
DEF = SoilWat.ldscript
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
DEF = SoilWat.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

