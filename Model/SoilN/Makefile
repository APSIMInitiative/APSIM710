include $(APSIM)/Model/Build/Platform.make

PROJECT = SoilN

SRC = SoilN.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
DEF = SoilN.ldscript
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
DEF = SoilN.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

