include $(APSIM)/Model/Build/Platform.make

PROJECT = SoilP

SRC = Registrations.f90 SoilP.f90 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
DEF = SoilP.ldscript
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
DEF = SoilP.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

