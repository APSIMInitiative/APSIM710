include $(APSIM)/Model/Build/Platform.make

PROJECT = Irrigation

SRC = Registrations.f90 Irrigation.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface
DEF = Irrigation.def

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make


