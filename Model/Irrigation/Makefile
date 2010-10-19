include $(APSIM)/Model/Build/Platform.make

PROJECT = Irrigation

SRC = Registrations.f90 Irrigation.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
DEF = Irrigation.rsp
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
DEF = Irrigation.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make


