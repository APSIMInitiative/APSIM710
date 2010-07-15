include $(APSIM)/Model/Build/Platform.make

PROJECT = MicroMet

SRC = Registrations.f90 MicroMet.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
endif


STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface
DEF = MicroMet.def

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

