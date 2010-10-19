include $(APSIM)/Model/Build/Platform.make

PROJECT = MicroMet

SRC = Registrations.f90 MicroMet.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
DEF = MicroMet.rsp
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
DEF = MicroMet.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

