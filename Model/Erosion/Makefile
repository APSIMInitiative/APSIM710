include $(APSIM)/Model/Build/Platform.make

PROJECT = Erosion

SRC = Registrations.f90 erosion.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
DEF = Erosion.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make
