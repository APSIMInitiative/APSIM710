include $(APSIM)/Model/Build/Platform.make

PROJECT = Operations

SRC = Registrations.f90 Operations.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
DEF = Operations.ldscript
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
DEF = Operations.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface ApsimShared

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

