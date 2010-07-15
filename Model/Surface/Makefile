include $(APSIM)/Model/Build/Platform.make

PROJECT = Surface

SRC = Registrations.f90 Surface.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

