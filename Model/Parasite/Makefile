include $(APSIM)/Model/Build/Platform.make

PROJECT = Parasite

SRC = Registrations.f90 Parasite.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
endif

STATICLIBS = FortranInfrastructure CropTemplate
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

