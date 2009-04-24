include $(APSIM)/Model/Build/Platform.make

PROJECT = Canopy

SRC = Registrations.f90 canopy.for
STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make
