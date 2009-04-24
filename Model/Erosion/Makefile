include $(APSIM)/Model/Build/Platform.make

PROJECT = Erosion

SRC = Registrations.f90 erosion.for
STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make
