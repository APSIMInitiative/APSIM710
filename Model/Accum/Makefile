include $(APSIM)/Model/Build/Platform.make

PROJECT = Accum

SRC = Registrations.f90 accum.for
STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make
