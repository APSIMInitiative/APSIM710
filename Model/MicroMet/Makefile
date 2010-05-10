include $(APSIM)/Model/Build/Platform.make

PROJECT = MicroMet

SRC = Registrations.f90 MicroMet.for

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface
DEF = MicroMet.def

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

