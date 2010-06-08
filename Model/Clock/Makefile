include $(APSIM)/Model/Build/Platform.make

PROJECT = Clock

SRC = Registrations.f90 clock.for clock_read_params.for
STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface
DEF = Clock.def

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

