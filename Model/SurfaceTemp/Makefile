include $(APSIM)/Model/Build/Platform.make

PROJECT = SurfaceTemp

SRC = Registrations.f90 SurfaceTemp.for
STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

