include $(APSIM)/Model/Build/Platform.make

PROJECT = SurfaceOM

SRC = Registrations.f90 SurfaceOM.f90
STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface
DEF = SurfaceOM.def

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

