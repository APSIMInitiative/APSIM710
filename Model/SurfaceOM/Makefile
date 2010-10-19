include $(APSIM)/Model/Build/Platform.make

PROJECT = SurfaceOM

SRC = Registrations.f90 SurfaceOM.f90 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
DEF = SurfaceOM.rsp
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
DEF = SurfaceOM.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

