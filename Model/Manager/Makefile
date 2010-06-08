include $(APSIM)/Model/Build/Platform.make

PROJECT = Manager

SRC = Registrations.f90 Manager.for
STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface ApsimShared
DEF = Manager.def

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make
