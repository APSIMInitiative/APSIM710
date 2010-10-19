include $(APSIM)/Model/Build/Platform.make

PROJECT = Manager

SRC = Registrations.f90 Manager.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for 
DEF = Manager.rsp
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for 
DEF = Manager.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface ApsimShared

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make
