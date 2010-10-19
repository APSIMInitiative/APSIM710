include $(APSIM)/Model/Build/Platform.make

PROJECT = Fertiliser

SRC = Fertiliser.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux2.for 
DEF = Fertiliser.rsp
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints2.for 
DEF = Fertiliser.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface2

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

