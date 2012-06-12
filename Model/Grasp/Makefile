include $(APSIM)/Model/Build/Platform.make

PROJECT = Grasp

SRC =  Grasp.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux2.for 
DEF = Grasp.ldscript
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints2.for 
DEF = Grasp.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface2
EXPORTS = OnTick,OnNewProfile,OnApply,getDescription,getDescriptionLength

APSIMMODEL = Yes
PROJECTTYPE = dll

ifeq ($(PLATFORM),Linux)
include $(APSIM)/Model/Build/$(PLATFORM)FOR.make
else
include $(APSIM)/Model/Build/$(PLATFORM)FOR.make
endif
