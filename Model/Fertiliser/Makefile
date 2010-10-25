include $(APSIM)/Model/Build/Platform.make

PROJECT = Fertiliser

SRC = Fertiliser.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux2.for 
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints2.for 
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface2
EXPORTS = OnTick,OnNewProfile,OnApply,getDescription,getDescriptionLength

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FORNew.make

