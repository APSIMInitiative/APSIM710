include $(APSIM)/Model/Build/Platform.make

PROJECT = Fertiliser

SRC = Registrations.f90 Fertiliser.for ../FortranInfrastructure/EntryPoints2.f90
STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface2
EXPORTS = OnTick,OnNewProfile,OnApply,getDescription,getDescriptionLength

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FORNew.make

