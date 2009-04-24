include $(APSIM)/Model/Build/Platform.make

PROJECT = Parasite

SRC = Registrations.f90 Parasite.for
STATICLIBS = FortranInfrastructure CropTemplate
LIBS = FortranComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

