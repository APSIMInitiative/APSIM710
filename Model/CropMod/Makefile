include $(APSIM)/Model/Build/Platform.make

PROJECT = CropMod

SRC = CropModData.for CropMod.for CropModMain.for

STATICLIBS = FortranInfrastructure CropTemplate
LIBS = ComponentInterface

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make
