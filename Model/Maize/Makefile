include $(APSIM)/Model/Build/Platform.make

PROJECT = Maize

SRC = maize.for 
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux.for CropModData.for
DEF = ../Build/gfcomp.ldscript
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints.for CropModData.for
DEF = ../Build/gfcomp.def
endif

STATICLIBS = FortranInfrastructure CropTemplate
LIBS = FortranComponentInterface


APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

CropModData.obj:	../CropMod/CropModData.for
	$(LF95) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

CropModData.o:	../CropMod/CropModData.for
	$(FC) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)





