include $(APSIM)/Model/Build/Platform.make

PROJECT = Oryza2

SRC = ./Source/VARIABLE.F90 \
		./ApsimModules.f90 \
		./Source/OR_ROOTG.F90 \
		./Source/quadpack.F90 \
		./Source/NightT.f90 \
		./Source/SUBDD.F90 \
		./Source/SUBCD2.F90 \
		./Source/OR_PHENOL.F90 \
		./Source/ET2.F90 \
		./Source/GPPARGET.F90 \
		./Source/GPPARSET.F90 \
		./Source/Ncrop3.f90 \
		./Source/Nnostress2.f90 \
		./Source/PARTITION.F90 \
		./Source/SASTRO.F90 \
		./Source/SETPMD.F90 \
		./Source/SETMKD.F90 \
		./Source/SETPTD.F90 \
		./Source/SGPC1.F90 \
		./Source/SGPC2.F90 \
		./Source/SGPCDT.F90 \
		./Source/SGPL.F90 \
		./Source/SRDPRF.F90 \
		./Source/SSKYC.F90 \
		./Source/SUBCBC.F90 \
		./Source/SUBGRN.F90 \
		./Source/SUBLAI3.F90 \
		./Source/SVPS1.F90 \
		./Source/wnostress.f90 \
		./Source/WStress2.f90  \
		./Source/ORYZA1.f90 \
		./Oryza2.f90 \
		./ApsimWrapper.f90 \
		./outdat.cpp 

EXTRACOMPILEFLAGS = -O0 -I$(APSIM) 
EXTRALIBS = -lstdc++
		
ifeq ($(PLATFORM),Linux)
SRC := $(SRC) ../FortranInfrastructure/EntryPointsLinux2.for 
DEF = Oryza2.ldscript
else
SRC := $(SRC) ../FortranInfrastructure/EntryPoints2.for 
DEF = Oryza2.def
endif

STATICLIBS = FortranInfrastructure
LIBS = FortranComponentInterface2

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

