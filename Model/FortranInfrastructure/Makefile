include $(APSIM)/Model/Build/Platform.make

PROJECT = FortranInfrastructure

SRC1 = DataTypes.f90 \
      DataTypesInterface.f90 \
      ScienceAPI.f90 \
      ComponentInterfaceModule.f90 \
      ConvertModule.f90 \
      ConstantsModule.f90 \
      error.f90 \
      string.f90 \
      data.f90 \
      datastr.f90 \
      date.f90 \
      science.f90  \
      read.f90 \
      DOTNETModule.f90 \
      PublishEvents.f90 \
      Infrastructure.f90 \
      Infrastructure2.f90

ifeq ($(PLATFORM),WIN32)
      SRC = $(SRC1) EntryPoints.for 
endif

ifeq ($(PLATFORM),Linux)
      SRC = $(SRC1) EntryPointsLinux.for
endif

PROJECTTYPE = lib

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make
