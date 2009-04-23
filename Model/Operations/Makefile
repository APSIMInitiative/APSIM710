include $(APSIM)/Model/Build/Platform.make

PROJECT = Operations

SRC = Registrations.f90 Operations.for
STATICLIBS = FortranInfrastructure
LIBS = ComponentInterface ApsimShared

APSIMMODEL = Yes
PROJECTTYPE = dll

include $(APSIM)/Model/Build/$(PLATFORM)FOR.make

