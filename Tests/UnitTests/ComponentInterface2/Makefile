include $(APSIM)/Model/Build/Platform.make

PROJECT = Test

SRC = testComponentInterface.cpp \
      TestVariables.cpp \
      TestParameters.cpp \
      TestEvents.cpp \
      TestMisc.cpp

ifeq ($(PLATFORM),Linux)
  LDDEBUGFLAGS := -lboost_unit_test_framework-mt
  LIBS = General ApsimShared ComponentInterface2
else 
 ifdef DEBUG
   LIBS = General ApsimShared ComponentInterface2 libboost_unit_test_framework-vc100-mt-gd-1_44
 else
   LIBS = General ApsimShared ComponentInterface2 libboost_unit_test_framework-vc100-mt-1_44
 endif
endif

PROJECTTYPE = exe


include $(APSIM)/Model/Build/$(PLATFORM)CPP.make


