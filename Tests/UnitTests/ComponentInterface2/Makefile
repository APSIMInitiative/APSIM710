include $(APSIM)/Model/Build/Platform.make

PROJECT = Test

SRC = testComponentInterface.cpp \
      TestVariables.cpp \
      TestParameters.cpp \
      TestEvents.cpp \
      TestMisc.cpp

ifdef DEBUG
  LIBS = General ApsimShared ComponentInterface2 libboost_unit_test_framework-vc90-mt-gd-1_42
else
  LIBS = General ApsimShared ComponentInterface2 libboost_unit_test_framework-vc90-mt-1_42
endif

PROJECTTYPE = exe


include $(APSIM)/Model/Build/$(PLATFORM)CPP.make


