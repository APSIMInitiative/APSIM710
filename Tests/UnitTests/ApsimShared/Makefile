include $(APSIM)/Model/Build/Platform.make

PROJECT = Test

SRC = test.cpp \
      testControlFileConverter.cpp \
      testApsimControlFile.cpp \
      testApsimDataFile.cpp \
      ../../../Model/ConToSim/ControlFileConverter.cpp

ifdef DEBUG
  LIBS = General ApsimShared libboost_unit_test_framework-vc90-mt-gd-1_42
else
  LIBS = General ApsimShared libboost_unit_test_framework-vc90-mt-1_42
endif

PROJECTTYPE = exe


include $(APSIM)/Model/Build/$(PLATFORM)CPP.make


