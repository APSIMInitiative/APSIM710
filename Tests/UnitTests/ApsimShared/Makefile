include $(APSIM)/Model/Build/Platform.make

PROJECT = Test

SRC = test.cpp \
      testControlFileConverter.cpp \
      testApsimControlFile.cpp \
      testApsimDataFile.cpp \
      $(APSIM)\Model\ConToSim\ControlFileConverter.cpp

LIBS = General ApsimShared libboost_unit_test_framework-vc90-mt-1_42
PROJECTTYPE = exe


include $(APSIM)/Model/Build/$(PLATFORM)CPP.make


