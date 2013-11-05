include $(APSIM)/Model/Build/Platform.make

PROJECT = Test

SRC = test.cpp \
      testControlFileConverter.cpp \
      testApsimControlFile.cpp \
      testApsimDataFile.cpp \
      ../../../Model/ConToSim/ControlFileConverter.cpp

ifeq ($(PLATFORM),Linux)
  LDDEBUGFLAGS := -lboost_unit_test_framework-mt
  LIBS = General ApsimShared 
else 
 ifdef DEBUG
   LIBS = General ApsimShared libboost_unit_test_framework-vc100-mt-gd-1_54
 else
   LIBS = General ApsimShared libboost_unit_test_framework-vc100-mt-1_54
 endif
endif

PROJECTTYPE = exe

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make


