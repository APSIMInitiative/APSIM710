include $(APSIM)/Model/Build/Platform.make

PROJECT = Test

SRC = Test.cpp \
      TestXml.cpp \
      TestIoFunctions.cpp \
      TestPathClass.cpp \
      TestMathFunctions.cpp \
      TestStringFunctions.cpp \
      TestDateClass.cpp \
      TestHttp.cpp

ifdef DEBUG
  LIBS = General libboost_unit_test_framework-vc100-mt-gd-1_54
else
  LIBS = General libboost_unit_test_framework-vc100-mt-1_54
endif

PROJECTTYPE = exe


include $(APSIM)/Model/Build/$(PLATFORM)CPP.make


