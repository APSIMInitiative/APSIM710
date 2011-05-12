include $(APSIM)/Model/Build/Platform.make

PROJECT = Protocol

SRC = Protocol.cpp Transport.cpp Computation.cpp

LIBS = General ApsimShared
PROJECTTYPE = libdll

ifdef USE_MONO
EXTRALIBS=$(MONO_LIBS)
endif

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
