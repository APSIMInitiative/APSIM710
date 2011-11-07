include $(APSIM)/Model/Build/Platform.make

PROJECT = Protocol

SRC = Protocol.cpp Transport.cpp Computation.cpp

LIBS = General ApsimShared
EXTRALIBS = -L$(MONO_PREFIX)/lib -lmono-2.0 
PROJECTTYPE = libdll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
