include $(APSIM)/Model/Build/Platform.make

PROJECT = Protocol

SRC = Protocol.cpp Transport.cpp Computation.cpp

ifeq ($(PLATFORM),Linux)
  CPPFLAGS := -DMONO_MAJOR=$(shell mono --version | head -1 | cut -c 27- | cut -c -1) -DMONO_MINOR=$(shell mono --version | head -1 | cut -c 29- | cut -c -2)
endif

LIBS = General ApsimShared
EXTRALIBS = -L$(MONO_PREFIX)/lib -lmono-2.0 
PROJECTTYPE = libdll

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
