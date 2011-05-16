include $(APSIM)/Model/Build/Platform.make

PROJECT = Apsim

SRC = ApsimMain.cpp Simulation.cpp ../ComponentInterface/message.cpp

LIBS = ApsimShared General Protocol ComponentInterface

EXTRALIBS = -L$(MONO)/lib -lmono-2.0 

PROJECTTYPE = exe

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

