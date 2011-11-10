include $(APSIM)/Model/Build/Platform.make

PROJECT = ShellExtensions

SRC = ContextHandler.cpp \
      ApsimCommands.cpp \
      ApsimContextMenu.cpp \
      ClsFact.cpp 


RES = Icons.res
PROJECTTYPE = dll
VCL = yes
LIBS = ole32 Rpcrt4 ShLwApi

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

