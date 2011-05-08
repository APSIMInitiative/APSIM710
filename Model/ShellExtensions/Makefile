include $(APSIM)/Model/Build/Platform.make

PROJECT = ShellExtensions

SRC = ContextHandler.cpp \
      ApsimCommands.cpp \
      ApsimContextMenu.cpp \
      ClsFact.cpp \
      ..\General\Path.cpp \
      ..\General\String_functions.cpp \
      ..\General\StriStr.cpp \
      ..\General\Stream_functions.cpp \
      ..\General\io_functions.cpp \
      ..\ApsimShared\ApsimDirectories.cpp

RES = Icons.res
PROJECTTYPE = dll
VCL = yes
LIBS = ole32 Rpcrt4 ShLwApi

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
