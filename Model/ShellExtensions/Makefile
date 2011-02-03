include $(APSIM)/Model/Build/Platform.make

PROJECT = ShellExtensions

SRC = ContextHandler.cpp \
      ApsimCommands.cpp \
      ApsimContextMenu.cpp \
      ClsFact.cpp \
      ..\General\Path.cpp \
      ..\General\String_functions.cpp \
      ..\General\Xml.cpp \
      ..\General\StriStr.cpp \
      ..\General\Stream_functions.cpp \
      ..\General\io_functions.cpp \
      ..\ApsimShared\ApsimDirectories.cpp \
      ..\ApsimShared\ApsimVersion.cpp \
      ..\ApsimShared\ApsimSettings.cpp

RES = Icons.res
PROJECTTYPE = dll
VCL = yes
LIBS = ole32 Rpcrt4

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
