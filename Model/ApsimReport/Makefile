include $(APSIM)/Model/Build/Platform.make

PROJECT = ApsimReport

SRC = ApsimReport.cpp \
      TMainForm.cpp \
      TPageSetupForm.cpp
      
LIBS = ApsimShared General GeneralVCL gtQRC60Pro goose SEGReport
BCB = c:\progra~1\Borland\CBuilder6
INCLUDEPATH := $(BCB)\Components\QRExport3\Lib\C6;$(BCB)\Components\MemTable;$(BCB)\Components\Dream\Units;(BCB)\components\goose-0.0.11;$(BCB)\components\goose-0.0.11\src\containers;$(BCB)\components\goose-0.0.11\src\util;$(BCB)\components\goose-0.0.11\src\stats;$(BCB)\components\goose-0.0.11\src\specfns;$(BCB)\Components\TeeChart\CBuilder6\Include;$(BCB)\Components\thirdparty
LIBPATH := $(BCB)\components\thirdparty;$(BCB)\Components\TeeChart\CBuilder6\Lib;$(BCB)\Components\QRExport3\Lib\C6;$(BCB)\Components\Dream\Units;$(BCB)\Components\MemTable;$(BCB)\Components\Goose-0.0.11\src

PROJECTTYPE = exe
VCL = yes
RES = ApsimReport.res

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
