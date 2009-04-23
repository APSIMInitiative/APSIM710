include $(APSIM)/Model/Build/Platform.make

PROJECT = SEGReport

SRC = ApsimFileReader.cpp \
      ComponentRegistration.cpp \
      Cumulative.cpp \
      DataContainer.cpp \
      DataProcessor.cpp \
      Depth.cpp \
      Diff.cpp \
      Filter.cpp \
      Frequency.cpp \
      Joiner.cpp \
      KWTest.cpp \
      PredObs.cpp \
      Probability.cpp \
      RecordFilter.cpp \
      Regression.cpp \
      REMS.cpp \
      Report2.cpp \
      ReportMacros.cpp \
      SEGReport.cpp \
      SeriesSplitter.cpp \
      SOI.cpp \
      Stats.cpp \
      TChartForm.cpp \
      TGraph.cpp \
      TGridForm.cpp \
      TImage.cpp \
      TImageForm.cpp \
      TPropertyForm.cpp \
      TShape.cpp \
      TShapeForm.cpp \
      TText.cpp \
      TTextForm.cpp \
      XmlFileReader.cpp

LIBS = ApsimShared General GeneralVCL gtQRC60Pro goose
BCB = c:\progra~1\Borland\CBuilder6
INCLUDEPATH := $(BCB)\Components\QRExport3\Lib\C6;$(BCB)\Components\MemTable;$(BCB)\Components\Dream\Units;(BCB)\components\goose-0.0.11;$(BCB)\components\goose-0.0.11\src\containers;$(BCB)\components\goose-0.0.11\src\util;$(BCB)\components\goose-0.0.11\src\stats;$(BCB)\components\goose-0.0.11\src\specfns;$(BCB)\Components\TeeChart\CBuilder6\Include
LIBPATH := $(BCB)\components\thirdparty;$(BCB)\Components\TeeChart\CBuilder6\Lib;$(BCB)\Components\QRExport3\Lib\C6;$(BCB)\Components\Dream\Units;$(BCB)\Components\MemTable;$(BCB)\Components\Goose-0.0.11\src

PROJECTTYPE = dll
VCL = yes

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

