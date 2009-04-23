//---------------------------------------------------------------------------

#ifndef TGraphH
#define TGraphH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <gtQrCtrls.hpp>
#include <QuickRpt.hpp>
class SeriesDataSource;
//---------------------------------------------------------------------------
// This chart component extends the base QuickReport Chart component by adding
// a series expansion capability.  This component will optionally look at the
// number of data series in the source dataset and make sure there is a chart
// series for each data series.
//---------------------------------------------------------------------------
class PACKAGE TGraph : public TgtQRChart
   {
   private:
      AnsiString title;
      AnsiString subTitle;
      AnsiString leftAxisTitle;
      AnsiString topAxisTitle;
      AnsiString rightAxisTitle;
      AnsiString bottomAxisTitle;
      AnsiString footTitle;
      AnsiString seriesTitle1;
      AnsiString seriesTitle2;
      AnsiString seriesTitle3;
      AnsiString seriesTitle4;
      AnsiString seriesTitle5;
      AnsiString* stRef;
      int bottomAxisScaleMonths;

      virtual void __fastcall DefineProperties(TFiler *Filer);
      void __fastcall LoadStringProperty(TReader *Reader);
      void __fastcall StoreStringProperty(TWriter *Writer);
      void __fastcall setNumMonths(int numMonths);

      virtual void __fastcall Loaded(void);
      void replaceChartMacros(void);
      void fixBottomAxisScaling();

   public:
      __fastcall TGraph(TComponent* Owner);
      __fastcall ~TGraph(void);

      void refresh(void);
      void userEdit(void);

   __published:
      __property int NumMonthsBottomAxis = {read=bottomAxisScaleMonths, write=setNumMonths};

   };
//---------------------------------------------------------------------------
#endif
