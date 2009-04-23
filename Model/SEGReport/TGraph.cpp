//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TGraph.h"
#include <General\string_functions.h>
#include <Generalvcl\vcl_functions.h>
#include <General\stl_functions.h>
#include <Generalvcl\db_functions.h>
#include <TeEngine.hpp>
#include <TeeEdit.hpp>
#include <DBEditCh.hpp>
#include <EditChar.hpp>
#include <Series.hpp>
#include <sstream>
#include "ReportMacros.h"
using namespace std;
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TGraph *)
{
   new TGraph(NULL);
}
//---------------------------------------------------------------------------
namespace tGraph
{
   void __fastcall PACKAGE Register()
   {
       TComponentClass classes[1] = {__classid(TGraph)};
       RegisterComponents("SEG", classes, 0);
   }
}
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TGraph::TGraph(TComponent* Owner)
   : TgtQRChart(Owner)
   {
   Frame->Style = psClear;
   if (Chart != NULL)
      {
      // Some defaults that make the charts look better.
      Chart->View3D = false;
      Chart->Legend->LegendStyle = lsSeries;
      Chart->LeftAxis->Grid->Visible = false;
      Chart->LeftAxis->TickLength = 7;
      Chart->LeftAxis->LabelsSeparation = 100;
      Chart->LeftAxis->MinorTicks->Visible = false;
      Chart->LeftAxis->AxisValuesFormat = "###0.###";
      Chart->TopAxis->Grid->Visible = false;
      Chart->TopAxis->TickLength = 7;
      Chart->TopAxis->LabelsSeparation = 100;
      Chart->TopAxis->MinorTicks->Visible = false;
      Chart->TopAxis->AxisValuesFormat = "###0.###";
      Chart->RightAxis->Grid->Visible = false;
      Chart->RightAxis->TickLength = 7;
      Chart->RightAxis->LabelsSeparation = 100;
      Chart->RightAxis->MinorTicks->Visible = false;
      Chart->RightAxis->AxisValuesFormat = "###0.###";
      Chart->BottomAxis->Grid->Visible = false;
      Chart->BottomAxis->TickLength = 7;
      Chart->BottomAxis->LabelsSeparation = 100;
      Chart->BottomAxis->MinorTicks->Visible = false;
      Chart->BottomAxis->AxisValuesFormat = "###0.###";
      Chart->BackWall->Visible = false;
      }
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TGraph::~TGraph(void)
   {
   }
//---------------------------------------------------------------------------
// set the NumMonths property.
//---------------------------------------------------------------------------
void __fastcall TGraph::setNumMonths(int NumMonths)
   {
   bottomAxisScaleMonths = NumMonths;
   refresh();
   }
//---------------------------------------------------------------------------
// Read in additional properties from stream.
//---------------------------------------------------------------------------
void __fastcall TGraph::LoadStringProperty(TReader *Reader)
   {
   *stRef = Reader->ReadString();
   if (*stRef == " ")
      *stRef = "";
   }
//---------------------------------------------------------------------------
// Write out additional properties to stream.
//---------------------------------------------------------------------------
void __fastcall TGraph::StoreStringProperty(TWriter *Writer)
   {
   if (*stRef == "")
      Writer->WriteString(" ");
   else
      Writer->WriteString(*stRef);
   }
//---------------------------------------------------------------------------
// called by filer to let us define all our properties.
//---------------------------------------------------------------------------
void __fastcall TGraph::DefineProperties(TFiler *Filer)
   {
   TgtQRChart::DefineProperties(Filer);
   stRef = &title;
   Filer->DefineProperty("title", LoadStringProperty, StoreStringProperty, true);
   stRef = &subTitle;
   Filer->DefineProperty("subTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &leftAxisTitle;
   Filer->DefineProperty("leftAxisTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &topAxisTitle;
   Filer->DefineProperty("topAxisTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &rightAxisTitle;
   Filer->DefineProperty("rightAxisTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &bottomAxisTitle;
   Filer->DefineProperty("bottomAxisTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &footTitle;
   Filer->DefineProperty("footTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &seriesTitle1;
   Filer->DefineProperty("seriesTitle1", LoadStringProperty, StoreStringProperty, true);
   stRef = &seriesTitle2;
   Filer->DefineProperty("seriesTitle2", LoadStringProperty, StoreStringProperty, true);
   stRef = &seriesTitle3;
   Filer->DefineProperty("seriesTitle3", LoadStringProperty, StoreStringProperty, true);
   stRef = &seriesTitle4;
   Filer->DefineProperty("seriesTitle4", LoadStringProperty, StoreStringProperty, true);
   stRef = &seriesTitle5;
   Filer->DefineProperty("seriesTitle5", LoadStringProperty, StoreStringProperty, true);
   }
//---------------------------------------------------------------------------
// Component has finished loading.
//---------------------------------------------------------------------------
void __fastcall TGraph::Loaded(void)
   {
   try
      {
      TgtQRChart::Loaded();
      }
   catch (Exception* err)
      {
      }
   catch (exception& err)
      {
      }
   }
//---------------------------------------------------------------------------
// refresh the chart
//---------------------------------------------------------------------------
void TGraph::refresh(void)
   {
   if (!ComponentState.Contains(csLoading))
      {
      try
         {
         Chart->RefreshData();
         Chart->Refresh();

         if (Chart->SeriesCount() == 1)
            {
            TPieSeries* pieSeries = dynamic_cast<TPieSeries*> (Chart->Series[0]);
            if (pieSeries != NULL)
               {
               static TColor OurColors[5] = {0x004080, 0x0080FF, clLtGray, clGreen, clBlue};
               SetDefaultColorPalette(OurColors, 5);
               }
            }


         replaceChartMacros();
         fixBottomAxisScaling();
         }
      catch (Exception* err)
         {
         }
      catch (exception& err)
         {
         }
      }

   }
//---------------------------------------------------------------------------
// replace all chart macros.
//---------------------------------------------------------------------------
void TGraph::replaceChartMacros(void)
   {
   if (title != "")
      Chart->Title->Text->Text = ReportMacros::resolve(Owner, title.c_str()).c_str();
   if (subTitle != "")
      Chart->SubTitle->Text->Text = ReportMacros::resolve(Owner, subTitle.c_str()).c_str();
   if (leftAxisTitle != "")
      Chart->LeftAxis->Title->Caption = ReportMacros::resolve(Owner, leftAxisTitle.c_str()).c_str();
   if (topAxisTitle != "")
      Chart->TopAxis->Title->Caption = ReportMacros::resolve(Owner, topAxisTitle.c_str()).c_str();
   if (rightAxisTitle != "")
      Chart->RightAxis->Title->Caption = ReportMacros::resolve(Owner, rightAxisTitle.c_str()).c_str();
   if (bottomAxisTitle != "")
      Chart->BottomAxis->Title->Caption = ReportMacros::resolve(Owner, bottomAxisTitle.c_str()).c_str();
   if (footTitle != "")
      Chart->Foot->Text->Text = ReportMacros::resolve(Owner, footTitle.c_str()).c_str();
   if (Chart->SeriesCount() >= 1 && seriesTitle1 != "" && seriesTitle1 != "$seriesName")
      Chart->Series[0]->Title = ReportMacros::resolve(Owner, seriesTitle1.c_str()).c_str();
   if (Chart->SeriesCount() >= 2 && seriesTitle2 != "")
      Chart->Series[1]->Title = ReportMacros::resolve(Owner, seriesTitle2.c_str()).c_str();
   if (Chart->SeriesCount() >= 3 && seriesTitle3 != "")
      Chart->Series[2]->Title = ReportMacros::resolve(Owner, seriesTitle3.c_str()).c_str();
   if (Chart->SeriesCount() >= 4 && seriesTitle4 != "")
      Chart->Series[3]->Title = ReportMacros::resolve(Owner, seriesTitle4.c_str()).c_str();
   if (Chart->SeriesCount() >= 5 && seriesTitle5 != "")
      Chart->Series[4]->Title = ReportMacros::resolve(Owner, seriesTitle5.c_str()).c_str();
   }
//---------------------------------------------------------------------------
// Let the user edit the chart.
//---------------------------------------------------------------------------
void TGraph::userEdit(void)
   {
   Chart->Title->Text->Text = title;
   Chart->SubTitle->Text->Text = subTitle;
   Chart->LeftAxis->Title->Caption = leftAxisTitle;
   Chart->TopAxis->Title->Caption = topAxisTitle;
   Chart->RightAxis->Title->Caption = rightAxisTitle;
   Chart->BottomAxis->Title->Caption = bottomAxisTitle;
   Chart->Foot->Text->Text = footTitle;
   if (Chart->SeriesCount() >= 1)
      Chart->Series[0]->Title = seriesTitle1;
   if (Chart->SeriesCount() >= 2)
      Chart->Series[1]->Title = seriesTitle2;
   if (Chart->SeriesCount() >= 3)
      Chart->Series[2]->Title = seriesTitle3;
   if (Chart->SeriesCount() >= 4)
      Chart->Series[3]->Title = seriesTitle4;
   if (Chart->SeriesCount() >= 5)
      Chart->Series[4]->Title = seriesTitle5;

   TDataSetSeriesSource* dummy = new TDataSetSeriesSource((TComponent*)NULL);

   TeeSaveBoolOption(TeeMsg_TreeMode, true);
   EditChart(NULL, this->Chart);
   delete dummy;

   title = Chart->Title->Text->Text;
   subTitle = Chart->SubTitle->Text->Text;
   leftAxisTitle = Chart->LeftAxis->Title->Caption;
   topAxisTitle = Chart->TopAxis->Title->Caption;
   rightAxisTitle = Chart->RightAxis->Title->Caption;
   bottomAxisTitle = Chart->BottomAxis->Title->Caption;
   footTitle = Chart->Foot->Text->Text;
   if (Chart->SeriesCount() >= 1)
      seriesTitle1 = Chart->Series[0]->Title;
   if (Chart->SeriesCount() >= 2)
      seriesTitle2 = Chart->Series[1]->Title;
   if (Chart->SeriesCount() >= 3)
      seriesTitle3 = Chart->Series[2]->Title;
   if (Chart->SeriesCount() >= 4)
      seriesTitle4 = Chart->Series[3]->Title;
   if (Chart->SeriesCount() >= 5)
      seriesTitle5 = Chart->Series[4]->Title;
   replaceChartMacros();
   }

//---------------------------------------------------------------------------
// When the x axis is a date time axis and it's interval is set to 1 month
// and the data range is less than a month, then the axis doesn't show anything.
// this method fixes that problem.
//---------------------------------------------------------------------------
void TGraph::fixBottomAxisScaling()
   {
   TChartAxis* BottomAxis = Chart->BottomAxis;
   if (bottomAxisScaleMonths > 0)
      {
      BottomAxis->AdjustMaxMin();
      if (BottomAxis->Minimum > 0 && BottomAxis->Maximum > 0)
         {
         TDateTime MinDate = TDateTime(BottomAxis->Minimum);
         unsigned short year, month, day;
         MinDate.DecodeDate(&year, &month, &day);

         month += bottomAxisScaleMonths;
         while (month > 12)
            {
            year++;
            month -= 12;
            }
         TDateTime MaxDate = TDateTime(year, month, day);
         BottomAxis->AutomaticMaximum = false;
         BottomAxis->Maximum = MaxDate;
         }
      }
   }
