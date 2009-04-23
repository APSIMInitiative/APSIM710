#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "drawable.h"

#include "XY_series.h"

/**# implementation XY_series:: id(C_0855183444)
*/

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
XY_series::XY_series(void)
   {
   Lines_visible = false;
   Marker_style = Rectangle;
   Line_series_ptr = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
XY_series::~XY_series(void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    create a series interface object.

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
TChartSeries* XY_series::Create_chart_series (TChart* TChart_ptr)

   {
   Line_series_ptr = new TLineSeries(TChart_ptr);
   return Line_series_ptr;
   }

// ------------------------------------------------------------------
//  Short description:
//    communicate with the TChart object.

//  Notes:

//  Changes:
//    DPH 7/2/1997

// ------------------------------------------------------------------
void XY_series::Design (TChart* TChart_ptr)
   {
   Series_base::Design(TChart_ptr);

   // set marker stuff.
   if (Line_series_ptr != NULL)
      {
      Outline.Set_TChartpen(Line_series_ptr->Pointer->Pen);
      Outline.Set_TChartpen(Line_series_ptr->LinePen);
      Line_series_ptr->SeriesColor = COLORREF(Outline.Colour);
      if (Marker_style == None)
         {
         Line_series_ptr->Pointer->Visible = false;
         }
      else
         {
         Line_series_ptr->Pointer->Visible = true;
         Line_series_ptr->Pointer->Style = (TSeriesPointerStyle) (Marker_style - 1);
         }

      // set lines between markers to off if necessary.
      if (!Lines_visible)
         Line_series_ptr->LinePen->Style = psClear;
      }
   }

