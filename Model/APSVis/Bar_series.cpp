#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "drawable.h"

#include "bar_series.h"

/**# implementation Bar_series:: id(C_0855183444)
*/

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
Bar_series::Bar_series(void)
   {
   Orientation = Vertical;
   Multi_bar = Side_by_side;
   Custom_bar_series_ptr = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//     destructor

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
Bar_series::~Bar_series(void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    create a series interface object.

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
TChartSeries* Bar_series::Create_chart_series (TChart* TChart_p)
   {
   TChart_ptr = TChart_p;

   if (Orientation == Vertical)
      Custom_bar_series_ptr = new TBarSeries(TChart_ptr);
   else
      Custom_bar_series_ptr = new THorizBarSeries(TChart_ptr);

   return Custom_bar_series_ptr;
   }

// ------------------------------------------------------------------
//  Short description:
//    communicate with the TChart object.

//  Notes:

//  Changes:
//    DPH 7/2/1997

// ------------------------------------------------------------------
void Bar_series::Design (TChart* TChart_ptr)
   {
   Series_base::Design(TChart_ptr);

   if (Custom_bar_series_ptr != NULL)
      {
      // don't colour each point.
      Custom_bar_series_ptr->ColorEachPoint = false;

      // set the brush.
      Fill.Set_TChartBrush(Custom_bar_series_ptr->BarBrush);
      Custom_bar_series_ptr->SeriesColor = COLORREF(Fill.Colour);

      // set the brush pen
      Custom_bar_series_ptr->BarPen->Visible = false;

      // set the multi bar property.
      Custom_bar_series_ptr->MultiBar = (TMultiBar) (Multi_bar + 1);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    set the orientation of the bar chart.

//  Notes:

//  Changes:
//    DPH 7/2/1997

// ------------------------------------------------------------------
void Bar_series::Set_orientation (Orientation_enum Orient)
   {
   Orientation = Orient;

   // do we already have a bar series created?
   if (Custom_bar_series_ptr != NULL)
      {
      // yes - get pointer to existing one.
      TChartSeries* Old_series_ptr = Custom_bar_series_ptr;

      // create new one.
      TChartSeries* New_series_ptr = Create_chart_series (NULL);

      // copy all data from old series to new series.
      New_series_ptr->AssignValues (Old_series_ptr);

      // delete the old series.
      delete Old_series_ptr;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    return the orientation of the bar chart.

//  Notes:

//  Changes:
//    DPH 7/2/1997

// ------------------------------------------------------------------
Bar_series::Orientation_enum Bar_series::Get_orientation (void)
   {
   return Orientation;
   }

