#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "drawable.h"

#include "Plot.h"

/**# implementation Plot:: id(C_0853986189) 
*/

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
Plot::Plot (void)
   {
   Data_series = NULL;
   X_axis.Axis_type = Axis::Bottom;
   Y_axis.Axis_type = Axis::Left;
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
Plot::~Plot (void)
   {
   if (Data_series != NULL)
      delete Data_series;
   }

// ------------------------------------------------------------------
//  Short description:
//    communicate with tchart

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
void Plot::Design(TChart* TChart_ptr)
   {
   X_axis.Design(TChart_ptr);
   Y_axis.Design(TChart_ptr);
   if (Data_series != NULL)
      {
      Data_series->Design(TChart_ptr);
      if (Data_series->Get_chart_series() != NULL)
         {
         Data_series->Get_chart_series()->HorizAxis = (THorizAxis) X_axis.Axis_type;
         Data_series->Get_chart_series()->VertAxis = (TVertAxis) (Y_axis.Axis_type - 2);
         }
      }
   TChart_ptr->BackColor = TColor(RGB(Fill.Colour.Red, Fill.Colour.Green, Fill.Colour.Blue));
   }

