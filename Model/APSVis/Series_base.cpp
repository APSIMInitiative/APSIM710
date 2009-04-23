#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "drawable.h"

#include "Series_base.h"
#include <general\date_class.h>
/**# implementation Series_base:: id(C_0853986465)
*/

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
Series_base::Series_base (void)
   {
   Display_labels = false;
   Series_ptr = NULL;
   x_data_type = Series_base::numeric;
   y_data_type = Series_base::numeric;
   Num_points_so_far = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
Series_base::~Series_base (void)
   {
//   if (Series_ptr != NULL)
//      delete Series_ptr;
   }

// ------------------------------------------------------------------
//  Short description:
//    return chart series pointer to caller.  Only called from plot.

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
TChartSeries* Series_base::Get_chart_series (void)
   {
   return Series_ptr;
   }

// ------------------------------------------------------------------
//  Short description:
//    communicate with the TChart object.

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
void Series_base::Design (TChart* TChart_ptr)
   {
   if (Series_ptr != NULL)
      {
//      Series_ptr->ParentChart = TChart_ptr;
      TChart_ptr->AddSeries(Series_ptr);
      Series_ptr->Marks->Visible = Display_labels;
      Series_ptr->Title = Title.c_str();
      if (x_data_type == Series_base::date)
         Series_ptr->XValues->DateTime = true;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    add data to the TChart object.

//  Notes:

//  Changes:
//    DPH 6/2/1997
//    dph 10/3/97 problem with dates < 1900.  TDateTime doesn't support
//                dates before 1900 - D-126

// ------------------------------------------------------------------
void Series_base::Add_data (const char* X, const char* Y)
   {
   if (Series_ptr == NULL)
      {
      Series_ptr = Create_chart_series (TChart_ptr);
      Series_ptr->XValues->Order = loNone;
      Series_ptr->YValues->Order = loNone;
      }

   double Par1;
   double Par2 = 0.0;
   string Par3;

   Num_points_so_far++;
   if (x_data_type == Series_base::numeric)
      Par1 = atof (X);

   else if (x_data_type == Series_base::alpha)
      {
      Par1 = Num_points_so_far;
      Par3 = X;
      }
   else if (x_data_type == Series_base::date)
      {
      GDate date;
      date.Read(X);
      int numDays = date-GDate(30,12,1899);
      Par1 = TDateTime(numDays);
      }

   else
      {
      GDate d;
      d.Read(X);
      Par1 = d.Get_jday();
      string st;
      d.Write (st);
      Par3 = st;
      }
   if (y_data_type == Series_base::numeric)
      Par2 = atof (Y);
   else if (y_data_type == Series_base::date)
      {
      GDate d;
      d.Read(Y);
      Par2 = d.Get_jday();
      }

   Series_ptr->AddXY (Par1, Par2, Par3.c_str(), TColor(clTeeColor));
   }

