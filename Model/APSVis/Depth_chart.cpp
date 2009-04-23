#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "depth_chart.h"
#include "scatter_format.h"

// ------------------------------------------------------------------
//  Short description:
//    initialise

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Depth_chart::Depth_chart (TForm* parent)
   : High_level_chart_base (parent)
   {
   Accumulate_y = true;
   Accumulated_value = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//    add an xy pair to chart.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Depth_chart::Add_xy (const char* X_field_name,
                          const char* Y_field_name,
                          Format_base* Format_ptr,
                          const char* Title)
   {
   // assign a default scatter format if caller didn't supply one.
   Format_base* New_format;
   if (Format_ptr == NULL)
      New_format = new Scatter_format (Scatter_format::Lines);
   else
      New_format = Format_ptr;

   // assign a default title if caller didn't supply one.
   string New_title;
   if (Title == NULL)
      New_title = X_field_name;
   else
      New_title = Title;

   // send data to base class.
   Add_xy_pair (X_field_name,
                Y_field_name,
                numeric,
                numeric,
                top,
                ::left,
                New_format,
                New_title.c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//    create the necessary plots and configure them from information
//    in the field descriptor formats.

//  Notes:

//  Changes:
//    DPH 21/4/1997

// ------------------------------------------------------------------
void Depth_chart::Format_plot (Plot* Plot_ptr,
                               XY_pair* XY_ptr,
                               const char* Title_of_table)
   {
   High_level_chart_base::Format_plot (Plot_ptr, XY_ptr, Title_of_table);

   Plot_ptr->Y_axis.Inverted = true;
   Plot_ptr->Y_axis.Minimum_fixed = true;
   Plot_ptr->Y_axis.Minimum = 0.0;

   Plot_ptr->X_axis.Minimum_fixed = true;
   Plot_ptr->X_axis.Minimum = 0.0;

   }

// ------------------------------------------------------------------
//  Short description:
//    return true if caller needs to create a new set of plots/series
//    for each xy pair.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
bool Depth_chart::Need_to_create_new_plots (Table_base& Table_obj)
   {
   bool New_plots = High_level_chart_base::Need_to_create_new_plots (Table_obj);

   if (New_plots)
      Accumulated_value = 0.0;

   return New_plots;
   }

// ------------------------------------------------------------------
//  Short description:
//    called to retrieve the next xy point.  Return true if this is
//    the last point for this plot.

//  Notes:
//    Assumes table_ptr is an open table.

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
bool Depth_chart::Get_next_point (Table_base& Table_obj,
                                  char* x_value,
                                  char* y_value)
   {
   bool result = High_level_chart_base::Get_next_point (Table_obj,
                                                   x_value,
                                                   y_value);

   // do we need to accumulate the y values?
   if (Accumulate_y)
      {
      double thickness = atof(y_value);
      Accumulated_value += thickness;
      sprintf(y_value, "%7.1f", Accumulated_value - thickness / 2.0);
      }
   return result;
   }

