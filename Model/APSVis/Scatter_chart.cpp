#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "scatter_chart.h"
#include "scatter_format.h"

// ------------------------------------------------------------------
//  Short description:
//    initialise

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Scatter_chart::Scatter_chart (TForm* parent)
   : High_level_chart_base (parent)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    add an xy pair to chart.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Scatter_chart::Add_xy (const char* X_field_name,
                            const char* Y_field_name,
                            Field_type_enum X_type,
                            X_axis_link_enum X_axis_link,
                            Y_axis_link_enum Y_axis_link,
                            Format_base* Format_ptr,
                            const char* Title,
                            bool Accumulate_x,
                            bool Accumulate_y)
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
      New_title = Y_field_name;
   else
      New_title = Title;

   // send data to base class.
   Add_xy_pair (X_field_name,
                Y_field_name,
                X_type,
                numeric,
                X_axis_link,
                Y_axis_link,
                New_format,
                New_title.c_str(),
                Accumulate_x,
                Accumulate_y);
   }

