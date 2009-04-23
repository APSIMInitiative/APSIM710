#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Frequency_chart.h"
#include "bar_format.h"
#include <strstrea.h>
#include <functional>

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Frequency_chart::Frequency_chart(TForm* parent)
   : Probability_chart (parent)
   {
   Frequency_type = count;
   }

// ------------------------------------------------------------------
//  Short description:
//    add an xy pair to chart.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Frequency_chart::Add_xy (const char* X_field_name,
                              Format_base* Format_ptr,
                              const char* Title)
   {
   // assign a default scatter format if caller didn't supply one.
   Format_base* New_format;
   if (Format_ptr == NULL)
      New_format = new Bar_format (Bar_format::Vertical_side_by_side);
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
                X_field_name,
                alpha,
                numeric,
                bottom,
                ::left,
                New_format,
                New_title.c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Frequency_chart::~Frequency_chart(void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//     Get the next x and y point to plot.  Returns true if this
//     is the last point for this plot.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
bool Frequency_chart::Get_next_point (Table_base& Table_obj,
                                      char* x_value,
                                      char* y_value)
   {
   // calculate which range we're looking for.
   int Start_range = Start_point + Current_point * Interval;
   int End_range = Start_range + Interval;

   // locate the position in the vector of numbers.
   vector<double>::iterator Start_iter = std::find_if
                                (Values[Current_xy_index].begin(),
                                 Values[Current_xy_index].end(),
                                 std::bind1st(std::less_equal<int>(), Start_range));
   vector<double>::iterator End_iter = std::find_if
                                  (Values[Current_xy_index].begin(),
                                   Values[Current_xy_index].end(),
                                   std::bind1st(std::less_equal<int>(), End_range));

   // calculate a y value.
   double y;
   switch (Frequency_type)
      {
      case Frequency_chart::count       : y = End_iter - Start_iter;
                                          break;
      case Frequency_chart::prob_exceed : y = 100.0 - Current_point * (100.0 / (Values[Current_xy_index].size()-1));
                                          break;
      case Frequency_chart::prob_cum    : y = Current_point * (100.0 / (Values[Current_xy_index].size()-1));
                                          break;
      };

   // output x and y values.

   ostrstream x_stream(x_value, 100);
   ostrstream y_stream(y_value, 100);
   x_stream << Start_range << " to <" << End_range << ends;
   y_stream << y << ends;

   if (Current_xy_index == Num_xys-1)
      {
      Current_point++;
      if (End_iter == Values[Current_xy_index].end())
         Table_obj.Last();
      }

   return true;
   }

// ------------------------------------------------------------------
//  Short description:
//    Format the specified plot given the xy pair structure.

//  Notes:

//  Changes:
//    DPH 21/4/1997

// ------------------------------------------------------------------
void Frequency_chart::Format_plot (Plot* Plot_ptr,
                                     XY_pair* XY_ptr,
                                     const char* Title_of_table)
   {
   High_level_chart_base::Format_plot (Plot_ptr, XY_ptr, Title_of_table);

   if (Frequency_type == Frequency_chart::count)
      Plot_ptr->Y_axis.Title.Text = "Frequency";

   else if (Frequency_type == Frequency_chart::prob_exceed)
      Plot_ptr->Y_axis.Title.Text = "Frequency (% exceed)";

   else
      Plot_ptr->Y_axis.Title.Text = "Cumulative frequency (%)";
   }

