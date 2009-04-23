#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Prob_chart.h"
#include "scatter_format.h"
#include <strstrea.h>
#include <algorithm>
// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Probability_chart::Probability_chart(TForm* parent)
   : High_level_chart_base (parent)
   {
   Do_prob_exceedance = true;
   Values = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Probability_chart::~Probability_chart(void)
   {
   delete [] Values;
   }

// ------------------------------------------------------------------
//  Short description:
//    add an xy pair to chart.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Probability_chart::Add_xy (const char* X_field_name,
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
                X_field_name,
                numeric,
                numeric,
                bottom,
                ::left,
                New_format,
                New_title.c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//    loop through all xy pairs and initialise each one.

//  Notes:

//  Changes:
//    DPH 21/4/1997
//    dph 10/3/98  moved table_obj.Next() outside of for loop - d122

// ------------------------------------------------------------------
void Probability_chart::Initialise_xys (Table_base& Table_obj)
   {
   High_level_chart_base::Initialise_xys(Table_obj);

   // create an array of vectors to hold all series for this table.
   delete [] Values;
   Values = new vector<double> [Num_xys];

   // loop through each record in the table and store all numbers in
   // all the columns we're interested in.
   char value[100];
   while (!Table_obj.At_end_of_data())
      {
      for (int XY_number = 0; XY_number < Num_xys; XY_number++)
         {
         Table_obj.Get_data_by_index (XYs[XY_number]->x_index, value);
         Values[XY_number].push_back (atof(value));
         }
      Table_obj.Next();
      }
   Table_obj.First();

   // need to sort each vector of numbers into ascending order.
   for (int XY_number = 0; XY_number < Num_xys; XY_number++)
      {
      std::sort (Values[XY_number].begin(), Values[XY_number].end());
      if (!Do_prob_exceedance)
         std::reverse (Values[XY_number].begin(), Values[XY_number].end());
      }
      
   Current_point = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//     Get the next x and y point to plot.  Returns true if this
//     is the last point for this plot.

//  Notes:

//  Changes:
//    DPH 18/4/1997
//    dph 11/3/98 Made the calculation of Prob_value a "double" calculation
//                instead of an "int" calculation D-128

// ------------------------------------------------------------------
bool Probability_chart::Get_next_point (Table_base& Table_obj,
                                        char* x_value,
                                        char* y_value)
   {
   ostrstream x_stream(x_value, 100);
   ostrstream y_stream(y_value, 100);
   x_stream << Values[Current_xy_index][Current_point] << ends;
   double Prob_value = 100.0 - Current_point * (100 / (Values[Current_xy_index].size()-1.0));
   y_stream << Prob_value << ends;

   if (Current_xy_index == Num_xys-1)
      Current_point++;

   return true;
   }

// ------------------------------------------------------------------
//  Short description:
//    Format the specified plot given the xy pair structure.

//  Notes:

//  Changes:
//    DPH 21/4/1997

// ------------------------------------------------------------------
void Probability_chart::Format_plot (Plot* Plot_ptr,
                                     XY_pair* XY_ptr,
                                     const char* Title_of_table)
   {
   High_level_chart_base::Format_plot (Plot_ptr, XY_ptr, Title_of_table);

   if (Do_prob_exceedance)
      Plot_ptr->Y_axis.Title.Text = "Probability of exceedance (%)";
   else
      Plot_ptr->Y_axis.Title.Text = "Cumulative probability (%)";
   }

