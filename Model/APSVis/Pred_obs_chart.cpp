#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "pred_obs_chart.h"
#include "scatter_format.h"
#include <values.h>

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Pred_obs_chart::Pred_obs_chart (TForm* parent)
   : High_level_chart_base (parent)
   {
   Minimum_value = MAXDOUBLE;
   Maximum_value = 0;
   Display_1_1_line = true;
   Display_regr_line = true;
   SumX = 0;
   SumY = 0;
   SumXY = 0;
   SumX2 = 0;
   SumY2 = 0;
   Num_points = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//    add an xy pair to chart.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Pred_obs_chart::Add_xy (const char* Pred_field_name,
                             const char* Obs_field_name,
                             const char* Title)
   {
   // assign a default scatter format if caller didn't supply one.
   Format_base* New_format = new Scatter_format (Scatter_format::Markers);

   // assign a default title if caller didn't supply one.
   string New_title;
   if (Title == NULL)
      New_title = "Predicted vs Obseved";
   else
      New_title = Title;

   // send data to base class.
   Add_xy_pair (Pred_field_name,
                Obs_field_name,
                numeric,
                numeric,
                bottom,
                ::left,
                New_format,
                New_title.c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//    derive from get_next_xy_point so that we can keep track of
//    the minimum and maximum x and y values.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
bool Pred_obs_chart::Get_next_point (Table_base& Table_obj,
                                     char* x_value,
                                     char* y_value)
   {
   bool result = High_level_chart_base::Get_next_point (Table_obj,
                                                   x_value,
                                                   y_value);

   // work out the maximum data value.
   double x = atof(x_value);
   double y = atof(y_value);
   Maximum_value = max(Maximum_value, x);
   Maximum_value = max(Maximum_value, y);

   // work out the minimum data value.
   Minimum_value = min(Minimum_value, x);
   Minimum_value = min(Minimum_value, y);

   // need to calculate regression stuff now on the way through
   // all the data points.

   SumX = SumX + x;
   SumX2 = SumX2 + x * x;
   SumY = SumY + y;
   SumY2 = SumY2 + y * y;
   SumXY = SumXY + x * y;
   Num_points++;
   return result;
   }

// ------------------------------------------------------------------
//  Short description:
//    derive from do_final_formatting so that we can display
//    1:1 line and regression line if necessary.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Pred_obs_chart::Do_final_formatting (void)
   {
   // firstly we need to scale the axes.
   Chart_ptr->Get_plot(0)->X_axis.Minimum_data = Minimum_value;
   Chart_ptr->Get_plot(0)->Y_axis.Minimum_data = Minimum_value;
   Chart_ptr->Get_plot(0)->X_axis.Maximum_data = Maximum_value;
   Chart_ptr->Get_plot(0)->Y_axis.Maximum_data = Maximum_value;

   if (Display_1_1_line)
      {
      // go create new plot.
      Plot* Plot_1_1 = new Plot;

      // go create a format object for our line.
      Scatter_format Format_1_1(Scatter_format::Lines);

      // create new series and give it to our plot.
      Plot_1_1->Data_series = Format_1_1.Create_series (0);

      // format our series.
      Plot_1_1->Data_series->Title = "1:1 line";

      // give new plot to chart.
      Chart_ptr->Add_plot (*Plot_1_1);

      // give some data to series.
      char max_st[20], min_st[20];
      sprintf(max_st, "%10.3f", Maximum_value);
      sprintf(min_st, "%10.3f", Minimum_value);
      Plot_1_1->Data_series->Add_data (min_st, min_st);
      Plot_1_1->Data_series->Add_data (max_st, max_st);
      }
   if (Display_regr_line)
      {
      double Xbar = SumX / Num_points;
      double Ybar = SumY / Num_points;
      double CSSXY = SumXY - SumX * SumY / Num_points;     // Corrected SS for products
      double CSSX = SumX2 - SumX * SumX / Num_points;      // Corrected SS for X
      double m = CSSXY / CSSX;                             // Calculate slope
      double c = Ybar - m * Xbar;                          // Calculate intercept

      double x1 = Minimum_value;
      double y1 = m * x1 + c;
      if (y1 < Minimum_value)
         {
         y1 = Minimum_value;
         x1 = (y1 - c) / m;
         }

      double x2 = Maximum_value;
      double y2 = m * x2 + c;
      if (y2 > Maximum_value)
         {
         y2 = Maximum_value;
         x2 = (y2 - c) / m;
         }
      // go create new plot.
      Plot* Plot_regr = new Plot;

      // go create a format object for our line.
      Scatter_format Format_regr(Scatter_format::Lines);

      // create new series and give it to our plot.
      Plot_regr->Data_series = Format_regr.Create_series (1);

      // format our series.
      Plot_regr->Data_series->Title = "Regression line";

      // give new plot to chart.
      Chart_ptr->Add_plot (*Plot_regr);

      // give some data to series.
      char x1_st[20], y1_st[20], x2_st[20],y2_st[20];
      sprintf(x1_st, "%10.3f", x1);
      sprintf(y1_st, "%10.3f", y1);
      sprintf(x2_st, "%10.3f", x2);
      sprintf(y2_st, "%10.3f", y2);
      Plot_regr->Data_series->Add_data (x1_st, y1_st);
      Plot_regr->Data_series->Add_data (x2_st, y2_st);
      }
   }

