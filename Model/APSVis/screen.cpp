#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "drawable.h"
#include "Screen.h"
#include <math.h>  // sqrt
#include <general\stl_functions.h>

/**# implementation Chart_screen:: id(C_0853909854)
*/
// ------------------------------------------------------------------
//  Short description:
//    constructor.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
Chart_screen::Chart_screen (void)
   {

   }

// ------------------------------------------------------------------
//  Short description:
//    destructor.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
Chart_screen::~Chart_screen (void)
   {
   Clear();
   }

// ------------------------------------------------------------------
//  Short description:
//    clear all charts from screen.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_screen::Clear (void)
   {
   // delete all charts.
   while (!Chart_list.empty())
      {
		Chart_base* Ptr = Chart_list.back();
      Chart_list.pop_back();
		delete Ptr;
		}
   }

// ------------------------------------------------------------------
//  Short description:
//    add a chart to the chart list

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_screen::Add_chart (Chart_base& Chart_obj)
   {
   Chart_list.push_back (&Chart_obj);
   }

// ------------------------------------------------------------------
//  Short description:
//    get number of charts in screen.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
int Chart_screen::Get_num_charts (void)
   {
   return Chart_list.size();
   }

// ------------------------------------------------------------------
//  Short description:
//    return a chart to caller.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
Chart_base* Chart_screen::Get_chart (int Chart_number)
   {
   return Chart_list[Chart_number];
   }

// ------------------------------------------------------------------
//  Short description:
//    go communicate with TChart.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_screen::Design (void)
   {
   if (Visible)
      {
      for (vector<Chart_base*> ::iterator Iter = Chart_list.begin();
                                          Iter != Chart_list.end();
                                          Iter++)
         (*Iter)->Design();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    draw screen on device context in specified rectangle.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
/*void Chart_screen::Draw (HDC dc, RECT r)
   {
   if (Visible && Chart_list.size() > 0)
      {
      // calculate all chart positions given the absolute measurements of the
      // device context.
      Calc_chart_positions (r);

      // loop through all charts and draw.
      for (vector<Chart_base*> ::iterator Iter = Chart_list.begin();
                                          Iter != Chart_list.end();
                                          Iter++)
         (*Iter)->Draw(dc);
      }
   }
*/
// ------------------------------------------------------------------
//  Short description:
//    calculate all chart positions given the absolute measurements of the
//    device context.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_screen::Calc_chart_positions (RECT r)
   {
   if (Chart_list.size() > 0)
      {
      // calculate how many rows and columns of charts we need on the screen
      // to fit all the charts.
      int Num_chart_cols = sqrt (Chart_list.size() - 1.0) + 1.0;
      int Num_chart_rows = Chart_list.size() * 1.0 / Num_chart_cols + 0.5;

      // calculate the width of a column and height of a row.
      int Col_width = (r.right - r.left) / Num_chart_cols;
      int Row_height = (r.bottom - r.top) / Num_chart_rows;

      // loop through all charts and position.
      vector<Chart_base*>::iterator Iter = Chart_list.begin();
      for (unsigned int Chart_number = 0;
           Chart_number < Chart_list.size();
           Chart_number++)
         {
         int Row_number = Chart_number / Num_chart_cols;
         int Col_number = Chart_number - (Row_number * Num_chart_cols);

         (*Iter)->Position.left = Col_number * Col_width;
         (*Iter)->Position.right = (Col_number + 1) * Col_width;
         (*Iter)->Position.top = Row_number * Row_height;
         (*Iter)->Position.bottom = (Row_number + 1) * Row_height;
         (*Iter)->Update_chart_position ();

         Iter++;
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    send the current screen to the printer.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_screen::Print(RECT Print_rect)
   {
   if (Visible && Chart_list.size() > 0)
      {
      // calculate all chart positions given the absolute measurements of the
      // device context.
      Calc_chart_positions (Print_rect);

      // loop through all charts and draw.
      for (vector<Chart_base*> ::iterator Iter = Chart_list.begin();
                                          Iter != Chart_list.end();
                                          Iter++)
         (*Iter)->Print();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    send the current screen to the printer.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_screen::Print_preview(void)
   {
   if (Visible && Chart_list.size() > 0)
      {
      // at the moment we can only do a preview for a single chart.
      // show user first chart.

      Chart_list[0]->Print_preview();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    send the current screen to the clipboard.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_screen::Copy_to_clipboard (void)
   {
   if (Visible && Chart_list.size() > 0)
      {
      // calculate all chart positions given the absolute measurements of the
      // device context.
      RECT r = {0, 0, 2000, 2000};
      Calc_chart_positions (r);

      // loop through all charts and draw.
      for (vector<Chart_base*> ::iterator Iter = Chart_list.begin();
                                          Iter != Chart_list.end();
                                          Iter++)
         (*Iter)->Copy_to_clipboard();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    set the size of the screen.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_screen::Set_screen_size (RECT r)
   {
   Calc_chart_positions (r);
   }


