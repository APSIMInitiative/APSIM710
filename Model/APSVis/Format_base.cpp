#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "format_base.h"

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Format_base::Format_base (void)
   {
   Colour_list[0] = clBlue;
   Colour_list[1] = clRed;
   Colour_list[2] = (Graphics::TColor) RGB (255, 0, 255);  // magenta
   Colour_list[3] = clGreen;
   Colour_list[4] = clDkGray;
   Colour_list[5] = clLtGray;
   Colour_list[6] = clYellow;
   Colour_list[7] = clLime;
   Colour_list[8] = clLtGray;
   Colour_list[9] = clMaroon;
   Colour_list[10] = clNavy;
   Colour_list[11] = clOlive;
   Colour_list[12] = clPurple;
   Colour_list[13] = clRed;
   Colour_list[14] = clSilver;
   Colour_list[15] = clTeal;
   Colour_list[16] = clYellow;
   Num_plots_to_subtract = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//    create a plot and return pointer to it.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Series_base* Format_base::Create_series (int /*Num_plots_so_far*/)
   {
   return NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//    cycle through a number series from 0 to Max_number - 1.
//    returns the next number in the sequence.
//    eg.  if current_number = 1   and max_number = 4 returns 1
//         if current_number = 2   and max_number = 4 returns 2
//         if current_number = 3   and max_number = 4 returns 3
//         if current_number = 4   and max_number = 4 returns 4
//         if current_number = 5   and max_number = 4 returns 1

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
int Format_base::Cycle (int Current_number, int Max_number)
   {
   // current_number = 0, max_number = 4, m = 4
   // current_number = 1, max_number = 4, m = 4
   // current_number = 2, max_number = 4, m = 4
   // current_number = 3, max_number = 4, m = 4
   // current_number = 4, max_number = 4, m = 8
   int m = (Current_number / Max_number + 1) * Max_number;

   return Max_number - (m - Current_number);
   }

