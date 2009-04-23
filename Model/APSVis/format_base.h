#ifndef FORMAT_BASE_H
#define FORMAT_BASE_H

#include "drawable.h"
#include "series_base.h"

// ------------------------------------------------------------------
//  Short description:
//    this is the base class for all plot formatting classes.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
class CHART_EXPORT Format_base
   {
   public:
      Format_base(void);

      virtual Series_base* Create_series (int Num_plots_so_far);
      int Num_plots_to_subtract;

   protected:

      #define Num_colours  17
      TColor Colour_list[Num_colours];

      int Cycle (int Current_number, int Max_number);
   };

#endif
