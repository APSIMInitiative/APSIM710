#ifndef APSIM_DEPTH_CHART_H
#define APSIM_DEPTH_CHART_H

#include "depth_chart.h"

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a depth chart

//  Notes:
//    this class creates a depth chart from an APSIM output file.
//    when calling addxy make sure that dlayer or any variable
//    does NOT have an array specifier eg (1).
//
//    Should always set the splitting field to date

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
class CHART_EXPORT APSIM_depth_chart : public Depth_chart
   {
   public:
      APSIM_depth_chart (TForm* parent);

   protected:
      virtual bool Get_next_point (Table_base& Table_obj,
                                   char* x_value,
                                   char* y_value);
      virtual void Initialise_xys (Table_base& Table_obj);
      
   private:
      int Current_layer_number;
      int Num_layers;
   };

#endif
