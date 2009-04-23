#ifndef DEPTH_CHART_H
#define DEPTH_CHART_H

#include "high_level_chart_base.h"

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a depth chart

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
class CHART_EXPORT Depth_chart : public High_level_chart_base
   {
   public:
      Depth_chart(TForm* parent);

      void Add_xy (const char* X_field_name,
                   const char* Y_field_name,
                   Format_base* Format_ptr = NULL,
                   const char* Title = NULL);

      bool Accumulate_y;

   protected:
      double Accumulated_value;

      virtual bool Need_to_create_new_plots (Table_base& Table_obj);
      virtual void Format_plot (Plot* Plot_ptr,
                                XY_pair* XY_ptr,
                                const char* Title_of_table);

      virtual bool Get_next_point (Table_base& Table_obj,
                                   char* x_value,
                                   char* y_value);
   };

#endif
