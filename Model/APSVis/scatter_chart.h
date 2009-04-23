#ifndef SCATTER_CHART_H
#define SCATTER_CHART_H

#include "high_level_chart_base.h"

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates an XY scatter chart

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
class CHART_EXPORT Scatter_chart : public High_level_chart_base
   {
   public:
      Scatter_chart(TForm* parent);

      void Add_xy (const char* X_field_name,
                   const char* Y_field_name,
                   Field_type_enum X_type = numeric,
                   X_axis_link_enum X_axis_link = bottom,
                   Y_axis_link_enum Y_axis_link = ::left,
                   Format_base* Format_ptr = NULL,
                   const char* Title = NULL,
                   bool Accumulate_x = false,
                   bool Accumulate_y = false);

   protected:
   };

#endif
