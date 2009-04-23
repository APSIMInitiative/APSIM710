#ifndef BAR_FORMAT_H
#define BAR_FORMAT_H

#include "format_base.h"
// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a bar formatting class.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
class CHART_EXPORT Bar_format : public Format_base
   {
   public:
      enum Bar_type_enum {Vertical_side_by_side, Vertical_stacked, Vertical_stacked100,
                          Horizontal_side_by_side, Horizontal_stacked, Horizontal_stacked100};

      Bar_format(void);
      Bar_format(Bar_type_enum Bar_type);
      virtual Series_base* Create_series (int Num_plots_so_far);

      Bar_type_enum Bar_type;
   private:

   };

#endif
