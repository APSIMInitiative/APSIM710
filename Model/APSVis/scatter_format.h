#ifndef SCATTER_FORMAT_H
#define SCATTER_FORMAT_H

#include "format_base.h"
// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a scatter formatting class.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
class CHART_EXPORT Scatter_format : public Format_base
   {
   public:
      enum Scatter_type_enum {Lines, Markers_lines, Markers};

      Scatter_format(void);
      Scatter_format(Scatter_type_enum Scatter_type);
      virtual Series_base* Create_series (int Num_plots_so_far);

      Scatter_type_enum Scatter_type;
   private:

   };

#endif
