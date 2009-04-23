#ifndef XY_SERIES_H
#define XY_SERIES_H

#include "series_base.h"

// ------------------------------------------------------------------
//  Short description:
//    Class encapsulating a xy scatter series on a plot.

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT XY_series : public Series_base
   {
   public:
      XY_series();
	   ~XY_series();
	   virtual void Design (TChart* TChart_ptr);

      enum Marker_styles {None, Rectangle, Circle, Triangle, Down_triangle,
                          Cross, Diag_cross, Star, Diamond, Small_dot};


      bool Lines_visible;
      Marker_styles Marker_style;

   protected:
      TLineSeries* Line_series_ptr;
      TChartSeries* Create_chart_series(TChart* TChart_ptr);
   };
#endif
