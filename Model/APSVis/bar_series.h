#ifndef BAR_SERIES_H
#define BAR_SERIES_H

#include "series_base.h"

// ------------------------------------------------------------------
//  Short description:
//    Class encapsulating a bar series on a plot.

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT Bar_series : public Series_base
   {
   public:
      Bar_series();
	   ~Bar_series();
	   virtual void Design (TChart* TChart_ptr);

      enum Orientation_enum  {Horizontal, Vertical};
      enum Multi_bar_enum    {Side_by_side, Stacked, Stacked100};

      void Set_orientation (Orientation_enum Orientation);
      Orientation_enum Get_orientation (void);

      Multi_bar_enum Multi_bar;

   protected:
      Orientation_enum Orientation;
      TCustomBarSeries* Custom_bar_series_ptr;
      TChartSeries* Create_chart_series(TChart* TChart_ptr);
      TChart* TChart_ptr;
   };
#endif
