#ifndef PLOT_H
#define PLOT_H

#include "series_base.h"
#include "axis.h"
// ------------------------------------------------------------------
//  Short description:
/**# :[Description = "Encapsulates an x and y axis and a series"] */
//    The data series passed in is NOT EVER deleted.

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT Plot : public Drawable
   {
   public:
      Plot ();
	   virtual ~Plot();
      int operator==(const Plot& Obj)
         {return (Data_series == Obj.Data_series);}
      int operator<(const Plot& Obj)
         {return false;}
   	void Design (TChart* TChart_ptr);

   	Axis X_axis;
	   Axis Y_axis;
   	Series_base* Data_series;
   };

#endif
