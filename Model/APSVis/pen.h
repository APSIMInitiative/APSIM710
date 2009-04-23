#ifndef PEN_H
#define PEN_H

#include "colour.h"

// ------------------------------------------------------------------
//  Short description:
/**# :[Description = "Encapsulates all pen operations."] */

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT Pen
   {
   public:
      enum Line_style_enum { Solid, Dash, Dot, DashDot, DashDotDot, Clear, InsideFrame};

      Pen();
   	virtual ~Pen();

	   Colour_struct Colour;
   	Line_style_enum Style;

      void Set_TChartpen (TChartPen* Pen);
   private:
      TPen *pen;
   };

#endif