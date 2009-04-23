#ifndef BRUSH_H
#define BRUSH_H

#include "colour.h"
// ------------------------------------------------------------------
//  Short description:
//    encapsulates a brush.

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT Brush
   {
   public:
      enum Brush_style_enum {Solid, Clear, Horizontal, Vertical, FDiagonal, BDiagonal, Cross, DiagCross };

      Brush();
	   virtual ~Brush();
   	Colour_struct Colour;
   	Brush_style_enum Style;

      void Set_TChartBrush (TChartBrush* Brush);
   private:
      TBrush *brush;

   };
#endif
