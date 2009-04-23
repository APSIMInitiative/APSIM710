#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "drawable.h"

/**# implementation Brush:: id(C_0853987306)
*/
// ------------------------------------------------------------------
//  Short description:
//    constructor.

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
Brush::Brush (void)
   {
   Colour.Red = 255;
   Colour.Green = 255;
   Colour.Blue = 255;
   Style = Solid;
   brush = new TBrush;
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor.

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
Brush::~Brush (void)
   {
//   delete brush;
   }

// ------------------------------------------------------------------
//  Short description:
//    return a tbrush to caller.

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
void Brush::Set_TChartBrush (TChartBrush* Brush)
   {
   Brush->Color = COLORREF(Colour);
   Brush->Style = TBrushStyle(Style);
   }

