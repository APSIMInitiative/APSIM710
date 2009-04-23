#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "drawable.h"

/**# implementation Pen:: id(C_0853987254)
*/

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------

Pen::Pen (void)
   {
   Colour.Red = 0;
   Colour.Green = 0;
   Colour.Blue = 0;            // black.
   Style = Solid;
   pen = new TPen;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------

Pen::~Pen (void)
   {
//   delete pen;
   }

// ------------------------------------------------------------------
//  Short description:
//    return a tchartpen to caller.

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------

void Pen::Set_TChartpen (TChartPen* Pen)
   {
   Pen->Color = COLORREF(Colour);
   Pen->Style = TPenStyle(Style);
   }

