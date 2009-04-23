#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "bar_format.h"
#include "bar_series.h"

#define Num_brush_styles  7
const Brush::Brush_style_enum Brush_style_list[Num_brush_styles] =
   { Brush::Solid,
     Brush::Horizontal,
     Brush::Vertical,
     Brush::FDiagonal,
     Brush::BDiagonal,
     Brush::Cross,
     Brush::DiagCross };

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Bar_format::Bar_format (void)
   {
   Bar_type = Vertical_side_by_side;
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Bar_format::Bar_format (Bar_type_enum Type)
   {
   Bar_type = Type;
   }

// ------------------------------------------------------------------
//  Short description:
//    create a plot and return pointer to it.

//  Notes:

//  Changes:
//    DPH 18/4/1997
//    dph 23/11/99 removed all -1 on cycle lines.

// ------------------------------------------------------------------
Series_base* Bar_format::Create_series (int Num_plots_so_far)
   {
   Format_base::Create_series(Num_plots_so_far);

   // create a new series
   Bar_series* Bar_series_ptr = new Bar_series;

   int Num_series_so_far = Num_plots_so_far - Num_plots_to_subtract;

   // set the orientation.
   if (Bar_type == Vertical_side_by_side ||
       Bar_type == Vertical_stacked ||
       Bar_type == Vertical_stacked100)
      Bar_series_ptr->Set_orientation(Bar_series::Vertical);
   else
      Bar_series_ptr->Set_orientation(Bar_series::Horizontal);

   // set the multibar property.
   switch (Bar_type)
      {
      case Vertical_side_by_side :
      case Horizontal_side_by_side : Bar_series_ptr->Multi_bar = Bar_series::Side_by_side;
                                     break;

      case Vertical_stacked :
      case Horizontal_stacked : Bar_series_ptr->Multi_bar = Bar_series::Stacked;
                                break;
      case Vertical_stacked100 :
      case Horizontal_stacked100 : Bar_series_ptr->Multi_bar = Bar_series::Stacked100;
                                   break;
      }

   // set the series brush
   TColor colour = Colour_list[Cycle(Num_series_so_far, Num_colours)];
   Bar_series_ptr->Fill.Colour = colour;
   Bar_series_ptr->Fill.Style = Brush::Solid;

   return Bar_series_ptr;
   }


