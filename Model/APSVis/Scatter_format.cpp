#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "scatter_format.h"
#include "xy_series.h"

#define Num_markers  9
const XY_series::Marker_styles Marker_style_list[Num_markers] =
   {XY_series::Rectangle,     XY_series::Circle,     XY_series::Triangle,
    XY_series::Down_triangle, XY_series::Cross,      XY_series::Diag_cross,
    XY_series::Star,          XY_series::Diamond,    XY_series::Small_dot};

#define Num_line_styles  5
const Pen::Line_style_enum Line_style_list[Num_line_styles] =
   {Pen::Solid,
    Pen::Dash,
    Pen::Dot,
    Pen::DashDot,
    Pen::DashDotDot};

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Scatter_format::Scatter_format (void)
   {
   Scatter_type = Markers;
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
Scatter_format::Scatter_format (Scatter_type_enum Type)
   {
   Scatter_type = Type;
   }

// ------------------------------------------------------------------
//  Short description:
//    create a plot and return pointer to it.

//  Notes:

//  Changes:
//    DPH 18/4/1997
//    dph 23/11/99 removed a -1 on all Cycle lines

// ------------------------------------------------------------------
Series_base* Scatter_format::Create_series (int Num_plots_so_far)
   {
   Format_base::Create_series(Num_plots_so_far);

   // create a new series
   XY_series* XY_series_ptr = new XY_series;

   int Num_series_so_far = Num_plots_so_far - Num_plots_to_subtract;

   // set the xy_series pen colour
   TColor colour = Colour_list[Cycle(Num_series_so_far, Num_colours)];
   XY_series_ptr->Outline.Colour = colour;

   // format series.
   switch (Scatter_type)
      {
      case Markers : XY_series_ptr->Marker_style = Marker_style_list[Cycle(Num_series_so_far, Num_markers)] + 1;
                     XY_series_ptr->Lines_visible = false;
                     break;
      case Markers_lines : XY_series_ptr->Marker_style = Marker_style_list[Cycle(Num_series_so_far, Num_markers)] + 1;
                           XY_series_ptr->Lines_visible = true;
                           XY_series_ptr->Outline.Style = Pen::Solid;
                           break;
      case Lines : XY_series_ptr->Marker_style = XY_series::None;
                   XY_series_ptr->Lines_visible = true;
                   XY_series_ptr->Outline.Style = Pen::Solid;
                   break;
      }

   return XY_series_ptr;
   }


