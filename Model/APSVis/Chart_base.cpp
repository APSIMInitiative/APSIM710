#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "drawable.h"

#include "Chart_base.h"
#include <general\stl_functions.h>
#include <teeprevi.hpp>

/**# implementation Chart:: id(C_0853910634)
*/

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
Chart_base::Chart_base (TForm* parent)
   {
   TChart_ptr = new TChart(parent);
   TChart_ptr->Parent = parent;
   Parent_form = parent;
   Is_3D = false;
   Allow_zoom = false;

   // make the chart the same size as the parent form.
   Position.left = 0;
   Position.top = 0;
   Position.right = parent->ClientWidth;
   Position.bottom = parent->ClientHeight;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
Chart_base::~Chart_base (void)
   {
   // delete all plots.
   Destroy_pointers (Plot_list, &Plot());

   // delete all annotations.
   Destroy_pointers (Annotation_list, &Drawable());
   }

// ------------------------------------------------------------------
//  Short description:
//    add a plot into the plot list.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_base::Add_plot (Plot& Plot_obj)
   {
   Plot_list.push_back (&Plot_obj);

   // setup series chart ptr
   Plot_obj.Data_series->TChart_ptr = TChart_ptr;
   }

// ------------------------------------------------------------------
//  Short description:
//    draw the chart on the specified device context in the specified
//    rectangle (pixels).

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
/*void Chart_base::Draw (HDC dc)
   {
   if (Visible)
      {
      TCanvas* My_canvas = new TCanvas;
      My_canvas->Handle = dc;
      TChart_ptr->Draw (My_canvas, TRect(Position));
      delete My_canvas;
      }
   }
*/
// ------------------------------------------------------------------
//  Short description:
//    send the chart to the printer.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_base::Print (void)
   {
   if (Visible)
      {
      TChart_ptr->PrintResolution = -100;
      TChart_ptr->Print();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    display a print preview dialog to user.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_base::Print_preview (void)
   {
//   if (Visible)
//      ChartPreview(Parent_form, TChart_ptr);
   }

// ------------------------------------------------------------------
//  Short description:
//    send the chart to the clipboard.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_base::Copy_to_clipboard (void)
   {
   if (Visible)
      {
      TChart_ptr->CopyToClipboardBitmap();
      TChart_ptr->CopyToClipboardMetafile(true);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Communicate with the TChart object all the chart settings.

//  Notes:

//  Changes:
//    DPH 4/2/1997
//    dph 20/1/98 changed default position of legend to top.

// ------------------------------------------------------------------
void Chart_base::Design(void)
   {
   // set up chart position.
   Update_chart_position();

   // set up miscellaneous chart settings.
   TChart_ptr->Color = TColor(RGB(Fill.Colour.Red, Fill.Colour.Green, Fill.Colour.Blue));
   TChart_ptr->Title->Text->Add(Title.Text.c_str());
   TChart_ptr->View3D = Is_3D;
   TChart_ptr->Legend->LegendStyle = lsSeries;
   TChart_ptr->Legend->Alignment = laTop;

   // create an x axis and a y axis title
   string bottom_axis_title;
   string left_axis_title;
   string top_axis_title;
   string right_axis_title;

   // loop through all plots.
   for (vector<Plot*>::iterator Iter = Plot_list.begin();
                                Iter != Plot_list.end();
                                Iter++)
      {
      // tell plot to design itself.
      (*Iter)->Design (TChart_ptr);

      // append the axis title to our axis title.
      string x_axis_label, y_axis_label;
      x_axis_label = (*Iter)->X_axis.Title.Text;
      y_axis_label = (*Iter)->Y_axis.Title.Text;
      switch ((*Iter)->X_axis.Axis_type)
         {
         case Axis::Bottom : if (bottom_axis_title.find (x_axis_label) == string::npos)
                                {
                                if (bottom_axis_title.length() > 0)
                                   bottom_axis_title += ", ";
                                bottom_axis_title += x_axis_label;
                                }
                             break;
         case Axis::Top    : if (top_axis_title.find (x_axis_label) == string::npos)
                                {
                                if (top_axis_title.length() > 0)
                                   top_axis_title += ", ";
                                top_axis_title += x_axis_label;
                                }
                             break;
         };
      switch ((*Iter)->Y_axis.Axis_type)
         {
         case Axis::Left   : if (left_axis_title.find (y_axis_label) == string::npos)
                                {
                                if (left_axis_title.length() > 0)
                                   left_axis_title += ", ";
                                left_axis_title += y_axis_label;
                                }
                             break;
         case Axis::Right  : if (right_axis_title.find (y_axis_label) == string::npos)
                                {
                                if (right_axis_title.length() > 0)
                                   right_axis_title += ", ";
                                right_axis_title += y_axis_label;
                                }
                             break;
         }
      }

   // send the axis title to the chart.
   TChart_ptr->BottomAxis->Title->Caption = bottom_axis_title.c_str();
   TChart_ptr->TopAxis->Title->Caption = top_axis_title.c_str();
   TChart_ptr->LeftAxis->Title->Caption = left_axis_title.c_str();
   TChart_ptr->RightAxis->Title->Caption = right_axis_title.c_str();

   // set the zooming info.
   TChart_ptr->AllowZoom = Allow_zoom;
   TChart_ptr->AllowPanning = pmBoth;
   }

// ------------------------------------------------------------------
//  Short description:
//    Set the tchart position and size.

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
void Chart_base::Update_chart_position(void)
   {
   // set up chart position.
   TChart_ptr->Left = Position.left;
   TChart_ptr->Top = Position.top;
   TChart_ptr->Width = Position.right - Position.left;
   TChart_ptr->Height = Position.bottom - Position.top;
   TChart_ptr->ReCalcWidthHeight();
   }

// ------------------------------------------------------------------
//  Short description:
//    allow user to edit the chart.

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
void Chart_base::User_edit (void)
   {
//   TForm* Dummy_form = new TForm((TComponent*) NULL);
   EditChart (Parent_form, TChart_ptr);
//   delete Dummy_form;
   }

// ------------------------------------------------------------------
//  Short description:
//    return number of plots to caller.

//  Notes:

//  Changes:
//    DPH 18/6/1997

// ------------------------------------------------------------------
int Chart_base::Get_num_plots (void)
   {
   return Plot_list.size();
   }

// ------------------------------------------------------------------
//  Short description:
//    return number of plots to caller.

//  Notes:

//  Changes:
//    DPH 18/6/1997

// ------------------------------------------------------------------
Plot* Chart_base::Get_plot (int Plot_number)
   {
   return Plot_list[Plot_number];
   }
// ------------------------------------------------------------------
void dummy(void)
   {
   vector<Plot*> vp;
   }

