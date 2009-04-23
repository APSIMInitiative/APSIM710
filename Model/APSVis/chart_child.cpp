//---------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Chart_child.h"
#include <vcl\printers.hpp>

#include "high_level_chart_base.h"
#include "scatter_format.h"
#include "apsim_table.h"
#include "apsim_binary_table.h"
#include <general\path.h>
// ------------------------------------------------------------------
#pragma resource "*.dfm"
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 25/7/97
//    dph 11/3/98 Added code to read in size of window C-078

// ------------------------------------------------------------------
__fastcall TChart_child::TChart_child(TComponent *Owner)
	: TForm(Owner)
   {
   }
// ------------------------------------------------------------------
//  Short description:
//      Form is about to close.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void __fastcall TChart_child::FormClose(TObject *Sender, TCloseAction &Action)
   {
	Action = caFree;
   }
// ------------------------------------------------------------------
//  Short description:
//      set the predicted files to use in creating a chart.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void TChart_child::Set_predicted_files (list<string>& File_names)
   {
   Predicted_database.Clear();
   for (list<string>::iterator Iter = File_names.begin();
                               Iter != File_names.end();
                               Iter++)
      {
      Predicted_database.Add_table(Create_table((*Iter).c_str()));
      }
   Setup_properties (Predicted_database);
   }

// ------------------------------------------------------------------
//  Short description:
//      set the observed files to use in creating a chart.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void TChart_child::Set_observed_files (list<string>& File_names)
   {
   Observed_database.Clear();
   for (list<string>::iterator Iter = File_names.begin();
                               Iter != File_names.end();
                               Iter++)
      {
      Observed_database.Add_table(Create_table((*Iter).c_str()));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      let user edit the properties of the chart.

//  Notes:

//  Changes:
//    DPH 25/7/97
//    dph 1/10/97 added support for binary files.

// ------------------------------------------------------------------
Table_base* TChart_child::Create_table (const char* File_name)
   {
   Path p;
   p.Set_path (File_name);
   if (p.Get_name()[0] == '#')
      return new APSIM_binary_table ( File_name);
   else
      return new APSIM_table (File_name);
   }

// ------------------------------------------------------------------
//  Short description:
//      let user edit the properties of the chart.  Return true
//      if ok was pressed.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
bool TChart_child::Edit_properties (void)
   {
   bool ok = Show_properties();
   if (ok)
      Create_chart();

   return ok;
   }
// ------------------------------------------------------------------
//  Short description:
//      let the user edit the chart.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void TChart_child::Edit_chart (void)
   {
   Chart_ptr->User_edit();
   }
// ------------------------------------------------------------------
//  Short description:
//      print the chart

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void TChart_child::Print (void)
   {
   My_screen.Print_preview();
   }
// ------------------------------------------------------------------
//  Short description:
//      copy the chart to the clipboard.

//  Notes:

//  Changes:
//    DPH 25/7/97
//    dph 27/5/99 added call to FormResize to recalc chart dimensions D257

// ------------------------------------------------------------------
void TChart_child::Copy_to_clipboard (void)
   {
   My_screen.Copy_to_clipboard();
   }
// ------------------------------------------------------------------
//  Short description:
//      Return all field names in specified database to caller.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void TChart_child::Get_field_names (Database& DB,
                                    TStringList* Return_list)
   {
   vector <string>* Field_names;

   for (int Table_number = 0;
        Table_number < DB.Count_tables();
        Table_number++)
      {
      Table_base* Table_ptr = DB.Get_table(Table_number);
      Table_ptr->Open ();
      Field_names = Table_ptr->Get_field_names();

      // loop through all field names.
      for (unsigned int Field_index = 0;
           Field_index < Field_names->size();
           Field_index++)
         {
         if (Return_list->IndexOf((*Field_names)[Field_index].c_str()) < 0)
            Return_list->Add ((*Field_names)[Field_index].c_str());
         }
      Table_ptr->Close ();
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      create a chart on the form.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void TChart_child::Create_chart (void)
   {
   // put hourglass up on screen.
   Screen->Cursor = crHourGlass;

   // clear any existing charts from screen.
   My_screen.Clear();

   // create our chart.
   Chart_ptr = Create_chart_object ();
   My_screen.Add_chart (*Chart_ptr);

   Chart_ptr->Allow_zoom();

   // setup chart for predicted styles
   Setup_chart_object (Chart_ptr, true);

   // create all plots for chart.
   if (Chart_ptr->Something_to_draw())
      {
      // Give filter object to all tables

      for (int Table_number = 0;
               Table_number < Predicted_database.Count_tables();
               Table_number++)
         {
         Table_base* Table_ptr = Predicted_database.Get_table(Table_number);
         Setup_filter(Table_ptr->Filter);
         Chart_ptr->Create_plots_from_table (*Table_ptr);
         }
      }

   // clear all predicted
   Chart_ptr->Clear_xys ();

   // setup chart for observed styles
   Setup_chart_object (Chart_ptr, false);

   // create all plots for chart.
   if (Chart_ptr->Something_to_draw())
      {
      // Give filter object to all tables

      for (int Table_number = 0;
               Table_number < Observed_database.Count_tables();
               Table_number++)
         {
         Table_base* Table_ptr = Observed_database.Get_table(Table_number);
         Setup_filter(Table_ptr->Filter);
         Chart_ptr->Create_plots_from_table (*Table_ptr);
         }
      }

   // design the screen.
   My_screen.Design ();

   // put hourglass up on screen.
   Screen->Cursor = crArrow;
   }
// ------------------------------------------------------------------
//  Short description:
//      set the screen size when the user moves the MDI form.

//  Notes:

//  Changes:
//    DPH 25/7/97
//    dph 11/3/98 added code to write the MDI form size to .ini file C-078

// ------------------------------------------------------------------
void __fastcall TChart_child::FormResize(TObject *Sender)
   {
   // need to resize the component.
   RECT r;
   r.left = 0;
   r.top = 0;
   r.right = ClientWidth;
   r.bottom = ClientHeight;
   My_screen.Set_screen_size (r);
   }
//---------------------------------------------------------------------------

