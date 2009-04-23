//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl\vcl.h>
#pragma hdrstop

#include "depth_child.h"
#include "scatter_format.h"
#include "no_date.h"
#include "table_stream.h"

// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
__fastcall Depth_child::Depth_child(TComponent *Owner)
	: TChart_child(Owner)
   {
   Property_form = new TDepth_prop_form (this);
   }

// ------------------------------------------------------------------
//  Short description:
//      Setup the properties form.  This is called whenever the database
//      that is passed into this method is changed.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void Depth_child::Setup_properties (Database& DB)
   {
   // get all field names.
   TStringList* Field_list = new TStringList;
   Get_field_names (DB, Field_list);

   // get pointer to a table_stream.
   Table_stream* Table_ptr = dynamic_cast<Table_stream*>(DB.Get_table(0));

   // setup property form
   Property_form->Setup (Field_list, Table_ptr);
   delete Field_list;
   }

// ------------------------------------------------------------------
//  Short description:
//      Return the start and end date in the specified database.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void Depth_child::Get_start_end_date (Table_base& Table_obj,
                                      GDate& Start_date,
                                      GDate& End_date)
   {
   char St1[100];

   // open the table.
   Table_obj.Open();

   // Get start date.
   Table_obj.Get_data_by_name ("Date", St1);
   Start_date.Read(St1);

   // Get end date
   Table_obj.Last();
   Table_obj.Get_data_by_name ("Date", St1);
   End_date.Read(St1);

   // close table
   Table_obj.Close();
   }

// ------------------------------------------------------------------
//  Short description:
//      Show the properties form to the user.  Return true if user
//      pressed ok.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
bool Depth_child::Show_properties (void)
   {
   if (Property_form->All_ok)
      return (Property_form->ShowModal() == mrOk);
   else
      {
      No_date_form->ShowModal();
      return false;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      This method creates a chart object derived from High_level_chart_base
//      and simply returns a pointer to the caller.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
High_level_chart_base* Depth_child::Create_chart_object (void)
   {
   return new APSIM_depth_chart (this);
   }

// ------------------------------------------------------------------
//  Short description:
//      Using the properties forms, this method fills an array of
//      chart description objects (as defined in high level charting library).
//      Returns the number of descriptions created.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void Depth_child::Setup_chart_object (High_level_chart_base* Chart_p,
                                      bool Is_predicted)
   {
   Depth_chart* Chart_ptr = dynamic_cast<Depth_chart*>(Chart_p);
   if (Chart_ptr != NULL)
      {
      // setup a splitting field for depth chart.
      Chart_ptr->Set_splitting_field ("date");
      
      // create each xy pair.
      Add_xy_to_chart (Property_form->X1_variable,
                       Property_form->Y_variable,
                       Chart_ptr,
                       Is_predicted);
      // create each xy pair.
      Add_xy_to_chart (Property_form->X2_variable,
                       Property_form->Y_variable,
                       Chart_ptr,
                       Is_predicted);
      // create each xy pair.
      Add_xy_to_chart (Property_form->X3_variable,
                       Property_form->Y_variable,
                       Chart_ptr,
                       Is_predicted);
      // create each xy pair.
      Add_xy_to_chart (Property_form->X4_variable,
                       Property_form->Y_variable,
                       Chart_ptr,
                       Is_predicted);
      // create each xy pair.
      Add_xy_to_chart (Property_form->X5_variable,
                       Property_form->Y_variable,
                       Chart_ptr,
                       Is_predicted);
      }

   }
// ------------------------------------------------------------------
//  Short description:
//

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void Depth_child::Add_xy_to_chart (TComboBox* X_variable,
                                   TComboBox* Y_variable,
                                   Depth_chart* Chart_ptr,
                                   bool Is_predicted)
   {
   if (X_variable->Text != "" && Y_variable->Text != "")
      {
      Format_base* Format_ptr;
      if (Is_predicted)
         Format_ptr = new Scatter_format (Scatter_format::Lines);
      else
         Format_ptr = new Scatter_format (Scatter_format::Markers);

      Chart_ptr->Add_xy (X_variable->Text.c_str(),
                         Y_variable->Text.c_str(),
                         Format_ptr);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      create a filter with all dates.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void Depth_child::Setup_filter (Record_filter& Filter)
   {
   Filter.Clear();
   
   // loop through all selected dates.
   for (int i = 0; i < Property_form->Date_box->Items->Count; i++)
      {
      if (Property_form->Date_box->Selected[i])
         {
         // add a criteria to filter.
         Filter.Add("date", "=", Property_form->Date_box->Items->Strings[i].c_str());
         }
      }
   }

