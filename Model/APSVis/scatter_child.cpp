//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl\vcl.h>
#pragma hdrstop

#include "scatter_child.h"
#include "scatter_format.h"
#include "bar_format.h"
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
__fastcall Scatter_child::Scatter_child(TComponent *Owner)
  	: TChart_child(Owner)
   {
   Property_form = new TScatter_prop_form (this);
   }

// ------------------------------------------------------------------
//  Short description:
//      Setup the properties form.  This is called whenever the database
//      that is passed into this method is changed.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void Scatter_child::Setup_properties (Database& DB)
   {
   TStringList* Field_list = new TStringList;
   Get_field_names (DB, Field_list);
   Property_form->Setup (Field_list);
   delete Field_list;
   }

// ------------------------------------------------------------------
//  Short description:
//      Show the properties form to the user.  Return true if user
//      pressed ok.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
bool Scatter_child::Show_properties (void)
   {
   return (Property_form->ShowModal() == mrOk);
   }

// ------------------------------------------------------------------
//  Short description:
//      This method creates a chart object derived from High_level_chart
//      and simply returns a pointer to the caller.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
High_level_chart_base* Scatter_child::Create_chart_object (void)
   {
   return new Scatter_chart(this);
   }

// ------------------------------------------------------------------
//  Short description:
//      Using the properties forms, this method creates xy plots on the
//      specified chart.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void Scatter_child::Setup_chart_object (High_level_chart_base* Chart_p,
                                        bool Is_predicted)
   {
   Scatter_chart* Chart_ptr = dynamic_cast<Scatter_chart*>(Chart_p);
   if (Chart_ptr != NULL)
      {
      Add_xy_to_chart (Property_form->X_variable,
                       Property_form->Y1_variable,
                       Property_form->Y1_right_axis,
                       Property_form->Chart_type1,
                       Chart_ptr,
                       Is_predicted,
                       Property_form->AccumulateX,
                       Property_form->AccumulateY1);
      Add_xy_to_chart (Property_form->X_variable,
                       Property_form->Y2_variable,
                       Property_form->Y2_right_axis,
                       Property_form->Chart_type2,
                       Chart_ptr,
                       Is_predicted,
                       Property_form->AccumulateX,
                       Property_form->AccumulateY2);
      Add_xy_to_chart (Property_form->X_variable,
                       Property_form->Y3_variable,
                       Property_form->Y3_right_axis,
                       Property_form->Chart_type3,
                       Chart_ptr,
                       Is_predicted,
                       Property_form->AccumulateX,
                       Property_form->AccumulateY3);
      Add_xy_to_chart (Property_form->X_variable,
                       Property_form->Y4_variable,
                       Property_form->Y4_right_axis,
                       Property_form->Chart_type4,
                       Chart_ptr,
                       Is_predicted,
                       Property_form->AccumulateX,
                       Property_form->AccumulateY4);
      Add_xy_to_chart (Property_form->X_variable,
                       Property_form->Y5_variable,
                       Property_form->Y5_right_axis,
                       Property_form->Chart_type5,
                       Chart_ptr,
                       Is_predicted,
                       Property_form->AccumulateX,
                       Property_form->AccumulateY5);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//        Create and xy pair in the specified chart based on info
//        user has selected in the specified controls.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void Scatter_child::Add_xy_to_chart (TComboBox* X_variable,
                                     TComboBox* Y_variable,
                                     TCheckBox* Right_axis_check,
                                     TCustomComboBox* Chart_type_combo,
                                     Scatter_chart* Chart_ptr,
                                     bool Is_predicted,
                                     TCheckBox* Accumulate_x,
                                     TCheckBox* Accumulate_y)
   {
   if (X_variable->Text != "" && Y_variable->Text != "")
      {
      // work out which field type user has chosen.
      Field_type_enum Field_type;
      if (Property_form->Numeric_radio->Checked)
         Field_type = numeric;
      else if (Property_form->Date_radio->Checked)
         Field_type = date;
      else
         Field_type = alpha;

      Y_axis_link_enum y_axis_link = ::left;
      if (Right_axis_check->Checked)
         y_axis_link = ::right;

      Format_base* Format_ptr;
      if (Is_predicted)
         {
         switch (Chart_type_combo->ItemIndex)
            {
            case 0 : Format_ptr = new Scatter_format (Scatter_format::Markers);
                     break;
            case 1 : Format_ptr = new Scatter_format (Scatter_format::Markers_lines);
                     break;
            case 2 : Format_ptr = new Scatter_format (Scatter_format::Lines);
                     break;
            case 3 : Format_ptr = new Bar_format (Bar_format::Vertical_side_by_side);
                     break;
            case 4 : Format_ptr = new Bar_format (Bar_format::Vertical_stacked);
                     break;
            case 5 : Format_ptr = new Bar_format (Bar_format::Vertical_stacked100);
                     break;
            case 6 : Format_ptr = new Bar_format (Bar_format::Horizontal_side_by_side);
                     break;
            case 7 : Format_ptr = new Bar_format (Bar_format::Horizontal_stacked);
                     break;
            case 8 : Format_ptr = new Bar_format (Bar_format::Horizontal_stacked100);
                     break;
            }
         }
      else
         Format_ptr = new Scatter_format (Scatter_format::Markers);

      // send all info to chart.
      Chart_ptr->Add_xy (X_variable->Text.c_str(),
                         Y_variable->Text.c_str(),
                         Field_type,
                         bottom,
                         y_axis_link,
                         Format_ptr,
                         NULL,
                         Accumulate_x->Checked,
                         Accumulate_y->Checked);
      }
   }

