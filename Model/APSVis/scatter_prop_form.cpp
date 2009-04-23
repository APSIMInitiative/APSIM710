//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl\vcl.h>
#pragma hdrstop

#include "scatter_prop_form.h"
#include <General\stristr.h>

//---------------------------------------------------------------------------
//#pragma link "tbitmap_combo"
#pragma link "AdvCombo"
#pragma link "ImagePicker"
#pragma resource "*.dfm"
TScatter_prop_form *Scatter_prop_form;
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
__fastcall TScatter_prop_form::TScatter_prop_form(TComponent* Owner)
   : TForm(Owner)
   {
   Numeric_radio->Checked = true;
   }
// ------------------------------------------------------------------
//  Short description:
//      Setup the object.

//  Notes:

//  Changes:
//    DPH 25/7/97
//    dph 6/4/98 added a call to x_variableChange method

// ------------------------------------------------------------------
void TScatter_prop_form::Setup (TStringList* Field_list)
   {
   if (X_variable->Items->Count == 0)
      {
      X_variable->Items->Assign(Field_list);
      Y1_variable->Items->Assign(Field_list);
      Y2_variable->Items->Assign(Field_list);
      Y3_variable->Items->Assign(Field_list);
      Y4_variable->Items->Assign(Field_list);
      Y5_variable->Items->Assign(Field_list);

      Chart_type1->ItemIndex = 2;
      Chart_type2->ItemIndex = 2;
      Chart_type3->ItemIndex = 2;
      Chart_type4->ItemIndex = 2;
      Chart_type5->ItemIndex = 2;

      // setup some default x values.
      int Indx = Field_list->IndexOf ("date");
      if (Indx == -1)
         Indx = Field_list->IndexOf ("day");

      if (Indx >= 0)
         {
         X_variable->ItemIndex = Indx;
         }
      X_variableChange(NULL);
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      Change keyboard focus to y1 combo box when form is shown.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void __fastcall TScatter_prop_form::FormShow(TObject *Sender)
   {
   if (X_variable->ItemIndex >= 0)
      Y1_variable->SetFocus();
   }
// ------------------------------------------------------------------
//  Short description:
//      When user changes the x variable update the x radio button
//      to reflect the type of data being plotted.

//  Notes:

//  Changes:
//    DPH 6/4/98

// ------------------------------------------------------------------
void __fastcall TScatter_prop_form::X_variableChange(TObject *Sender)
   {
   if (stristr(X_variable->Text.c_str(), "date") != NULL)
      Date_radio->Checked = true;
   else
      Numeric_radio->Checked = true;
   }
//---------------------------------------------------------------------------
