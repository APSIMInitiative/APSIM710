//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl\vcl.h>
#pragma hdrstop

#include "TFreq_prop_form.h"
//---------------------------------------------------------------------------
#pragma link "AdvCombo"
#pragma link "ImagePicker"
#pragma resource "*.dfm"
TFrequency_prop_form *Frequency_prop_form;
//---------------------------------------------------------------------------
__fastcall TFrequency_prop_form::TFrequency_prop_form(TComponent* Owner)
   : TForm(Owner)
{
}
// ------------------------------------------------------------------
//  Short description:
//      Setup the object.

//  Notes:

//  Changes:
//    DPH 29/1/98

// ------------------------------------------------------------------
void TFrequency_prop_form::Setup (TStringList* Field_list)
   {
   if (X_variables->Items->Count == 0)
      {
      X_variables->Items->Assign(Field_list);
      Chart_type_combo->ItemIndex = 3;
      }
   }
   
