//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl\vcl.h>
#pragma hdrstop

#include "TProb_prop_form.h"
//---------------------------------------------------------------------------
//#pragma link "tbitmap_combo"
#pragma link "AdvCombo"
#pragma link "ImagePicker"
#pragma resource "*.dfm"
TProb_prop_form *Prob_prop_form;
//---------------------------------------------------------------------------
__fastcall TProb_prop_form::TProb_prop_form(TComponent* Owner)
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
void TProb_prop_form::Setup (TStringList* Field_list)
   {
   if (X_variables->Items->Count == 0)
      {
      X_variables->Items->Assign(Field_list);
      Chart_type_combo->ItemIndex = 2;
      }
   }

