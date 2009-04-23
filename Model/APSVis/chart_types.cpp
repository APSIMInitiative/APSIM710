//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl\vcl.h>
#pragma hdrstop

#include "chart_types.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TChart_types_form *Chart_types_form;
//---------------------------------------------------------------------------
__fastcall TChart_types_form::TChart_types_form(TComponent* Owner)
   : TForm(Owner)
   {
   Scatter_radio->Checked = true;
   }
//---------------------------------------------------------------------------