//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TPageSetupForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TPageSetupForm *PageSetupForm;
//---------------------------------------------------------------------------
__fastcall TPageSetupForm::TPageSetupForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
