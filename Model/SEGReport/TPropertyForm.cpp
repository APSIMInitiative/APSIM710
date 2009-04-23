//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TPropertyForm.h"
#include <generalVCL\vcl_functions.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "DBAdvGrd"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TPropertyForm *PropertyForm;
//---------------------------------------------------------------------------
__fastcall TPropertyForm::TPropertyForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
__fastcall TPropertyForm::~TPropertyForm()
   {
   }
//---------------------------------------------------------------------------
void __fastcall TPropertyForm::FormShow(TObject *Sender)
   {
   setComponent(component);
   }
//---------------------------------------------------------------------------
