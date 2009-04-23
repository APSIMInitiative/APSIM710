//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "TGridForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "DBAdvGrd"
#pragma resource "*.dfm"
TGridForm *GridForm;
//---------------------------------------------------------------------------
__fastcall TGridForm::TGridForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
