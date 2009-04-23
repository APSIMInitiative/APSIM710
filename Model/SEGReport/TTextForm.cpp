//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TTextForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TTextForm *TextForm;
//---------------------------------------------------------------------------
__fastcall TTextForm::TTextForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
void TTextForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);
   text = dynamic_cast<TText*> (component);

   TextEdit->Lines->Text = text->text;
   AlignmentCombo->Text = text->alignment;
   AutosizeCheckBox->Checked = text->AutoSize;
   }
//---------------------------------------------------------------------------
void __fastcall TTextForm::TextEditExit(TObject *Sender)
   {
   text->text = TextEdit->Lines->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TTextForm::AlignmentComboChange(TObject *Sender)
   {
   text->alignment = AlignmentCombo->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TTextForm::ToolbarCheckBoxClick(TObject *Sender)
   {
   text->AutoSize = AutosizeCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TTextForm::FontLabelClick(TObject *Sender)
   {
   FontDialog->Font = text->Font;
   if (FontDialog->Execute())
      text->Font->Assign(FontDialog->Font);
   }
//---------------------------------------------------------------------------

