//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TChartForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma link "AdvCGrid"
#pragma link "AdvGrid"
#pragma link "AsgLinks"
#pragma link "BaseGrid"
#pragma resource "*.dfm"
TChartForm *ChartForm;
//---------------------------------------------------------------------------
__fastcall TChartForm::TChartForm(TComponent* Owner)
   : TPropertyForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void TChartForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);
   graph = dynamic_cast<TGraph*>(component);
   NumMonthsCheckBox->Checked = (graph->NumMonthsBottomAxis > 0);
   NumMonthsCheckBoxClick(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TChartForm::ChartPropertyLabelClick(TObject *Sender)
   {
   graph->userEdit();
   }
//---------------------------------------------------------------------------
void __fastcall TChartForm::NumMonthsCheckBoxClick(TObject *Sender)
   {
   NumMonthsEdit->Enabled = NumMonthsCheckBox->Checked;
   NumMonthsLabel->Enabled = NumMonthsCheckBox->Checked;
   if (NumMonthsCheckBox->Checked)
      NumMonthsEdit->Text = IntToStr(graph->NumMonthsBottomAxis);
   else
      NumMonthsEdit->Text = "";
   }
//---------------------------------------------------------------------------
void __fastcall TChartForm::NumMonthsEditChange(TObject *Sender)
   {
   graph->NumMonthsBottomAxis = atoi(NumMonthsEdit->Text.c_str());
   }
//---------------------------------------------------------------------------

