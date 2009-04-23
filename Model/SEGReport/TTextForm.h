//---------------------------------------------------------------------------

#ifndef TTextFormH
#define TTextFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TPropertyForm.h"
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include "TText.h"
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TTextForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TFontDialog *FontDialog;
   TLabel *Label7;
   TComboBox *AlignmentCombo;
   TCheckBox *AutosizeCheckBox;
   TLabel *FontLabel;
   TLabel *Label6;
   TRichEdit *TextEdit;
   TLabel *Label5;
   TLabel *Label1;
   TLabel *Label2;
   TLabel *Label3;
   TLabel *Label4;
   TLabel *Label8;
   TLabel *Label9;
   void __fastcall TextEditExit(TObject *Sender);
   void __fastcall AlignmentComboChange(TObject *Sender);
   void __fastcall ToolbarCheckBoxClick(TObject *Sender);
   void __fastcall FontLabelClick(TObject *Sender);
private:	// User declarations
      TText* text;
public:		// User declarations
   __fastcall TTextForm(TComponent* Owner);
   virtual void setComponent(TComponent* component);

};
//---------------------------------------------------------------------------
extern PACKAGE TTextForm *TextForm;
//---------------------------------------------------------------------------
#endif
