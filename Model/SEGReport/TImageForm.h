//---------------------------------------------------------------------------

#ifndef TImageFormH
#define TImageFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvEdBtn.hpp"
#include "AdvEdit.hpp"
#include "TPropertyForm.h"
#include "TImage.h"
#include <Dialogs.hpp>
#include <ExtDlgs.hpp>
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TImageForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TOpenPictureDialog *OpenPictureDialog;
   TLabel *FilenameLabel;
   TAdvEditBtn *ImageFileEdit;
   TCheckBox *LinkCheckBox;
   TCheckBox *AutoSizeCheckBox;
   TCheckBox *CentreCheckBox;
   TCheckBox *StretchCheckBox;
   void __fastcall AutoSizeCheckBoxClick(TObject *Sender);
   void __fastcall CentreCheckBoxClick(TObject *Sender);
   void __fastcall StretchCheckBoxClick(TObject *Sender);
   void __fastcall ImageFileEditClickBtn(TObject *Sender);
   void __fastcall LinkCheckBoxClick(TObject *Sender);
   void __fastcall ImageFileEditExit(TObject *Sender);
private:	// User declarations
   ::TImage* image;

public:		// User declarations
   __fastcall TImageForm(TComponent* Owner);
   virtual void setComponent(TComponent* comp);

   };
//---------------------------------------------------------------------------
extern PACKAGE TImageForm *ImageForm;
//---------------------------------------------------------------------------
#endif
