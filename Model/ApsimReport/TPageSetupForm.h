//---------------------------------------------------------------------------

#ifndef TPageSetupFormH
#define TPageSetupFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TPageSetupForm : public TForm
{
__published:	// IDE-managed Components
   TSpeedButton *PortraitButton;
   TSpeedButton *LandscapeButton;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
private:	// User declarations
public:		// User declarations
   __fastcall TPageSetupForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPageSetupForm *PageSetupForm;
//---------------------------------------------------------------------------
#endif
