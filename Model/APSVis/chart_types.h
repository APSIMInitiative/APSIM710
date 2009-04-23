//---------------------------------------------------------------------------
#ifndef chart_typesH
#define chart_typesH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>
#include <vcl\Buttons.hpp>
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TChart_types_form : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label1;
   TImage *Image1;
   TRadioButton *Scatter_radio;
   TImage *Image2;
   TRadioButton *Depth_radio;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
   TImage *Image3;
   TRadioButton *Prob_radio;
   TRadioButton *Freq_radio;
   TImage *Image4;
private:	// User declarations
public:		// User declarations
   __fastcall TChart_types_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TChart_types_form *Chart_types_form;
//---------------------------------------------------------------------------
#endif
