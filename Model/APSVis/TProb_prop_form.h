//---------------------------------------------------------------------------
#ifndef TProb_prop_formH
#define TProb_prop_formH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Buttons.hpp>
#include "AdvCombo.hpp"
#include "ImagePicker.hpp"
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TProb_prop_form : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label2;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
   TListBox *X_variables;
   TGroupBox *GroupBox1;
   TRadioButton *Prob_exceed;
   TRadioButton *Cum_prob;
   TLabel *Label4;
   TImageList *ImageList1;
   TImagePicker *Chart_type_combo;
private:	// User declarations
public:		// User declarations
   __fastcall TProb_prop_form(TComponent* Owner);
   void Setup (TStringList* Field_list);
};
//---------------------------------------------------------------------------
extern TProb_prop_form *Prob_prop_form;
//---------------------------------------------------------------------------
#endif
