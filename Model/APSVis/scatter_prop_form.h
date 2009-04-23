//---------------------------------------------------------------------------
#ifndef scatpropH
#define scatpropH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Buttons.hpp>
#include "AdvCombo.hpp"
#include "ImagePicker.hpp"
#include <ImgList.hpp>
// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a scatter chart property form

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
class TScatter_prop_form : public TForm
{
__published:	// IDE-managed Components
   TComboBox *X_variable;
   TLabel *Label1;
   TComboBox *Y1_variable;
   TCheckBox *Y1_right_axis;
   TLabel *Label3;
   TLabel *Label4;
   TComboBox *Y2_variable;
   TCheckBox *Y2_right_axis;
   TComboBox *Y3_variable;
   TCheckBox *Y3_right_axis;
   TComboBox *Y4_variable;
   TCheckBox *Y4_right_axis;
   TComboBox *Y5_variable;
   TCheckBox *Y5_right_axis;
   TLabel *Label2;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
   TRadioButton *Numeric_radio;
   TRadioButton *Date_radio;
   TRadioButton *String_radio;
   TCheckBox *AccumulateX;
   TLabel *Label5;
   TCheckBox *AccumulateY1;
   TCheckBox *AccumulateY2;
   TCheckBox *AccumulateY3;
   TCheckBox *AccumulateY4;
   TCheckBox *AccumulateY5;
   TImageList *ImageList1;
   TImagePicker *Chart_type2;
   TImagePicker *Chart_type3;
   TImagePicker *Chart_type4;
   TImagePicker *Chart_type5;
   TImagePicker *Chart_type1;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall X_variableChange(TObject *Sender);
private:	// User declarations

public:		// User declarations
   __fastcall TScatter_prop_form(TComponent* Owner);
   void Setup (TStringList* Field_list);

};
//---------------------------------------------------------------------------
extern TScatter_prop_form *Scatter_prop_form;
//---------------------------------------------------------------------------
#endif
