//---------------------------------------------------------------------------
#ifndef TDepth_prop_formH
#define TDepth_prop_formH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Buttons.hpp>
#include <general\date_class.h>
#include "table_base.h"
//---------------------------------------------------------------------------
class TDepth_prop_form : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label1;
   TLabel *Label2;
   TComboBox *Y_variable;
   TComboBox *X1_variable;
   TComboBox *X2_variable;
   TComboBox *X3_variable;
   TComboBox *X4_variable;
   TComboBox *X5_variable;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
   TListBox *Date_box;
   TLabel *Label3;
   TLabel *Label4;
   TLabel *Label5;
   TLabel *Label6;
   TLabel *Label7;
   void __fastcall FormShow(TObject *Sender);
   
   
   void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
private:	// User declarations
public:		// User declarations
   __fastcall TDepth_prop_form(TComponent* Owner);
   void Setup (TStringList* Field_list, Table_base* Table_ptr);
   bool All_ok;

};
//---------------------------------------------------------------------------
extern TDepth_prop_form *Depth_prop_form;
//---------------------------------------------------------------------------
#endif
