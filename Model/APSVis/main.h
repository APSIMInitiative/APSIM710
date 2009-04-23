//----------------------------------------------------------------------------
#ifndef MainH
#define MainH
//----------------------------------------------------------------------------
#include "chart_child.h"
#include <vcl\ComCtrls.hpp>
#include <vcl\ExtCtrls.hpp>
#include <vcl\Messages.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\Dialogs.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Menus.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\Classes.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Windows.hpp>
#include <vcl\System.hpp>
#include <vcl\DdeMan.hpp>
//----------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:
	TMainMenu *MainMenu1;
	TMenuItem *File1;
	TMenuItem *FileNewItem;
	TMenuItem *Window1;
	TMenuItem *Help1;
	TMenuItem *N1;
	TMenuItem *FileExitItem;
	TMenuItem *WindowCascadeItem;
	TMenuItem *WindowTileItem;
	TMenuItem *WindowArrangeItem;
	TMenuItem *HelpAboutItem;
   TOpenDialog *Predicted_open_dialog;
	TMenuItem *WindowMinimizeItem;
	TPanel *SpeedPanel;
	TStatusBar *StatusBar;
   TMenuItem *Scatterchart1;
   TSpeedButton *Scatter_button;
   TSpeedButton *Predicted_files_button;
   TSpeedButton *Observed_files_button;
   TOpenDialog *Observed_open_dialog;
   TSpeedButton *Property_button;
   TSpeedButton *Edit_chart_button;
   TMenuItem *Openpredictedfiles1;
   TMenuItem *Openobservedfiles1;
   TMenuItem *N2;
   TMenuItem *N3;
   TMenuItem *Properties1;
   TMenuItem *Editchart1;
   TMenuItem *Print1;
   TMenuItem *Edit1;
   TMenuItem *CopytCtlC1;
   TPrintDialog *Print_dialog;
   TSpeedButton *Depth_button;
   TMenuItem *Depthchart1;
   TMenuItem *Contents1;
   TSpeedButton *Prob_button;
   TMenuItem *Probabilitychart1;
   TSpeedButton *Freq_button;
   TMenuItem *Frequencychart1;
   TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall WindowCascadeItemClick(TObject *Sender);
	void __fastcall UpdateMenuItems(TObject *Sender);
	void __fastcall WindowTileItemClick(TObject *Sender);
	void __fastcall WindowArrangeItemClick(TObject *Sender);
	void __fastcall FileExitItemClick(TObject *Sender);
	void __fastcall WindowMinimizeItemClick(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
   void __fastcall User_create_scatter_chart(TObject *Sender);
   void __fastcall User_specify_properties(TObject *Sender);
   void __fastcall User_edit_chart(TObject *Sender);
   void __fastcall User_specify_predicted_files(TObject *Sender);
   void __fastcall User_specify_observed_files(TObject *Sender);


   
   void __fastcall Print1Click(TObject *Sender);
   void __fastcall CopytCtlC1Click(TObject *Sender);
   void __fastcall User_create_depth_chart(TObject *Sender);
   
   void __fastcall HelpAboutItemClick(TObject *Sender);
   void __fastcall Contents1Click(TObject *Sender);
   void __fastcall User_create_probability_chart(TObject *Sender);
   void __fastcall User_create_frequency_chart(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall Timer1Timer(TObject *Sender);
private:
   list<string> Predicted_file_list;
   list<string> Observed_file_list;
   
	void __fastcall CreateMDIChild(const String Name);
	void __fastcall ShowHint(TObject *Sender);
   void Setup_childs_database (TChart_child *Child);
   void Display_chart_types_box (void);
   void __fastcall Application_minimize (TObject* Sender);
   void readCommandLine();

public:
	virtual __fastcall TMainForm(TComponent *Owner);
};
//----------------------------------------------------------------------------
extern TMainForm *MainForm;
extern TChart_child *__fastcall MDIChildCreate(void);
//----------------------------------------------------------------------------
#endif
