//---------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Main.h"
#include "scatter_child.h"
#include "about.h"
#include <sstream.h>
#include <general\stream_functions.h>
#include <general\string_functions.h>
#include <generalvcl\vcl_functions.h>
#include <general\path.h>
#include <ApsimShared\ApsimDirectories.h>
#include "depth_child.h"
#include "prob_child.h"
#include "freq_child.h"
#include "chart_types.h"
extern char* Command_line;
//---------------------------------------------------------------------
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent *Owner)
	: TForm(Owner)
   {
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender)
{
	Application->OnHint = ShowHint;
	Screen->OnActiveFormChange = UpdateMenuItems;
   Application->OnMinimize = Application_minimize;
}
//---------------------------------------------------------------------
void __fastcall TMainForm::ShowHint(TObject *Sender)
{
	StatusBar->SimpleText = Application->Hint;
}
//---------------------------------------------------------------------
void __fastcall TMainForm::Application_minimize (TObject* Sender)
   {
   ShowWindow(Handle, SW_MINIMIZE);
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::CreateMDIChild(String Name)
   {
	TChart_child *Child;

   // do we have any predicted OR observed files?
   if (Predicted_file_list.size() > 0 ||
       Observed_file_list.size() > 0)
      {
      //--- create a new MDI child window ----
      if (Name == "Scatter")
         Child = new Scatter_child(Application);
      else if (Name == "Depth")
         Child = new Depth_child(Application);
      else if (Name == "Probability")
         Child = new Prob_child(Application);
      else if (Name == "Frequency")
         Child = new Freq_child(Application);
      else
         Child = NULL;

      Child->Caption = Name + IntToStr(MDIChildCount);
      Setup_childs_database (Child);

      bool ok = Child->Edit_properties();
      if (ok)
         Child->Invalidate();
      else
         delete Child;
      }
   else
      Application->MessageBox("No predicted or observed files have been selected.  "
                              "Under the file menu select OPEN PREDICTED FILES "
                              "(or click on the first button on button bar) to "
                              "specify the predicted files to plot on the chart.",
                              "Error",
                              MB_ICONSTOP | MB_OK);
   }
//---------------------------------------------------------------------
void Setup_open_dialog (TOpenDialog* Open_dialog)
   {
   // I cannot get this to work properly.  Open_dialog->FileName is an AnsiString class.
   // To get multiple files to be selected by default, I need to embed NULL
   // characters in the file name (see Win32 reference for OpenFileName
   // routine).  An AnsiString doesn't allow me to embed these NULLs.

   string Initial_file_name;

   // loop through all files
   for (int Iter = 0; Iter < Open_dialog->Files->Count; Iter++)
      {
      Path p;
      p.Set_path (Open_dialog->Files->Strings[Iter].c_str());

      Initial_file_name += "\"";
      Initial_file_name += p.Get_name();
      Initial_file_name += "\" ";
      }

   if (Initial_file_name != "")
      Open_dialog->FileName = Initial_file_name.c_str();
   }
//---------------------------------------------------------------------
void Setup_open_dialog (TOpenDialog* Open_dialog, list<string>& File_list)
   {
   string Initial_file_name;

   // loop through all files
   for (list<string>::iterator Iter = File_list.begin();
                               Iter != File_list.end();
                               Iter++)
      {
      Path p;
      p.Set_path ((*Iter).c_str());

      Initial_file_name += "\"";
      Initial_file_name += p.Get_name();
      Initial_file_name += "\" ";
      Open_dialog->InitialDir = p.Get_directory().c_str();
      }

   Open_dialog->FileName += Initial_file_name.c_str();
   }

//---------------------------------------------------------------------
void TMainForm::Setup_childs_database (TChart_child *Child)
   {
   Child->Set_predicted_files (Predicted_file_list);
   Child->Set_observed_files (Observed_file_list);
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::FileExitItemClick(TObject *Sender)
{
	Close();
}
//---------------------------------------------------------------------
void __fastcall TMainForm::WindowCascadeItemClick(TObject *Sender)
{
	Cascade();
}
//---------------------------------------------------------------------
void __fastcall TMainForm::WindowTileItemClick(TObject *Sender)
{
	Tile();
}
//---------------------------------------------------------------------
void __fastcall TMainForm::WindowArrangeItemClick(TObject *Sender)
{
	ArrangeIcons();
}
//---------------------------------------------------------------------
void __fastcall TMainForm::WindowMinimizeItemClick(TObject *Sender)
{
	int i;

	//---- Must be done backwards through the MDIChildren array ----
	for (i=MDIChildCount-1; i >= 0; i--)
		MDIChildren[i]->WindowState = wsMinimized;
}
//---------------------------------------------------------------------
void __fastcall TMainForm::UpdateMenuItems(TObject *Sender)
{
	WindowCascadeItem->Enabled = MDIChildCount > 0;
	WindowTileItem->Enabled = MDIChildCount > 0;
	WindowArrangeItem->Enabled = MDIChildCount > 0;
	WindowMinimizeItem->Enabled = MDIChildCount > 0;
}
//---------------------------------------------------------------------
void __fastcall TMainForm::FormDestroy(TObject *Sender)
{
	Screen->OnActiveFormChange = NULL;
}
//---------------------------------------------------------------------
void __fastcall TMainForm::User_specify_predicted_files(TObject *Sender)
   {
//   Setup_open_dialog (Predicted_open_dialog, Predicted_file_list);
   if (Predicted_open_dialog->Execute())
      TStrings_2_stl (Predicted_open_dialog->Files, Predicted_file_list);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::User_specify_observed_files(TObject *Sender)
   {
//   Setup_open_dialog (Observed_open_dialog, Observed_file_list);
   if (Observed_open_dialog->Execute())
      TStrings_2_stl (Observed_open_dialog->Files, Observed_file_list);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::User_specify_properties(TObject *Sender)
   {
   TChart_child* Chart_child = dynamic_cast<TChart_child*>(ActiveMDIChild);
   if (Chart_child != NULL)
      {
      Chart_child->Edit_properties ();
      Chart_child->Invalidate();
      }
   else
      Application->MessageBox("You must create a new chart first before modifying "
                              "any chart properties.",
                              "Error",
                              MB_ICONSTOP | MB_OK);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::User_edit_chart(TObject *Sender)
   {
   TChart_child* Chart_child = dynamic_cast<TChart_child*>(ActiveMDIChild);
   if (Chart_child != NULL)
      {
      Chart_child->Edit_chart();
      Chart_child->Invalidate();
      }
   else
      Application->MessageBox("You must create a new chart first before editing "
                              "any chart properties.",
                              "Error",
                              MB_ICONSTOP | MB_OK);

   }
//---------------------------------------------------------------------------
void TMainForm::readCommandLine()
   {
   if (strlen(Command_line) > 0)
      {
      TStringList* fileList = new TStringList;
      fileList->LoadFromFile(Command_line);
      TStrings_2_stl(fileList, Predicted_file_list);
      delete fileList;

      Setup_open_dialog (Predicted_open_dialog, Predicted_file_list);
      Timer1->Enabled = true;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Print1Click(TObject *Sender)
   {
   TChart_child* Chart_child = dynamic_cast<TChart_child*>(ActiveMDIChild);
   if (Chart_child != NULL)
      {
      Chart_child->Print ();
      }
   else
      Application->MessageBox ("No charts to print.",
                               "Error",
                               MB_ICONSTOP | MB_OK);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::CopytCtlC1Click(TObject *Sender)
   {
   TChart_child* Chart_child = dynamic_cast<TChart_child*>(ActiveMDIChild);
   if (Chart_child != NULL)
      Chart_child->Copy_to_clipboard ();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::HelpAboutItemClick(TObject *Sender)
   {
   AboutBox->ShowModal();
   }
//---------------------------------------------------------------------------
void TMainForm::Display_chart_types_box (void)
   {
   TChart_types_form* Chart_types_form = new TChart_types_form(this);
   if (Chart_types_form->ShowModal() == mrOk)
      {
      if (Chart_types_form->Scatter_radio->Checked)
         User_create_scatter_chart (NULL);
      else if (Chart_types_form->Depth_radio->Checked)
         User_create_depth_chart (NULL);
      else if (Chart_types_form->Prob_radio->Checked)
         User_create_probability_chart (NULL);
      else if (Chart_types_form->Freq_radio->Checked)
         User_create_frequency_chart (NULL);
      }
   delete Chart_types_form;
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Contents1Click(TObject *Sender)
   {
   string CommandLine = "winhlp32.exe \"" + getApsimDirectory() + "\\ApsimOutlook\\docs\\apsvis.hlp\"";
   WinExec (CommandLine.c_str(), SW_SHOW);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::User_create_scatter_chart(TObject *Sender)
   {
   CreateMDIChild("Scatter");
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::User_create_depth_chart(TObject *Sender)
   {
   CreateMDIChild("Depth");
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::User_create_probability_chart(TObject *Sender)
   {
   CreateMDIChild("Probability");
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::User_create_frequency_chart(TObject *Sender)
   {
   CreateMDIChild("Frequency");
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject *Sender)
   {
   readCommandLine();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Timer1Timer(TObject *Sender)
   {
   Timer1->Enabled = false;
   Display_chart_types_box ();
   }
//---------------------------------------------------------------------------

