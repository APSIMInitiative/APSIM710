//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
USEFORM("main.cpp", MainForm);
USEFORM("TDepth_prop_form.cpp", Depth_prop_form);
USEFORM("no_date.cpp", No_date_form);
USEFORM("about.cpp", AboutBox);
USEFORM("chart_types.cpp", Chart_types_form);
USEFORM("TProb_prop_form.cpp", Prob_prop_form);
USEFORM("TFreq_prop_form.cpp", Frequency_prop_form);
USEFORM("chart_child.cpp", Chart_child);
//---------------------------------------------------------------------------
#pragma link "TeEngine"
#pragma link "TeeProcs"
#pragma link "Series"
#pragma link "EditChar"
char* Command_line;
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR cmd_line, int)
   {
   Command_line = cmd_line;

   try
   {
      Application->Initialize();
      Application->Title = "APSVisual";
      Application->CreateForm(__classid(TMainForm), &MainForm);
                 Application->CreateForm(__classid(TAboutBox), &AboutBox);
                 Application->CreateForm(__classid(TNo_date_form), &No_date_form);
                 Application->Run();
   }
   catch (Exception &exception)
   {
      Application->ShowException(&exception);
   }
   return 0;
}
//---------------------------------------------------------------------------
