//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TMainForm.h"
#include "TPageSetupForm.h"
#include <Generalvcl\vcl_functions.h>
#include <General\io_functions.h>
#include <General\inifile.h>
#include <General\path.h>
#include <ApsimShared\ApsimSettings.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "MRUFList"
#pragma link "JPEG"
#pragma resource "*.dfm"
TMainForm *MainForm;
extern AnsiString commandLine;

//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
   : TForm(Owner), report(TabControl)
   {
   }
//---------------------------------------------------------------------------
// Form has been shown - set everything up.
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject *Sender)
   {
   ZoomUpDown->Position = report.getZoom();
   report.getPageNames(TabControl->Tabs);
   pageChanged(NULL);

   ApsimSettings settings;
   string leftSt, topSt, widthSt, heightSt;
   settings.read("Apsim Report Pos|MainFormLeft", leftSt);
   settings.read("Apsim Report Pos|MainFormTop", topSt);
   settings.read("Apsim Report Pos|MainFormWidth", widthSt);
   settings.read("Apsim Report Pos|MainFormHeight", heightSt);
   if (leftSt != "" && topSt != "" && widthSt != "" && heightSt != "")
      {
      Left = atoi(leftSt.c_str());
      Top = atoi(topSt.c_str());
      Width = atoi(widthSt.c_str());
      Height = atoi(heightSt.c_str());
      }
   if (commandLine != "")
      processCommandLine(commandLine);
   }
//---------------------------------------------------------------------------
// Form has been closed - save everything.
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormClose(TObject *Sender, TCloseAction &Action)
   {
   report.edit(false);
   saveIfNecessary();
   ApsimSettings settings;
   settings.write("Apsim Report Pos|MainFormLeft", IntToStr(Left).c_str());
   settings.write("Apsim Report Pos|MainFormTop", IntToStr(Top).c_str());
   settings.write("Apsim Report Pos|MainFormWidth", IntToStr(Width).c_str());
   settings.write("Apsim Report Pos|MainFormHeight", IntToStr(Height).c_str());
   }
//---------------------------------------------------------------------------
// User wants to edit the report.
//---------------------------------------------------------------------------
void __fastcall TMainForm::EditReportActionExecute(TObject *Sender)
   {
   bool turnOn = EditReportButton->Down;

   // turn on edit mode and make palette dockable.
   static TForm* palette = NULL;
   if (!turnOn && palette != NULL)
      palette->Visible = false;

   palette = report.edit(turnOn);
   if (palette != NULL)
      {
      palette->DragMode = dmAutomatic;
      palette->DragKind = dkDock;
//      palette->BorderIcons.Clear();
      palette->Caption = "";
//      palette->BorderStyle = bsNone;
      palette->Parent = ButtonBar;
      }
   report.setObjectInspector(ObjectInspector, NULL);
   if (turnOn)
      {
      ObjectInspector->Visible = true;
      ObjectInspectorSplitter->Visible = true;
      }
   else
      {
      ObjectInspectorSplitter->Visible = false;
      ObjectInspector->Visible = false;
      }
   }
//---------------------------------------------------------------------------
// User wants to edit the data.
//---------------------------------------------------------------------------
void __fastcall TMainForm::EditDataActionExecute(TObject *Sender)
   {
   report.showDataPage();
   }
//---------------------------------------------------------------------------
// User has changed pages in tab control - update main form.
//---------------------------------------------------------------------------
void __fastcall TMainForm::pageChanged(TObject* sender)
   {
   ZoomUpDown->Position = report.getZoom();
   report.showPage(TabControl->TabIndex);
   report.showPage(TabControl->TabIndex+1);
   report.showPage(TabControl->TabIndex);
   }
//---------------------------------------------------------------------------
// User has clicked exit.
//---------------------------------------------------------------------------
void __fastcall TMainForm::ExitActionExecute(TObject *Sender)
   {
   Close();
   }
//---------------------------------------------------------------------------
// User has clicked new.
//---------------------------------------------------------------------------
void __fastcall TMainForm::NewActionExecute(TObject *Sender)
   {
   saveIfNecessary();
   report.clear();
   filename = "";
   setCaption();
   report.getPageNames(TabControl->Tabs);

   }
//---------------------------------------------------------------------------
// User has clicked open.
//---------------------------------------------------------------------------
void __fastcall TMainForm::OpenActionExecute(TObject *Sender)
   {
   // populate the open menu
   OpenMenu->Items->Clear();
   populateOpenMenu();

   // display the open menu.
   TPoint Pos = ButtonBar->ClientToScreen(TPoint(OpenButton->Left, OpenButton->Top + OpenButton->Height));
   OpenMenu->Popup(Pos.x, Pos.y);
   }

//---------------------------------------------------------------------------
// Populate the open menu.
//---------------------------------------------------------------------------
void TMainForm::populateOpenMenu()
   {
   TMenuItem* NewItem = new TMenuItem(OpenMenu);
   OpenMenu->Items->Add(NewItem);
   NewItem->Caption = "Open";
   NewItem->OnClick = OpenMenuItemClick;

   /*NewItem = new TMenuItem(OpenMenu);
   OpenMenu->Items->Add(NewItem);
   NewItem->Caption = "Open (safe mode)";
   NewItem->OnClick = OpenMenuItemClick;*/

   NewItem = new TMenuItem(OpenMenu);
   OpenMenu->Items->Add(NewItem);
   NewItem->Caption = "-";
   for (int i = 0; i != MRUFileList->Items->Count; i++)
      {
      NewItem = new TMenuItem(OpenMenu);
      OpenMenu->Items->Add(NewItem);
      string fileName = MRUFileList->Items->Strings[i].c_str();
      replaceAll(fileName, "&", "");
      NewItem->Caption = fileName.c_str();
      NewItem->OnClick = OpenMenuItemClick;
      }
   }

//---------------------------------------------------------------------------
// User has clicked an item on the open menu - respond.
//---------------------------------------------------------------------------
void __fastcall TMainForm::OpenMenuItemClick(TObject* sender)
   {
   TMenuItem* item = dynamic_cast<TMenuItem*> (sender);
   if (item != NULL)
      {
      if (item->Caption == "&Open")
         {
         if (OpenDialog1->Execute())
            {
            saveIfNecessary();
            open(OpenDialog1->FileName, false, false);
            ZoomUpDown->Position = report.getZoom();
            }
         }
      else if (item->Caption == "O&pen (safe mode)")
         {
         if (OpenDialog1->Execute())
            {
            saveIfNecessary();
            if (MessageBox(NULL, "Opening a file in safe mode means all chart series x/y values will "
                                 "be unlinked from their data source. This can help when .report files "
                                 "don't open properly. Are you sure you want to do this?",
                                 "Question",
                                 MB_ICONQUESTION | MB_YESNO) == IDYES)
               {
               open(OpenDialog1->FileName, false, true);
               ZoomUpDown->Position = report.getZoom();
               }
            }
         }

      else
         {
         string fileName = item->Caption.c_str();
         replaceAll(fileName, "&", "");
         open(fileName.c_str(), false, false);
         }
      }
   }
//---------------------------------------------------------------------------
// User has clicked save.
//---------------------------------------------------------------------------
void __fastcall TMainForm::SaveActionExecute(TObject *Sender)
   {
   save(filename);
   }
//---------------------------------------------------------------------------
// User has clicked saveAs
//---------------------------------------------------------------------------
void __fastcall TMainForm::SaveAsActionExecute(TObject *Sender)
   {
   if (SaveDialog1->Execute())
      save(SaveDialog1->FileName);
   }
//---------------------------------------------------------------------------
// Export the specified file to a PDF or RTF or another file type.
//---------------------------------------------------------------------------
void __fastcall TMainForm::OnExport(TObject *Sender)
   {
   if (ExportDialog->Execute())
      save(ExportDialog->FileName);
   }
//---------------------------------------------------------------------------
// Go open the specified file.
//---------------------------------------------------------------------------
void TMainForm::open(AnsiString file, bool quiet, bool safe)
   {
   filename = ExpandFileName(file);
   MRUFileList->AddItem(filename);
   report.load(filename.c_str(), quiet, safe);
   setCaption();
   ZoomUpDown->Position = report.getZoom();
   report.getPageNames(TabControl->Tabs);
   }
//---------------------------------------------------------------------------
// Go save to the specified file.
//---------------------------------------------------------------------------
void TMainForm::save(AnsiString file)
   {
   if (file == "")
      SaveAsActionExecute(NULL);
   else
      {
      if (ExtractFileExt(file) == ".report")
         {
         filename = file;
         setCaption();
         }
      report.save(file.c_str());
      MRUFileList->AddItem(filename);
      }
   }
//---------------------------------------------------------------------------
// Set the caption of the application.
//---------------------------------------------------------------------------
void TMainForm::setCaption(void)
   {
   static const char* APPLICATION_CAPTION = "APSIM Report";

   AnsiString caption = APPLICATION_CAPTION;
   if (filename != "")
      caption += " - " + ExtractFileName(filename);
   Caption = caption;
   }
//---------------------------------------------------------------------------
int TMainForm::textToZoom(AnsiString zoomText)
   {
   int posPercent = zoomText.Pos("%");
   if (posPercent == 0)
      posPercent = zoomText.Length() + 1;
   return StrToInt(zoomText.SubString(1, zoomText.Pos("%")-1));
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::ZoomEditKeyDown(TObject *Sender, WORD &Key,
      TShiftState Shift)
   {
   if (Key == VK_RETURN)
      {
      int zoom = textToZoom(ZoomEdit->Text);
      ZoomUpDown->Position = zoom;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::CopyToClipboardActionExecute(TObject *Sender)
   {
   report.copyToClipboard();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::PageSetupActionExecute(TObject *Sender)
   {
   PageSetupForm->PortraitButton->Down = report.getIsPortrait();
   PageSetupForm->LandscapeButton->Down = !report.getIsPortrait();
   if (PageSetupForm->ShowModal() == mrOk)
      report.setIsPortrait(PageSetupForm->PortraitButton->Down);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::PrintActionExecute(TObject *Sender)
   {
   report.print(false);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::PrintCurrentPageActionExecute(TObject *Sender)
   {
   report.print(true);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::ZoomEditChange(TObject *Sender)
   {
   static bool inHere = false;
   if (!inHere)
      {
      inHere = true;
      ZoomEdit->Text = IntToStr(ZoomUpDown->Position) + "%";
      report.setZoom(ZoomUpDown->Position);
      inHere = false;
      }
   }
//---------------------------------------------------------------------------
// Perform a save if the report needs saving and the user says so.
//---------------------------------------------------------------------------
void TMainForm::saveIfNecessary(void)
   {
   if (report.needsSaving() && MessageBox(NULL, "Save changes to report?", "Confirm",
                               MB_ICONQUESTION | MB_YESNO) == IDYES)
      save(filename);
   }
//---------------------------------------------------------------------------
// Refresh report
//---------------------------------------------------------------------------
void __fastcall TMainForm::RefreshActionExecute(TObject *Sender)
   {
   report.refresh(false);
   }
//---------------------------------------------------------------------------
// Use has clicked on a MRU filename - load it into the report.
//---------------------------------------------------------------------------
void __fastcall TMainForm::MRUFileListMRUItemClick(TObject *Sender,
      AnsiString AFilename)
   {
   open(AFilename, false, false);
   }
//---------------------------------------------------------------------------
// process the specified command line.
// e.g. commandline:
//    apsimreport test.report wmf farm1.out farm2.out ...
// all are optional.
//---------------------------------------------------------------------------
void TMainForm::processCommandLine(AnsiString commandLine)
   {
   vector<string> commandWords;
   SplitStringHonouringQuotes(commandLine.c_str(), " ", commandWords);
   if (commandWords.size() >= 1)
      {
      string reportFileName = commandWords[0];
      stripLeadingTrailing(reportFileName, "\"");
      open(reportFileName.c_str(), true, false);
      if (commandWords.size() >= 2)
         {
         string outputFileName = commandWords[1];
         replaceAll(outputFileName, "\"", "");

         for (unsigned i = 2; i != commandWords.size(); i++)
            {
            replaceAll(commandWords[i], "\"", "");
            unsigned posPeriod = commandWords[i].find('.');
            if (posPeriod != string::npos)
               {
               string objectName = commandWords[i].substr(0, posPeriod);
               string propertyLine = commandWords[i].substr(posPeriod+1);
               string propertyName, propertyValue;
               unsigned posEquals = propertyLine.find('=');
               if (posEquals != string::npos)
                  {
                  propertyName = propertyLine.substr(0, posEquals);
                  propertyValue = propertyLine.substr(posEquals+1);
                  //report.setProperty(objectName, propertyName, propertyValue);
                  }
               }
            }
         Path(reportFileName).Change_directory();
         save(outputFileName.c_str());
         report.clear();
         Close();
         }
      }
   }

//---------------------------------------------------------------------------
// User has clicked add menu item
//---------------------------------------------------------------------------
void __fastcall TMainForm::addMenuItemClick(TObject* sender)
   {
   AnsiString newPageName = report.createPage();
   TabControl->Tabs->Add(newPageName);
   TabControl->TabIndex = TabControl->Tabs->Count-1;
   }
//---------------------------------------------------------------------------
// User has clicked delete menu item
//---------------------------------------------------------------------------
void __fastcall TMainForm::deleteMenuItemClick(TObject* sender)
   {
   if (TabControl->Tabs->Count > 1 &&
       Application->MessageBox("Are you sure you want to delete this page",
                               "Question", MB_ICONQUESTION | MB_YESNO) == IDYES)
      {
      report.deletePage(TabControl->TabIndex);
      TabControl->Tabs->Delete(TabControl->TabIndex);
      }
   }
//---------------------------------------------------------------------------
// User has clicked rename menu item
//---------------------------------------------------------------------------
void __fastcall TMainForm::renameMenuItemClick(TObject* sender)
   {
   int currentTab = TabControl->TabIndex;
   AnsiString pageName = TabControl->Tabs->Strings[currentTab];
   if (InputQuery("Rename page", "Enter new page name", pageName))
      {
      report.renamePage(TabControl->Tabs->Strings[currentTab], pageName);
      TabControl->Tabs->Strings[currentTab] = pageName;
      }
   }
//---------------------------------------------------------------------------
// given a page control and an x/y coordinate - returns the tab index.
//---------------------------------------------------------------------------
int PCDetectTab(TTabControl* tabControl, int x, int y)
   {
   TTCHitTestInfo hitInfo;
   hitInfo.pt.x = x;
   hitInfo.pt.y = y;
   hitInfo.flags = 0;
   return tabControl->Perform(TCM_HITTEST, 0, (int)&hitInfo);
   }
//---------------------------------------------------------------------------
// User has begun to drag something - if it is a tab then allow the drag
// to continue.
//---------------------------------------------------------------------------
void __fastcall TMainForm::onMouseDown(TObject* sender, TMouseButton button,
                                       TShiftState state, int x, int y)
   {
   int tab = PCDetectTab(TabControl, x, y);
   if (tab >= 0)
      {
      draggedTab = tab;
      TabControl->BeginDrag(true);
      }
   }
//---------------------------------------------------------------------------
// User has begun to drag something - if it is a tab then allow the drag
// to continue.
//---------------------------------------------------------------------------
void __fastcall TMainForm::onDragOver(TObject* sender, TObject* source,
                                      int x, int y, TDragState state,
                                      bool &accept)
   {
   int tab = PCDetectTab(TabControl, x, y);
   accept = (source == sender && tab >= 0 && tab != draggedTab);

   static int lastTab = -1;
   static int lastLeft;
   if (accept && tab != lastTab)
      {
      TRect r;
      SendMessage(TabControl->Handle, TCM_GETITEMRECT, tab, (int) &r);
      int aleft;
      if (tab > draggedTab)
         aleft = r.right - 3;
      else
         aleft = r.left + 3;
      TabControl->Canvas->Pen->Mode = pmNotXor;
      TabControl->Canvas->Pen->Width = 4;
      TabControl->Canvas->Pen->Color = clRed;
      if (lastTab >= 0)
         {
         TabControl->Canvas->MoveTo(lastLeft, r.top+2);
         TabControl->Canvas->LineTo(lastLeft, r.bottom-1);
         }
      lastLeft = aleft;
      TabControl->Canvas->MoveTo(aleft, r.top+2);
      TabControl->Canvas->LineTo(aleft, r.bottom-1);
      lastTab = tab;
      }
   }
//---------------------------------------------------------------------------
// User has dropped a tab.
//---------------------------------------------------------------------------
void __fastcall TMainForm::onDragDrop(TObject* sender, TObject* source,
                                      int x, int y)
   {
   int tab = PCDetectTab(TabControl, x, y);
   if (tab >= 0)
      {
      TabControl->Tabs->Move(draggedTab, tab);
      report.movePage(draggedTab, tab);
      }
   }

//---------------------------------------------------------------------------
// User has clicked the page button - Show user the page menu.
//---------------------------------------------------------------------------
void __fastcall TMainForm::PageActionExecute(TObject *Sender)
   {
   TPoint Pos = ButtonBar->ClientToScreen(TPoint(PageButton->Left, PageButton->Top + PageButton->Height));
   PagePopupMenu->Popup(Pos.x, Pos.y);

   }


