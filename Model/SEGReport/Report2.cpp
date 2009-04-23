//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Report2.h"
#include "ComponentRegistration.h"

#include <QuickRpt.hpp>
#include <Generalvcl\vcl_functions.h>
#include <General\stl_functions.h>
#include <General\path.h>
#include <General\inifile.h>
#include <General\stringtokenizer.h>
#include <General\xml.h>
#include <General\exec.h>
#include <ApsimShared\ApsimDirectories.h>
#include <dcfdes.hpp>
#include <qrctrls.hpp>
#include <gtQrRtns.hpp>
#include "TText.h"
#include "TGraph.h"

#pragma package(smart_init)
#pragma link "dcfdes"
#pragma link "gtQRXport"
#pragma link "gtQRXport_Doc"
#pragma link "gtQRXport_HTML"
#pragma link "gtQRXport_JPEG"
#pragma link "kbmMemTable"
#pragma link "jpeg"
using namespace std;
extern HINSTANCE instanceHandle;
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Report::Report(TWinControl* p)
   : parent(p)
   {
   currentPage = NULL;
   uiForm = NULL;
   OnObjectInspectorUpdate = NULL;
   data = NULL;
   handle = NULL;

   reportForm = new TForm(parent);
   reportForm->Parent = parent;
   reportForm->Name = "report";
   reportForm->Align = alClient;
   reportForm->BorderIcons = TBorderIcons();
   reportForm->BorderStyle = bsNone;
   reportForm->Caption = "";
   reportForm->Visible = true;
   reportForm->OnResize = onResize;
   scrollBox = new TScrollBox(reportForm);
   scrollBox->Parent = reportForm;
   scrollBox->Align = alClient;
   clear();

   compositeReport = new TQRCompositeReport(reportForm);
   compositeReport->OnAddReports = OnAddReports;

   isEditing = false;
   isDirty = false;
   zoomToFit = false;

   buttonImages = new TImageList(parent);
   buttonImages->Width = 24;
   buttonImages->Height = 24;
   buttonImages->Masked = true;

   RegisterComponents();

   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
Report::~Report(void)
   {
   if (handle != NULL)
        FreeLibrary(handle);
   if (isEditing)
      edit(false);
  for (unsigned i = 0; i != pages.size(); i++)
      delete pages[i];
   pages.erase(pages.begin(), pages.end());

   delete buttonImages;
   delete data;
   }
//---------------------------------------------------------------------------
// Clear the report and create a new one.
//---------------------------------------------------------------------------
void Report::clear(void)
   {
   const string reportTemplate =
      "<report version=\"5\">\r\n"
      "   <data/>\r\n"
      "   <page><![CDATA[\r\n"
      "     object Page1: TQuickRep\r\n"
      "       Left = 241\r\n"
      "       Top = 0\r\n"
      "       Width = 794\r\n"
      "       Height = 1123\r\n"
      "       Frame.Color = clBlack\r\n"
      "       Frame.DrawTop = False\r\n"
      "       Frame.DrawBottom = False\r\n"
      "       Frame.DrawLeft = False\r\n"
      "       Frame.DrawRight = False\r\n"
      "       Frame.Style = psClear\r\n"
      "       Font.Charset = DEFAULT_CHARSET\r\n"
      "       Font.Color = clWindowText\r\n"
      "       Font.Height = -17\r\n"
      "       Font.Name = 'Arial'\r\n"
      "       Font.Style = []\r\n"
      "       Functions.Strings = (\r\n"
      "        'PAGENUMBER'\r\n"
      "        'COLUMNNUMBER'\r\n"
      "        'REPORTTITLE')\r\n"
      "       Functions.DATA = (\r\n"
      "        '0'\r\n"
      "        '0'\r\n"
      "        #39#39)\r\n"
      "       Options = [FirstPageHeader, LastPageFooter]\r\n"
      "       Page.Columns = 1\r\n"
      "       Page.Orientation = poPortrait\r\n"
      "       Page.PaperSize = A4\r\n"
      "       Page.Ruler = False\r\n"
      "       Page.Values = (\r\n"
      "        0\r\n"
      "        2970\r\n"
      "        0\r\n"
      "        2100\r\n"
      "        0\r\n"
      "        0\r\n"
      "        0)\r\n"
      "       PrinterSettings.Copies = 1\r\n"
      "       PrinterSettings.Duplex = False\r\n"
      "       PrinterSettings.FirstPage = 0\r\n"
      "       PrinterSettings.LastPage = 0\r\n"
      "       PrinterSettings.OutputBin = Auto\r\n"
      "       PrintIfEmpty = True\r\n"
      "       SnapToGrid = True\r\n"
      "       Units = MM\r\n"
      "       Zoom = 100\r\n"
      "       object TitleBand1: TQRBand\r\n"
      "         Left = 0\r\n"
      "         Top = 0\r\n"
      "         Width = 794\r\n"
      "         Height = 1123\r\n"
      "         Frame.Color = clBlack\r\n"
      "         Frame.DrawTop = False\r\n"
      "         Frame.DrawBottom = False\r\n"
      "         Frame.DrawLeft = False\r\n"
      "         Frame.DrawRight = False\r\n"
      "         AlignToBottom = False\r\n"
      "         Color = clWhite\r\n"
      "         ForceNewColumn = False\r\n"
      "         ForceNewPage = False\r\n"
      "         Size.Values = (\r\n"
      "           2970\r\n"
      "           2100.79166666667)\r\n"
      "         BandType = rbTitle\r\n"
      "       end\r\n"
      "     end\r\n]]>"
      "   </page>\r\n"
      "</report>";

   loadFromContents(reportTemplate, false);
   isDirty = false;
   }
//---------------------------------------------------------------------------
// If the filename exists then load it into the report.
//---------------------------------------------------------------------------
void Report::load(const string& fileName, bool quiet, bool safe)
   {
   if (FileExists(fileName.c_str()))
      {
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      SetCurrentDir(ExtractFileDir(fileName.c_str()));
      edit(false);

      ifstream in(fileName.c_str());
      string versionLine;
      getline(in, versionLine);
      if (versionLine.find('<') == string::npos)
         {
         convertVersion3To4(in, fileName);
         in.clear();
         in.open(fileName.c_str());
         getline(in, versionLine);
         }
      if (versionLine.substr(0, 2) == "<?")
         getline(in, versionLine);
      unsigned posVersion = versionLine.find("version=\"");
      if (posVersion != string::npos)
         {
         posVersion += strlen("version=\"");
         unsigned posQuote = versionLine.find('"', posVersion);
         if (posQuote != string::npos)
            {
            string versionString = versionLine.substr(posVersion, posQuote - posVersion);
            int version = atoi(versionString.c_str());
            if (version < 5)
               convertVersion4To5(in, fileName);
            }
         }
      in.close();

      // if use is holding down the CTRL key then open in 'safe' mode.
      if (safe)
         safeMode(fileName);

      // set up the data.
      in.open(fileName.c_str());
      ostringstream contents;
      contents << in.rdbuf();
      in.close();
      loadFromContents(contents.str(), quiet);
      refreshAllPages();
      Screen->Cursor = savedCursor;
      }
   }
//---------------------------------------------------------------------------
// Given the specified xml contents - set ourselves up.
//---------------------------------------------------------------------------
void Report::loadFromContents(const string& contents, bool quiet)
   {
   currentPage = NULL;
   for (unsigned i = 0; i != pages.size(); i++)
      delete pages[i];
   pages.erase(pages.begin(), pages.end());

   // set up the pages.
   XMLDocument doc(contents, XMLDocument::xmlContents);
   for (XMLNode::iterator i = doc.documentElement().begin();
                          i != doc.documentElement().end();
                          i++)
      {
      try
         {
         if (Str_i_Eq(i->getName(), "data"))
            {
            delete data;
            data = new DataContainer(reportForm);
            data->setup(i->write());
            refreshIfNecessary(quiet);
            }
         else if (Str_i_Eq(i->getName(), "page"))
            {
            TQuickRep* newPage = new TQuickRep(reportForm);
            pages.push_back(newPage);
            string pageContents = i->getValue();
            checkPageContents(pageContents);
            istringstream pageStream (pageContents);
            if (pageContents != "")
               loadComponent(pageStream, newPage);
            newPage->Visible = false;
            newPage->Parent = NULL;
            }
         }
      catch (exception& err)
         {
         if (!quiet)
            ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
         }
      catch (Exception* err)
         {
         if (!quiet)
            ::MessageBox(NULL, err->Message.c_str(), "Error", MB_ICONSTOP | MB_OK);
         }
      }

   showPage(0);

   isDirty = false;
   }

//---------------------------------------------------------------------------
// Save the report to the specified filename.
//---------------------------------------------------------------------------
void Report::save(const std::string& fileName)
   {
   if (ExtractFileExt(fileName.c_str()) == ".report")
      {
      ofstream out(fileName.c_str());
      if (out.is_open())
         {
         out << getReportXml();
         out.close();
         isDirty = false;
         }
      else
         MessageBox(NULL, "Cannot save file. Is file readonly?", "Error", MB_ICONSTOP | MB_OK);
      }
   else
      exportCurrentToFile(fileName);
   }

void __fastcall Report::OnAddReports(TObject* sender)
   {
   compositeReport->Reports->Clear();
   for (unsigned page = 0; page != pages.size(); page++)
      compositeReport->Reports->Add(pages[page]);
   }
//---------------------------------------------------------------------------
// write the full report xml to the specified stream.
//---------------------------------------------------------------------------
string Report::getReportXml()
   {
   ostringstream out;
   out << "<report version=\"5\">" << endl;
   out << data->xml() << endl;
   for (unsigned p = 0; p != pages.size(); p++)
      {
      out << "   <page> <![CDATA[" << endl;
      saveComponent(out, pages[p]);
      out << "]]>" << endl;
      out << "   </page>" << endl;
      }
   out << "</report>" << endl;
   string contents = out.str();
   replaceAll(contents, "\r", "");
   return contents;
   }

//---------------------------------------------------------------------------
// Edit the current page.
//---------------------------------------------------------------------------
TForm* Report::edit(bool turnOn)
   {
   static TDCLiteDesigner* formDesigner = NULL;

   if (turnOn && currentPage != NULL)
      {
      formDesigner = new TDCLiteDesigner(NULL);
      formDesigner->OnSelectionChanged = onDesignerSelectionChanged;
      formDesigner->DesignedComponent = scrollBox;
      formDesigner->LimitControl = scrollBox;
      formDesigner->ShowComponents = false;
      formDesigner->ShowPalette = true;
      formDesigner->ShowCaptions = false;
      formDesigner->Active = true;
      GetPalForm()->AutoSize = false;
      GetPalForm()->BorderIcons = TBorderIcons();
      GetPalForm()->BorderStyle = bsSizeToolWin;
      GetPalForm()->Caption = "";
      return dynamic_cast<TForm*> (GetPalForm());
      }
   else if (!turnOn && formDesigner != NULL)
      {
      isDirty = formDesigner->Designer->WasChanged;
      delete formDesigner;
      formDesigner = NULL;
      }
   return NULL;
   }
//---------------------------------------------------------------------------
// Set the object inspector form to use.
//---------------------------------------------------------------------------
void Report::setObjectInspector(TWinControl* objectinspector, TDataSource* dataInspector)
   {
   objectInspector = objectinspector;
   dataInspectorSource = dataInspector;
   }
//---------------------------------------------------------------------------
// The designer has changed selection - get current object and call
// selectionChangedEvent.
//---------------------------------------------------------------------------
void __fastcall Report::onDesignerSelectionChanged(TObject* sender)
   {
   TDCLiteDesigner* formDesigner = dynamic_cast<TDCLiteDesigner*>(sender);

   // get the currently selected component.
   if (formDesigner->Designer->SelectedComponents->Count == 1)
      {
      TComponent* selectedComponent
         = (TComponent*) formDesigner->Designer->SelectedComponents->Items[0];
      updateObjectInspector(selectedComponent);
      }
   else
      updateObjectInspector(NULL);
   }
//---------------------------------------------------------------------------
// update the objectInspector for the specified component.
//---------------------------------------------------------------------------
void Report::updateObjectInspector(TComponent* component)
   {
   delete uiForm;
   uiForm = NULL;

   if (component != NULL)
      {
      // get a property form.
      uiForm = createComponentUI(component, objectInspector, true);

      // If an addin returned a form then make that form a child of the parent
      // form.
      if (uiForm != NULL)
         {
         uiForm->BorderStyle = bsNone;
         uiForm->Parent = objectInspector;
         uiForm->Show();
         uiForm->Align = alClient;
         }
      isDirty = true;
      }
   }
//---------------------------------------------------------------------------
// Create a page on the report.
//---------------------------------------------------------------------------
AnsiString Report::createPage(void)
   {
   unsigned pageNumber = 1;
   while (getComponent<TQuickRep>(reportForm, "Page" + IntToStr(pageNumber)) != NULL)
      pageNumber++;

   AnsiString pageName = "Page" + IntToStr(pageNumber);
   if (pages.size() != 0)
      isDirty = true;

   TQuickRep* newPage = new TQuickRep(reportForm);
   newPage->Parent = scrollBox;
   newPage->Name = pageName;
   newPage->Units = MM;
   newPage->Page->Ruler = false;
   newPage->Frame->Style = psClear;
   newPage->Page->TopMargin = 0;
   newPage->Page->LeftMargin = 0;
   newPage->Page->RightMargin = 0;
   newPage->Page->BottomMargin = 0;
   newPage->Enabled = false;
   newPage->Zoom = 50;

   newPage->CreateBand(rbTitle);

   pages.push_back(newPage);
   showPage(pages.size()-1);
   return pageName;
   }
//---------------------------------------------------------------------------
// Get a page name for a particular page.
//---------------------------------------------------------------------------
AnsiString getPageName(TQuickRep* page)
   {
   return page->Name;
   }
//---------------------------------------------------------------------------
// Return a list of page names.
//---------------------------------------------------------------------------
void Report::getPageNames(TStrings* pageNames)
   {
   pageNames->Clear();
   for (unsigned p = 0; p != pages.size(); p++)
      pageNames->Add(getPageName(pages[p]));
   }
//---------------------------------------------------------------------------
// Show a page on the parent form.
//---------------------------------------------------------------------------
void Report::showPage(unsigned pageNumber)
   {
   if (pageNumber < pages.size())
      {
      if (currentPage != NULL)
         {
         currentPage->Visible = false;
         currentPage->Parent = NULL;
         }
      currentPage = pages[pageNumber];
      currentPage->Parent = scrollBox;
      currentPage->Visible = true;
      centrePage();
      }
   }

//---------------------------------------------------------------------------
// Show the data page.
//---------------------------------------------------------------------------
void Report::showDataPage()
   {
   ostringstream argument;
   argument << (unsigned) data;

   string dllFileName = getApsimDirectory() + "\\model\\GraphUserInterface.dll";

   if (handle == NULL)
        {
        string callManagedDLL = getApsimDirectory() + "\\model\\" + "CallManagedDLL.dll";
        handle = LoadLibrary(callManagedDLL.c_str());
        (FARPROC) callDLL = GetProcAddress(handle, "CallDLL");
        }
   if (handle == NULL)
        ::MessageBox(NULL, "Cannot find CallManagedDLL.dll", "Error", MB_ICONSTOP | MB_OK);
   else
        {
        (*callDLL)(dllFileName.c_str(), "GraphUserInterface.ApsimReportDataForm", "Go", argument.str().c_str());
        refreshAllPages();
        isDirty = true;
        }
   }
//---------------------------------------------------------------------------
// Rename a page.
//---------------------------------------------------------------------------
void Report::renamePage(AnsiString oldName, AnsiString newName)
   {
   for (unsigned p = 0; p != pages.size(); p++)
      {
      if (oldName.AnsiCompareIC(getPageName(pages[p])) == 0)
         pages[p]->Name = newName;
      isDirty = true;
      }
   }
//---------------------------------------------------------------------------
// Delete a page.
//---------------------------------------------------------------------------
void Report::deletePage(unsigned pageIndex)
   {
   if (pageIndex < pages.size())
      {
      delete pages[pageIndex];
      pages.erase(pages.begin()+pageIndex);
      currentPage = NULL;
      showPage(0);
      isDirty = true;
      }
   }
//---------------------------------------------------------------------------
// Move the specified page (oldPageIndex) to before insertBeforePageIndex
//---------------------------------------------------------------------------
void Report::movePage(unsigned oldPageIndex, unsigned insertBeforePageIndex)
   {
   TQuickRep* pageToMove = pages[oldPageIndex];
   if (oldPageIndex < insertBeforePageIndex)
      {
      // move down.
      insertBeforePageIndex--;
      }
   pages.erase(pages.begin() + oldPageIndex);
   pages.insert(pages.begin() + insertBeforePageIndex, pageToMove);
   isDirty = true;
   }

//---------------------------------------------------------------------------
// Centre the current page within the parent container
//---------------------------------------------------------------------------
void Report::centrePage(void)
   {
   if (currentPage != NULL)
      {
      if (zoomToFit)
         {
         scrollBox->AutoScroll = false;
         currentPage->Zoom *= 1.0 * parent->Width / currentPage->Width;
         }

      int left = max((scrollBox->Width - currentPage->Width) / 2, 0);
      int top = max((scrollBox->Height - currentPage->Height) / 2, 0);
      currentPage->Left = max(left, 0);
      currentPage->Top  = max(top, 0);

      // position the title band.

      TQRBand* titleBand = getControlOfType<TQRBand>(currentPage);
      titleBand->Size->Height = currentPage->Page->Length
                              - currentPage->Page->TopMargin
                              - currentPage->Page->BottomMargin;
      titleBand->Size->Width = currentPage->Page->Width
                             - currentPage->Page->LeftMargin
                             - currentPage->Page->RightMargin;
//      ShowMessage((int)titleBand->Size->Height);
//      ShowMessage((int)titleBand->Size->Width);
      }
   }
//---------------------------------------------------------------------------
// The report has been resized - reposition ourselves in the middle
// of the form.
//---------------------------------------------------------------------------
void __fastcall Report::onResize(TObject* sender)
   {
   centrePage();
   }

//---------------------------------------------------------------------------
// return zoom
//---------------------------------------------------------------------------
int Report::getZoom(void)
   {
   if (currentPage != NULL)
      return currentPage->Zoom;
   return 50;
   }
//---------------------------------------------------------------------------
// Set the zoom level
//---------------------------------------------------------------------------
void Report::setZoom(int zoom)
   {
   if (currentPage != NULL && currentPage->Zoom != zoom)
      {
      currentPage->Zoom = zoom;
      centrePage();
      isDirty = true;
      }
   }
//---------------------------------------------------------------------------
// return true if the report is in portrait mode.
//---------------------------------------------------------------------------
bool Report::getIsPortrait(void)
   {
   if (currentPage != NULL)
      return (currentPage->Page->Orientation == poPortrait);
   return true;
   }

//---------------------------------------------------------------------------
// set the reports orientation.
//---------------------------------------------------------------------------
void Report::setIsPortrait(bool isPortrait)
   {
   if (currentPage != NULL)
      {
      if (isPortrait)
         currentPage->Page->Orientation = poPortrait;
      else
         currentPage->Page->Orientation = poLandscape;
      centrePage();
      isDirty = true;
      }
   }
//---------------------------------------------------------------------------
// Copy the report to the clipboard.
//---------------------------------------------------------------------------
void Report::copyToClipboard(void)
   {
   // save a bmp version of current page to clipboard.
   string tmpFile = Path::getTempFolder().Get_path();
   tmpFile += "\\clipboard.bmp";
   exportCurrentToFile(tmpFile);
   Graphics::TBitmap* bitmap = new Graphics::TBitmap;
   bitmap->LoadFromFile(tmpFile.c_str());
   unsigned short format;
   unsigned int dataHandle;
   HPALETTE palette;
   bitmap->SaveToClipboardFormat(format, dataHandle, palette);
   Clipboard()->SetAsHandle(format, dataHandle);
   delete bitmap;
   DeleteFile(tmpFile.c_str());

   // save an emf version of current page to clipboard.
   tmpFile = Path::getTempFolder().Get_path();
   tmpFile += "\\clipboard.emf";
   exportCurrentToFile(tmpFile);
   Graphics::TMetafile* metafile = new Graphics::TMetafile;
   metafile->LoadFromFile(tmpFile.c_str());
   metafile->SaveToClipboardFormat(format, dataHandle, palette);
   Clipboard()->SetAsHandle(format, dataHandle);
   delete metafile;
   DeleteFile(tmpFile.c_str());
   }
//---------------------------------------------------------------------------
// Print the report.
//---------------------------------------------------------------------------
void Report::print(bool currentPageOnly)
   {
   compositeReport->PrinterSetup();
   if (compositeReport->Tag == 0)
      currentPage->Print();
   }
//---------------------------------------------------------------------------
// Refresh the report
//---------------------------------------------------------------------------
void Report::refresh(bool quiet)
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;
   try
      {
      data->refresh();
      refreshAllPages();
      }
   catch (Exception* err)
      {
      MessageBox(NULL, err->Message.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   catch (exception& err)
      {
      MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }

   Screen->Cursor = savedCursor;
   }

//---------------------------------------------------------------------------
// Refresh the report
//---------------------------------------------------------------------------
void Report::refreshIfNecessary(bool quiet)
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;
   try
      {
      data->refreshIfNecessary();
      refreshAllPages();
      }
   catch (Exception* err)
      {
      MessageBox(NULL, err->Message.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   catch (exception& err)
      {
      MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }

   Screen->Cursor = savedCursor;
   }

//---------------------------------------------------------------------------
// now refresh our gui components on all pages.
//---------------------------------------------------------------------------
void Report::refreshAllPages()
   {
   for (unsigned p = 0; p != pages.size(); p++)
      refreshControls(pages[p]);
   }
//---------------------------------------------------------------------------
// now refresh our gui components.
//---------------------------------------------------------------------------
void Report::refreshControls(TWinControl* control)
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;
   try
      {
      if (control != NULL)
         {
         TText* text = dynamic_cast<TText*> (control);
         if (text != NULL)
            text->refresh();

         TGraph* graph = dynamic_cast<TGraph*> (control);
         if (graph != NULL)
            graph->refresh();

         for (int i = 0; i != control->ControlCount; i++)
            refreshControls(dynamic_cast<TWinControl*> (control->Controls[i]));
         }
      }
   catch (Exception& err)
      {
      MessageBox(NULL, err.Message.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   Screen->Cursor = savedCursor;
   }
//---------------------------------------------------------------------------
// Export all pages to the specified file.
//---------------------------------------------------------------------------
void Report::exportCurrentToFile(const std::string& fileNameBase)
   {
   // make sure the directory exists.
   if (ExtractFileDir(fileNameBase.c_str()) != "")
      ForceDirectories(ExtractFileDir(fileNameBase.c_str()));

   for (vector<Quickrpt::TQuickRep*>::iterator page = pages.begin(); page != pages.end(); page++)
      (*page)->Zoom = 100;
   if (ExtractFileExt(fileNameBase.c_str()) == ".pdf")
      {
      OnAddReports(NULL);
      CompositeExportToFile(compositeReport, etPDF, fileNameBase.c_str(), false, false);
      }
   else if (ExtractFileExt(fileNameBase.c_str()) == ".rtf")
      {
      OnAddReports(NULL);
      CompositeExportToFile(compositeReport, etRTF, fileNameBase.c_str(), false, false);
      }
   else
      {
      for (vector<Quickrpt::TQuickRep*>::iterator page = pages.begin(); page != pages.end(); page++)
         {
         string fileName = fileNameBase;
         if (pages.size() > 1)
            {
            Path p(fileName);
            fileName = p.Get_name_without_ext();
            if (p.Get_directory() != "")
               fileName = p.Get_directory() + "\\" + fileName;
            int pageNumber = page - pages.begin() + 1;
            fileName += "[page" + itoa(pageNumber) + "]";
            fileName += p.Get_extension();
            }
         try
            {
            if (ExtractFileExt(fileName.c_str()) == ".bmp")
               ExportToBMP(*page, fileName.c_str(), false, true);
            else if (ExtractFileExt(fileName.c_str()) == ".jpg")
               {
               gtQRJPEGSettings->PixelFormat = pf8bit;
               ExportToJPEG(*page, fileName.c_str(), false, false);
               }
            else if (ExtractFileExt(fileName.c_str()) == ".html")
               {
               gtQRHTMLSettings->ExportImageFormat = ifGIF;
               ExportToHTML(*page, fileName.c_str(), false, false);
               }
            else if (ExtractFileExt(fileName.c_str()) == ".gif")
               {
               gtQRGIFSettings->PixelFormat = pf24bit;
               ExportToGIF(*page, fileName.c_str(), false, true);
               }
            else if (ExtractFileExt(fileName.c_str()) == ".wmf")
               ExportToWMF(*page, fileName.c_str(), false, false);
            else if (ExtractFileExt(fileName.c_str()) == ".emf")
               ExportToEMF(*page, fileName.c_str(), false, false);

            AnsiString tempFilePath = ExtractFileDir(fileName.c_str());
            if (tempFilePath != "")
               tempFilePath += "\\";
            AnsiString tempFile =  tempFilePath
                                + Path(fileName).Get_name_without_ext().c_str()
                                + "0001"
                                + ExtractFileExt(fileName.c_str());
            CopyFile(tempFile.c_str(), fileName.c_str(), false);
            DeleteFile(tempFile);

            // html exports do things this way!!!!!???
            tempFilePath = ExtractFileDir(fileName.c_str());
            if (tempFilePath != "")
               tempFilePath += "\\";
            tempFile =  tempFilePath
                                + Path(fileName).Get_name_without_ext().c_str()
                                + ".0001.htm";
            CopyFile(tempFile.c_str(), fileName.c_str(), false);
            DeleteFile(tempFile);
            }
         catch (const Exception& err)
            {
            ShowMessage(err.Message);
            }
         }
      }
   }
//---------------------------------------------------------------------------
// Add a button to the specified toolbar.
//---------------------------------------------------------------------------
void Report::addButtonToToolBar(TComponent* component, TToolBar* toolbar)
   {
   // Add bitmap to imagelist.
   Graphics::TBitmap* bitmap = new Graphics::TBitmap;
   getComponentBitmap(component->ClassName(), bitmap);
   if (!bitmap->Empty)
      {
      buttonImages->AddMasked(bitmap, bitmap->TransparentColor);
      delete bitmap;

      // add action to actionlist
      AnsiString name = component->Name;

      int buttonNum = buttonImages->Count;
      TToolButton *button = new TToolButton(toolbar);
      button->Parent = toolbar;
      button->Caption = name;
      button->Style = tbsButton;
      button->Hint = name;
      button->Tag = (int)component;
      button->ImageIndex = buttonImages->Count-1;
      button->OnClick = buttonClick;

      // make sure the button is at the far right of the toolbar.
      button->Left = (buttonNum-1) * toolbar->ButtonWidth + 2;
      toolbar->Width = button->Left + toolbar->ButtonWidth + 2;
      }
   }
//---------------------------------------------------------------------------
// loop through all add-in's looking for a resource for the specified
// component name.  Return true if found.
//---------------------------------------------------------------------------
bool Report::getComponentBitmap(AnsiString className, Graphics::TBitmap* bitmap)
   {
   if (className.LowerCase().Pos("series") == 0)
      {
      // try loading the resource with same name as class.
      AnsiString resourceName = UpperCase(className);
      try
         {
         bitmap->LoadFromResourceName((unsigned)instanceHandle, resourceName);
         return true;
         }
      catch (const Exception& error)
         {}
      }
   return false;
   }
//---------------------------------------------------------------------------
// User has clicked a button - go display object inspector.
//---------------------------------------------------------------------------
void __fastcall Report::buttonClick(TObject* sender)
   {
   TToolButton* button = dynamic_cast<TToolButton*>(sender);
   updateObjectInspector((TComponent*)button->Tag);
   }

//---------------------------------------------------------------------------
// Look through the page contents at all x/y sources for all charts.
// If they don't exist, then remove them.
//---------------------------------------------------------------------------
void Report::checkPageContents(string& pageContents)
   {
   unsigned posValueSource = pageContents.find(".ValueSource = ");
   while (posValueSource != string::npos)
      {
      unsigned posStartObject = pageContents.rfind("object ", posValueSource);
      unsigned posDataSource = pageContents.rfind("DataSource = ", posValueSource);
      if (posDataSource > posStartObject)
         {
         // check to make sure it is valid.

         posValueSource += strlen(".ValueSource = ");
         unsigned posValueSourceEoln = pageContents.find("\n", posValueSource);
         string valueSource = pageContents.substr(posValueSource, posValueSourceEoln - posValueSource);
         replaceAll(valueSource, "'", "");

         posDataSource += strlen("DataSource = ");
         unsigned posDataSourceEoln = pageContents.find("\n", posDataSource);
         string dataSource = pageContents.substr(posDataSource, posDataSourceEoln - posDataSource);

         if (reportForm->FindComponent(dataSource.c_str()) == NULL)
            {
//            string msg = "Cannot find data source: " + dataSource + ". " +
//                         "Removing all chart references to it.";
//            MessageBox(NULL, msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
//            replaceAll(pageContents, "DataSource = " + dataSource, "DataSource = ");
            }
         else
            {
            TDataSet* data = dynamic_cast<TDataSet*> (reportForm->FindComponent(dataSource.c_str()));
            if (data != NULL && data->Active && data->FieldDefs->IndexOf(valueSource.c_str()) == -1)
               {
//               string msg = "Cannot find column: " + valueSource +
//                            " in data source: " + dataSource + ". " +
//                            "Removing all chart references to it.";
//               MessageBox(NULL, msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
               unsigned posStartLine = pageContents.rfind("\n", posValueSource) + 1;
               pageContents.erase(posStartLine, posValueSourceEoln - posStartLine);
               }
            }
         }
      posValueSource = pageContents.find(".ValueSource = '", posValueSource+1);
      }
   }
//---------------------------------------------------------------------------
// Safe mode is where we go through the .report file and remove all
// .valuesource = lines. TChart gets quite upset when trying to load
// a chart that has x and y value sources pointing to non-existant
// table fields.
//---------------------------------------------------------------------------
void Report::safeMode(const std::string& fileName)
   {
   string tempFileName = Path::getTempFolder().Get_path() + "\\apsimreport.tmp";
   ifstream in(fileName.c_str());
   ofstream out(tempFileName.c_str());
   string line;
   while (getline(in, line))
      {
      if (line.find(".ValueSource = '") == string::npos)
         out << line << endl;
      }
   in.close();
   out.close();
   CopyFile(tempFileName.c_str(), fileName.c_str(), false);
   unlink(tempFileName.c_str());
   }
//---------------------------------------------------------------------------
// Version 3 to 4
//---------------------------------------------------------------------------
void Report::convertVersion3To4(ifstream& in, const std::string& fileName)
   {
   in.ignore(1000, '\n');   // skip past the numpages.
   in.ignore(1000, '\n');   // skip past: object data: TForm

   XMLDocument doc("report", XMLDocument::rootName);
   doc.documentElement().setAttribute("version", "4");
   bool end = false;
   string line;
   while (!end && getline(in, line))
      {
      StringTokenizer tokenizer(line, " :");
      string firstWord = tokenizer.nextToken();
      if (firstWord == "object")
         {
         string objectName = tokenizer.nextToken();
         string objectType = tokenizer.nextToken();
         addObjectToXML(in, objectName, objectType, doc);
         }
      else if (firstWord == "end")
         end = true;
      }
   nestAllObjectsUsingSource(doc);

   // now go through each page and write to xml doc.
   string pageContents;
   while (getline(in, line))
      {
      pageContents += line + "\n";
      if (line == "end")
         {
         XMLNode page = doc.documentElement().appendChild("page", true);
         replaceAll(pageContents, "DataSource = Data.", "DataSource = ");
         page.setValueAsCData(pageContents);
         pageContents = "";
         }
      }
   AnsiString newFileName = ExtractFileName(fileName.c_str());
   in.close();
   RenameFile(fileName.c_str(), ChangeFileExt(newFileName, ".bak"));
   doc.write(fileName);
   }

//---------------------------------------------------------------------------
// Add multiple elements to specified 'elements' and 'values' vectors
// from BORLAND lines that look like:
//    filenames.Strings = (
//      'scenario1Yearly.out')
//---------------------------------------------------------------------------
void addMultipleElements(istream& in, const string& elementName,
                         const string& elementType,
                         vector<string>& elements,
                         vector<string>& types,
                         vector<string>& values)
   {
   string fullLine;
   string line;
   bool endOfMultipleElements = false;
   do
      {
      getline(in, line);
      bool lineContinuation = (line.find("' +") != string::npos);
      endOfMultipleElements = (line.find("')") != string::npos);

      replaceAll(line, "' +", "");
      replaceAll(line, "'", "");
      replaceAll(line, ")", "");
      stripLeadingTrailing(line, " ");
      if (line != "")
         {
         fullLine += line;

         if (!lineContinuation)
            {
            replaceAll(fullLine, "#39", "'");
            elements.push_back(elementName);
            types.push_back(elementType);
            values.push_back(fullLine);
            fullLine = "";
            }
         }
      }
   while (!endOfMultipleElements);
   }
//---------------------------------------------------------------------------
// Add an element to specified 'elements' and 'values' vectors
// from BORLAND lines that look like:
//    filter = 'xxxx'
//---------------------------------------------------------------------------
void addElement(const string& elementName, const string& elementType,
                const string& value,
                vector<string>& elements,
                vector<string>& types,
                vector<string>& values)
   {
   elements.push_back(elementName);
   types.push_back(elementType);
   string rawValue = value;
   replaceAll(rawValue, "'", "");
   replaceAll(rawValue, "#39", "'");
   values.push_back(rawValue);
   }
// ------------------------------------------------------------------
// Go find an object somewhere in the tree with the specified name.
// ------------------------------------------------------------------
XMLNode findObject(XMLNode parent, const std::string& name)
   {
   for (XMLNode::iterator child = parent.begin();
                          child != parent.end();
                          child++)
      {
      if (Str_i_Eq(child->getAttribute("name"), name))
         return XMLNode(*child);
      else
         {
         XMLNode node = findObject(*child, name);
         if (node.isValid())
            return node;
         }
      }
   return XMLNode();
   }

//---------------------------------------------------------------------------
// Convert a single 'object' from old BORLAND format to XML
//---------------------------------------------------------------------------
void Report::addObjectToXML(istream& in, const string& objectName,
                            const string& objectType, XMLDocument& doc)
   {
   // go collect all settings for this object and store in the 2 vectors
   // declared immediately below.
   vector<string> elements, types, values;
   string line;
   string sourceName;
   bool end = false;
   int nestingLevel = 0;
   while (in && !end && getline(in, line))
      {
      stripLeadingTrailing(line, " >");
      if (line == "item")
         nestingLevel++;
      if (line == "end")
         {
         if (nestingLevel > 0)
            nestingLevel--;
         else
            end = true;
         }
      string key, value;
      int posEquals = line.find('=');
      if (posEquals != string::npos)
         {
         key = line.substr(0, posEquals);
         stripLeadingTrailing(key, " ");
         value = line.substr(posEquals+1);
         stripLeadingTrailing(value, " ");
         }

      if (Str_i_Eq(key, "source") || Str_i_Eq(key, "predData"))
         sourceName = value;
      else if (Str_i_Eq(key, "filenames.Strings"))
         addMultipleElements(in, "FileName", "filenames", elements, types, values);
      else if (Str_i_Eq(key, "interpretTitles"))
         addElement("ParseTitle", "yesno", "yes", elements, types, values);
      else if (Str_i_Eq(key, "pageName"))
         addElement("PageName", "", value, elements, types, values);
      else if (Str_i_Eq(key, "pagenames.Strings"))
        addMultipleElements(in, "PageName", "", elements, types, values);
      else if (key == "filter")
         addElement("FilterString", "", value, elements, types, values);
      else if (Str_i_Eq(key, "exceedence"))
         addElement("Exceedence", "yesno", "yes", elements, types, values);
      else if (Str_i_Eq(key, "FieldName"))
         addElement("FieldName", "fieldnames", value, elements, types, values);
      else if (Str_i_Eq(key, "FileName"))
         addElement("FileName", "filename", value, elements, types, values);
      else if (Str_i_Eq(key, "Experiment"))
         addElement("Experiment", "experiment", value, elements, types, values);
      else if (Str_i_Eq(key, "Treatment"))
         addElement("Treatment", "treatment", value, elements, types, values);
      else if (Str_i_Eq(key, "DataSource"))
         addElement("DataSource", "datasource", value, elements, types, values);
      else if (Str_i_Eq(key, "cropnames.Strings"))
        addMultipleElements(in, "CropName", "", elements, types, values);
      else if (Str_i_Eq(key, "AveragedFields.Strings"))
        addMultipleElements(in, "AveragedField", "fieldnames", elements, types, values);
      else if (Str_i_Eq(key, "Month"))
         addElement("Month", "month", value, elements, types, values);
      else if (Str_i_Eq(key, "Negative"))
         addElement("Phase", "soiphase", "Negative", elements, types, values);
      else if (Str_i_Eq(key, "Positive"))
         addElement("Phase", "soiphase", "Positive", elements, types, values);
      else if (Str_i_Eq(key, "Falling"))
         addElement("Phase", "soiphase", "Falling", elements, types, values);
      else if (Str_i_Eq(key, "Rising"))
         addElement("Phase", "soiphase", "Rising", elements, types, values);
      else if (Str_i_Eq(key, "Zero"))
         addElement("Phase", "soiphase", "Zero", elements, types, values);
      else if (Str_i_Eq(key, "GetSOIFromSource"))
         addElement("GetSOIFromSource", "yesno", "yes", elements, types, values);
      else if (Str_i_Eq(key, "AllOtherYears"))
         addElement("Phase", "soiphase", "AllOtherYears", elements, types, values);
      else if (Str_i_Eq(key, "stats"))
         {
         string statsAsString = splitOffBracketedValue(value, '[', ']');
         vector<string> stats;
         splitIntoValues(statsAsString, ", ", stats);
         for (unsigned i = 0; i != stats.size(); i++)
            addElement("Stat", "stat", stats[i].substr(4), elements, types, values);
         }
      else if (Str_i_Eq(key, "FirstRecord"))
         addElement("FirstRecord", "yesno", "yes", elements, types, values);
      else if (Str_i_Eq(key, "LastRecord"))
         addElement("LastRecord", "yesno", "yes", elements, types, values);
      else if (Str_i_Eq(key, "RecordNumber"))
         addElement("RecordNumber", "", value, elements, types, values);
      else if (Str_i_Eq(key, "X1"))
         addElement("X1FieldName", "fieldname", value, elements, types, values);
      else if (Str_i_Eq(key, "Y1"))
         addElement("Y1FieldName", "fieldname", value, elements, types, values);
      else if (Str_i_Eq(key, "Label"))
         addElement("Label", "fieldname", value, elements, types, values);
      else if (Str_i_Eq(key, "labels.Strings"))
        addMultipleElements(in, "fieldname", "Label", elements, types, values);
      else if (Str_i_Eq(key, "filters.Strings"))
        addMultipleElements(in, "filters", "FilterString", elements, types, values);
      else if (Str_i_Eq(key, "Percent"))
         addElement("Percent", "yesno", "yes", elements, types, values);
      else if (Str_i_Eq(key, "obsData"))
         addElement("obsFileName", "filename", value, elements, types, values);
      else if (Str_i_Eq(key, "keyfields.strings"))
         addMultipleElements(in, "KeyFieldName", "fieldnames", elements, types, values);
      else if (Str_i_Eq(key, "xfieldname"))
         addElement("XFieldName", "fieldname", value, elements, types, values);
      else if (Str_i_Eq(key, "yfieldname"))
         addElement("YFieldName", "fieldname", value, elements, types, values);
      }

   XMLNode data = doc.documentElement().appendChild("Data", false);
   string oType = objectType.substr(1);
   if (oType == "Regr")
      oType = "Regression";
   XMLNode object = data.appendChild(oType, true);
   object.setAttribute("name", objectName);
   if (sourceName != "")
      object.setAttribute("source", sourceName);
   for (unsigned i = 0; i != elements.size(); i++)
      {
      XMLNode element = object.appendChild(elements[i], true);
      if (types[i] != "")
         element.setAttribute("type", types[i]);
      element.setValue(values[i]);
      }
   }

//---------------------------------------------------------------------------
// Go through all children of the documentelement and nest them using their
// source attribute as the name of their parent node.
//---------------------------------------------------------------------------
void Report::nestAllObjectsUsingSource(XMLDocument& doc)
   {
   // go through the elements and values vectors and write everything to
   // the XML doc that was passed in.
   XMLNode data = doc.documentElement().appendChild("Data", false);
   XMLNode::iterator object = data.begin();
   while (object != data.end())
      {
      string sourceName = object->getAttribute("source");
      if (sourceName != "")
         {
         XMLNode parent = findObject(doc.documentElement(), sourceName);
         if (parent.isValid())
            {
            XMLNode nestedObject = parent.appendChild(*object, true);
            nestedObject.setAttribute("source", "");
            object = doc.documentElement().erase(object);
            }
         else
            object++;
         }
      else
         object++;
      }
   }

void replaceFileContents(const std::string& fileName,
                         const std::string& oldText,
                         const std::string& newText)
   {
   ifstream in (fileName.c_str());
   ostringstream contentsBuffer;
   contentsBuffer << in.rdbuf();
   in.close();
   string contents = contentsBuffer.str();
   replaceAll(contents, oldText, "~~~~");
   replaceAll(contents, "~~~~", newText);
   ofstream out(fileName.c_str());
   out << contents;
   out.close();
   }

//---------------------------------------------------------------------------
// Version 4 to 5 - Flatten out the children of data so that they are all
// the same level i.e. they are a child of "<Data>"
//---------------------------------------------------------------------------
void Report::convertVersion4To5(ifstream& in, const std::string& fileName)
   {
   static const int MAXNAMES = 15;
   static const char* Names[MAXNAMES] = {"ApsimFileReader",
                                         "Probability",
                                         "PredObs",
                                         "XmlFileReader",
                                         "Filter",
                                         "Cumulative",
                                         "Depth",
                                         "Diff",
                                         "Frequency",
                                         "KWTest",
                                         "REMS",
                                         "Regression",
                                         "SOIData",
                                         "Stats",
                                         "RecordFilter"};
   vector<string> validNames(Names, Names + MAXNAMES);
   in.close();
   replaceFileContents(fileName, "<SOI", "<SOIData");
   replaceFileContents(fileName, "</SOI>", "</SOIData>");
   XMLDocument doc(fileName);
   XMLNode::iterator data = find_if(doc.documentElement().begin(), doc.documentElement().end(),
                                    EqualToName<XMLNode>("Data"));
   if (data != doc.documentElement().end())
      {
      XMLNode::iterator dataChild = data->begin();
      while (dataChild != data->end())
         {
         XMLNode::iterator object = dataChild->begin();
         while (object != dataChild->end())
            {
            if (find_if(validNames.begin(), validNames.end(),
                        CaseInsensitiveStringComparison(object->getName())) != validNames.end())
               {
               // Is a valid name - should be moved to under data.
               XMLNode newNode = data->appendChild(*object, true);
               string sourceName = dataChild->getAttribute("name");
               if (sourceName == "")
                  sourceName = dataChild->getName();
               newNode.appendChild("source", true).setValue(sourceName);

               if (newNode.getName() == "PredObs")
                  {
                  // Change "KeyFieldName" to "FieldName"
                  XMLNode::iterator PredObsChild = newNode.begin();
                  while (PredObsChild != newNode.end())
                     {
                     if (PredObsChild->getName() == "KeyFieldName")
                        {
                        XMLNode FieldNode = newNode.appendChild("FieldName", true);
                        FieldNode.setValue(PredObsChild->getValue());
                        PredObsChild = newNode.erase(PredObsChild);
                        }
                     else
                        PredObsChild++;
                     }
                  }
               object = dataChild->erase(object);
               }
            else
               object++;
            }
         dataChild++;
         }
      }

   int Col = 0;
   int Row = 0;
   for (XMLNode::iterator child = data->begin(); child != data->end(); child++)
      {
      child->setAttribute("Left", itoa(Col*200));
      child->setAttribute("Top", itoa(Row*200));
      Col++;
      if (Col > 3)
         {
         Col = 0;
         Row++;
         }
      }

   doc.write(fileName);
   }


