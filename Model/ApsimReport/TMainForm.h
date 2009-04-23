//---------------------------------------------------------------------------

#ifndef TMainFormH
#define TMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include <FileCtrl.hpp>
#include "MRUFList.hpp"
#include <ExtCtrls.hpp>
#include <Buttons.hpp>
#include <general\xml.h>
#include <SEGReport\report2.h>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
   TOpenDialog *OpenDialog1;
   TSaveDialog *SaveDialog1;
   TImageList *ImageList1;
   TActionList *ActionList1;
   TAction *NewAction;
   TAction *OpenAction;
   TAction *SaveAction;
   TAction *SaveAsAction;
   TAction *CopyToClipboardAction;
   TAction *PageSetupAction;
   TAction *PrintAction;
   TAction *RefreshAction;
   TdfsMRUFileList *MRUFileList;
   TAction *PrintCurrentPageAction;
   TAction *EditDataAction;
   TPanel *BottomDockPanel;
   TAction *EditReportAction;
   TTabControl *TabControl;
   TPopupMenu *PagePopupMenu;
   TMenuItem *Addanewpage1;
   TMenuItem *Deletecurrentpage1;
   TMenuItem *Renamecurrentpage1;
   TControlBar *ButtonBar;
   TPanel *Panel2;
   TSpeedButton *NewButton;
   TSpeedButton *OpenButton;
   TSpeedButton *SaveButton;
   TSpeedButton *SaveAsButton;
   TSpeedButton *RefreshButton;
   TPanel *Panel1;
   TLabel *Label1;
   TEdit *ZoomEdit;
   TUpDown *ZoomUpDown;
   TSpeedButton *EditReportButton;
   TSpeedButton *EditDataButton;
   TSpeedButton *CopyButton;
   TSpeedButton *PageSetupButton;
   TSpeedButton *PrintAllButton;
   TPanel *Panel3;
   TPanel *Panel4;
   TPanel *ObjectInspector;
   TSplitter *ObjectInspectorSplitter;
   TSpeedButton *PageButton;
   TAction *PageAction;
   TPopupMenu *OpenMenu;
   TSpeedButton *ExportButton;
   TSaveDialog *ExportDialog;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall ExitActionExecute(TObject *Sender);
   void __fastcall OpenActionExecute(TObject *Sender);
   void __fastcall SaveActionExecute(TObject *Sender);
   void __fastcall SaveAsActionExecute(TObject *Sender);
   void __fastcall ZoomEditKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
   void __fastcall NewActionExecute(TObject *Sender);
   void __fastcall CopyToClipboardActionExecute(TObject *Sender);
   void __fastcall PageSetupActionExecute(TObject *Sender);
   void __fastcall PrintActionExecute(TObject *Sender);
   void __fastcall ZoomEditChange(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall RefreshActionExecute(TObject *Sender);
   void __fastcall MRUFileListMRUItemClick(TObject *Sender,
          AnsiString AFilename);
   void __fastcall PrintCurrentPageActionExecute(TObject *Sender);
   void __fastcall EditReportActionExecute(TObject *Sender);
   void __fastcall EditDataActionExecute(TObject *Sender);
   void __fastcall pageChanged(TObject* sender);
   void __fastcall addMenuItemClick(TObject* sender);
   void __fastcall deleteMenuItemClick(TObject* sender);
   void __fastcall renameMenuItemClick(TObject* sender);
   void __fastcall onMouseDown(TObject* sender, TMouseButton button,
                               TShiftState state, int x, int y);
   void __fastcall onDragOver(TObject* sender, TObject* source,
                              int x, int y, TDragState state,
                              bool &accept);
   void __fastcall onDragDrop(TObject* sender, TObject* source,
                              int x, int y);
   void __fastcall PageActionExecute(TObject *Sender);
   void __fastcall OnExport(TObject *Sender);


private:	// User declarations
   AnsiString filename;
   AnsiString fileThatWasCopied;
   Report report;
   int draggedTab;

   void open(AnsiString file, bool quiet, bool safe);
   void save(AnsiString file);
   void saveIfNecessary(void);
   void setCaption(void);
   int textToZoom(AnsiString zoomText);
   void processCommandLine(AnsiString commandLine);
   void populateOpenMenu();
   void __fastcall OpenMenuItemClick(TObject* sender);

public:		// User declarations
   __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
