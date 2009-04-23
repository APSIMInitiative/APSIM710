//---------------------------------------------------------------------------

#ifndef Report2H
#define Report2H
#include <string>
#include <fstream>
#include "DataContainer.h"
namespace Quickrpt {
   class TQuickRep;
   class TQRCompositeReport;
   };
namespace Classes {
   class TComponent;
   };
class XMLDocument;
//---------------------------------------------------------------------------
// Report class.
//---------------------------------------------------------------------------
class __declspec(dllexport) Report
   {
   public:
      Report(TWinControl* parent);
      ~Report(void);

      //---------------------------------------------------------------------------
      // Clear the report and create a new one.
      //---------------------------------------------------------------------------
      void clear(void);

      //---------------------------------------------------------------------------
      // If the filename exists then load it into the report.
      //---------------------------------------------------------------------------
      void load(const std::string& fileName, bool quiet, bool safe);

      //---------------------------------------------------------------------------
      // Save the report to the specified filename.
      //---------------------------------------------------------------------------
      void save(const std::string& fileName);

      //---------------------------------------------------------------------------
      // Edit the report.  Returns a pointer to the palette.
      //---------------------------------------------------------------------------
      TForm* edit(bool turnOn);

      //---------------------------------------------------------------------------
      // Set the object inspector form to use.
      //---------------------------------------------------------------------------
      void setObjectInspector(TWinControl* objectInspector, TDataSource* dataInspectorSource);

      //---------------------------------------------------------------------------
      // Page methods.
      //---------------------------------------------------------------------------
      AnsiString createPage(void);
      void getPageNames(TStrings* pageNames);
      void showPage(unsigned pageIndex);
      void showDataPage();
      void renamePage(AnsiString oldName, AnsiString newName);
      void deletePage(unsigned pageIndex);
      void movePage(unsigned oldPageIndex, unsigned insertBeforePageIndex);

      //---------------------------------------------------------------------------
      // Copy the report to the clipboard.
      //---------------------------------------------------------------------------
      void copyToClipboard(void);

      //---------------------------------------------------------------------------
      // zoom methods
      //---------------------------------------------------------------------------
      int getZoom(void);
      void setZoom(int zoom);

      //---------------------------------------------------------------------------
      // report orientation methods
      //---------------------------------------------------------------------------
      bool getIsPortrait(void);
      void setIsPortrait(bool isPortrait);

      //---------------------------------------------------------------------------
      // Print the report.
      //---------------------------------------------------------------------------
      void print(bool currentPageOnly);

      //---------------------------------------------------------------------------
      // Refresh the report
      //---------------------------------------------------------------------------
      void refresh(bool quiet);

      //---------------------------------------------------------------------------
      // Return true if the report needs saving.
      //---------------------------------------------------------------------------
      bool needsSaving(void) {return isDirty;}

      //---------------------------------------------------------------------------
      // Return a component to caller.
      //---------------------------------------------------------------------------
      //TComponent* getAComponent(const std::string& componentName);

      //---------------------------------------------------------------------------
      // Event that triggers when object inspector is updated.
      //---------------------------------------------------------------------------
      TNotifyEvent OnObjectInspectorUpdate;

      //---------------------------------------------------------------------------
      // Set zoom to fit.
      //---------------------------------------------------------------------------
      void setZoomToFit(bool fit) {zoomToFit = fit;}

   private:
      TWinControl* parent;
      std::vector<Quickrpt::TQuickRep*> pages;
      Quickrpt::TQuickRep* currentPage;
      bool isDirty;
      bool isEditing;
      bool zoomToFit;
      TForm* reportForm;
      TWinControl* objectInspector;
      TDataSource* dataInspectorSource;
      TForm* uiForm;
      TScrollBox* scrollBox;
      TImageList* buttonImages;
      HINSTANCE handle;
      Quickrpt::TQRCompositeReport* compositeReport;
      void _export __stdcall (*callDLL)(const char* dllFileName, const char* className,
                              const char* methodName, const char* methodArgument);


      DataContainer* data;

      void loadFromContents(const std::string& contents, bool quiet);
      std::string getReportXml();

      //---------------------------------------------------------------------------
      // The designer has changed selection - get current object and call
      // selectionChangedEvent.
      //---------------------------------------------------------------------------
      void __fastcall onDesignerSelectionChanged(TObject* sender);

      //---------------------------------------------------------------------------
      // update the objectInspector for the specified component.
      //---------------------------------------------------------------------------
      void updateObjectInspector(TComponent* component);

      //---------------------------------------------------------------------------
      // Centre the current page within the parent container
      //---------------------------------------------------------------------------
      void centrePage(void);

      //---------------------------------------------------------------------------
      // The report has been resized - reposition ourselves in the middle
      // of the form.
      //---------------------------------------------------------------------------
      void __fastcall onResize(TObject* sender);

      //---------------------------------------------------------------------------
      // Export the current page to the specified file.
      //---------------------------------------------------------------------------
      void exportCurrentToFile(const std::string& fileName);


      //---------------------------------------------------------------------------
      // Add a button to the specified toolbar.
      //---------------------------------------------------------------------------
      void addButtonToToolBar(TComponent* component, TToolBar* toolbar);

      //---------------------------------------------------------------------------
      // loop through all add-in's looking for a resource for the specified
      // component name.  Return true if found.
      //---------------------------------------------------------------------------
      bool getComponentBitmap(AnsiString className, Graphics::TBitmap* bitmap);

      //---------------------------------------------------------------------------
      // User has clicked a button - go display object inspector.
      //---------------------------------------------------------------------------
      void __fastcall buttonClick(TObject* button);

      void convertVersion3To4(std::ifstream& in, const std::string& fileName);
      void convertVersion4To5(std::ifstream& in, const std::string& fileName);
      void addObjectToXML(std::istream& in, const std::string& objectName,
                            const std::string& objectType, XMLDocument& doc);
      void nestAllObjectsUsingSource(XMLDocument& doc);
      void refreshIfNecessary(bool quiet);
      void refreshAllPages();
      void refreshControls(TWinControl* parent);
      void safeMode(const std::string& fileName);

      void __fastcall OnAddReports(TObject* sender);
      void checkPageContents(std::string& pageContents);

   };
#endif
