//---------------------------------------------------------------------------
#include <general/pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Excel.h"
#include <general/string_functions.h>
#pragma package(smart_init)
//---------------------------------------------------------------------------
// Return a list of EXCEL sheet names for the specified xlsfile.
//---------------------------------------------------------------------------
void getXLSSheetNames(const string& xlsFileName, vector<string>& pageNames)
   {
   pageNames.erase(pageNames.begin(), pageNames.end());

   TExcelApplication* ExcelApp = new TExcelApplication(NULL);
   ExcelApp->Connect();
   ExcelApp->Workbooks->Open(WideString(xlsFileName.c_str()));

   for(int i = 1; i <= ExcelApp->Sheets->Count; i++)
      {
      ExcelWorksheetPtr worksheet = ExcelApp->Sheets->get_Item((Variant)i);
      pageNames.push_back(AnsiString(worksheet->Name).c_str());
      worksheet->Release();
      }

   ExcelApp->Workbooks->Close();
   ExcelApp->Quit();
   ExcelApp->Disconnect();
   delete ExcelApp;
   }
//---------------------------------------------------------------------------
// Locate the specified sheet.  Return true if found.
//---------------------------------------------------------------------------
bool getXLSSheet(const std::string& sheetName,
                 TExcelApplication* excelApp,
                 ExcelWorksheetPtr& sheet)
   {
   for(int sheetIndex = 1; sheetIndex <= excelApp->Sheets->Count; sheetIndex++)
      {
      ExcelWorksheetPtr worksheet;
      worksheet = excelApp->Sheets->get_Item((Variant)sheetIndex);
      if (strcmpi(sheetName.c_str(), AnsiString(worksheet->Name).c_str()) == 0)
         {
         sheet = worksheet;
         return true;
         }
      worksheet->Release();
      }
   return false;
   }

//---------------------------------------------------------------------------
// Return a row of values from the specified XLS sheet.  Returns true
// if a row of values was read ok.  If row is outside the range of
// rows in the sheet then false will be returned. Row is 1 index based.
//---------------------------------------------------------------------------
bool getXLSRow(ExcelWorksheetPtr worksheet, int row, vector<string>& values)
   {
   values.erase(values.begin(), values.end());

   // get the number of columns.
   RangePtr usedRange = worksheet->get_UsedRange();
   RangePtr usedColumns = usedRange->Columns;
   RangePtr usedRows = usedRange->Rows;
   int numCols = usedColumns->Count;
   int numRows = usedRows->Count;

   // make sure row is in range.
   if (row >= 1 && row <= numRows)
      {
      // work out the start and end ref in excel notation e.g B45
      AnsiString startref = "A" + IntToStr(row);

      int endCol1 = (numCols-1) / 26;
      int endCol2 = numCols-endCol1*26;
      AnsiString endref;
      if (endCol1 > 0)
         endref = (char) (endCol1 + 'A' - 1);
      endref += (char) (endCol2 + 'A' - 1);
      endref += IntToStr(row);

      // ask EXCEL for the entire row.
      RangePtr range = worksheet->get_Range((Variant)startref, (Variant)endref);
      Variant farray;
      farray = range->get_Value();

      // Look through each cell on row and store value in cells.
      int indexes[2] = {1, 1};   // row, col
      for (int col = 1; col <= numCols; col++)
         {
         indexes[1] = col;
         Variant variantValue = VarArrayGet(farray, indexes, 1);
         AnsiString value = variantValue;
         string strippedValue = value.c_str();
         stripLeadingTrailing(strippedValue, " ");
         values.push_back(strippedValue);
         }
      }
   return (values.size() > 0);
   }
