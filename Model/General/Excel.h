//---------------------------------------------------------------------------
#ifndef ExcelH
#define ExcelH

#include <string>
#include <vector>
#include "Excel_2K_SRVR.h"
#include <OleServer.hpp>

//---------------------------------------------------------------------------
// Return a list of EXCEL sheet names for the specified xlsfile.
//---------------------------------------------------------------------------
void getXLSSheetNames(const std::string& xlsFileName, std::vector<std::string>& pageNames);

//---------------------------------------------------------------------------
// Locate the specified sheet.  Return true if found.
//---------------------------------------------------------------------------
bool getXLSSheet(const std::string& sheetName,
                 TExcelApplication* excelApp,
                 ExcelWorksheetPtr& sheet);

//---------------------------------------------------------------------------
// Return a row of values from the specified XLS sheet.  Returns true
// if a row of values was read ok.  If row is outside the range of
// rows in the sheet then false will be returned. Row is 1 index based.
//---------------------------------------------------------------------------
bool getXLSRow(ExcelWorksheetPtr worksheet, int row, std::vector<std::string>& values);
#endif
