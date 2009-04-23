//---------------------------------------------------------------------------

#ifndef ApsimCommandsH
#define ApsimCommandsH
//---------------------------------------------------------------------------
// Send all files to EXCEL.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall excelFiles(const char* csvFiles);

extern "C" _export void __stdcall apsimuigraph(const char* csvFiles);
extern "C" _export void __stdcall runapsimgraph(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to APSVis.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsvisFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to Apsim Outlook.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsimoutlookFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to Apsim.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall runFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Convert all files to SIM format.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall createSimFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to an editor.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall viewFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to ApsimReport
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsimReportFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Open an interface file.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall interfaceFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Open an .apsim file.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsimFiles(const char* csvFiles);

#endif
