//---------------------------------------------------------------------------

#ifndef ApsimCommandsH
#define ApsimCommandsH
//---------------------------------------------------------------------------
// Send all files to EXCEL.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL excelFiles(const char* csvFiles);

extern "C" void EXPORT STDCALL apsimuigraph(const char* csvFiles);
extern "C" void EXPORT STDCALL runapsimgraph(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to Apsim.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL runFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Convert all files to SIM format.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL createSimFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to an editor.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL viewFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Open an .apsim file.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL apsimFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Probe a types file or plugin file
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL probeFile(const char* csvFiles);

#endif
