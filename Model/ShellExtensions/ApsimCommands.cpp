//---------------------------------------------------------------------------

#include <string>
#include <vector>
#include <sstream>
#include <fstream>
#include <windows.h>
#include <General\platform.h>

#include <boost/algorithm/string.hpp>

#include "ApsimCommands.h"

extern HINSTANCE hInstance;
#include <shlobj.h>

//nb. windows does not care to much if you don't add the .exe suffix to a file (as you can see from some of the paths below)
using namespace std;

std::string  getApsimDirectory(void)
   {
// ------------------------------------------------------------------
// This routine provides a way for APSIM applications to get the
// home directory.  Will throw a runtime error if the current
// Application is not in the apsim directory structure.
// ------------------------------------------------------------------
   char moduleFileName[MAX_PATH];
   GetModuleFileName(hInstance, moduleFileName, sizeof moduleFileName);
   std::string dll(moduleFileName);

   size_t Pos_extension = dll.rfind("\\");
   if (Pos_extension != string::npos)
	    dll = dll.substr(0, Pos_extension);

   Pos_extension = dll.rfind("\\");
   if (Pos_extension != string::npos)
	    dll = dll.substr(0, Pos_extension);
   return dll;
}

void Convert_2_CSV(istream& In_stream, ostream& Out_stream)
   {
   string Variable;
   string Line;
   getline(In_stream, Line);
   while (In_stream)
      {
      istringstream Line_stream((char*) Line.c_str());
      Line_stream >> Variable;
      while (Line_stream)
         {
         Out_stream << Variable << ',';
         Line_stream >> Variable;
         }
      Out_stream << endl;
      getline(In_stream, Line);
      }
   }


//---------------------------------------------------------------------------
// Send all files to EXCEL.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL excelFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   boost::split(fileNames, csvFiles, boost::is_any_of(","));

   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      // open input stream
	  string fileName = fileNames[file];
      ifstream in (fileName.c_str());

      // open output stream.
      string OutPath = fileName;
      size_t Pos_extension = OutPath.rfind(".");
      if (Pos_extension != string::npos)
	    OutPath = OutPath.substr(0, Pos_extension) + ".csv";
      ofstream out (OutPath.c_str());

      // copy first two lines as is.
      string Line;
      getline (in, Line);
      out << Line << endl;
      getline (in, Line);
      out << Line << endl;

      // convert file.
      Convert_2_CSV(in, out);

      // close files.
      in.close();
      out.close();

      // give output file to EXCEL.
      ShellExecute (NULL, "open", OutPath.c_str(), NULL, "", SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Send all files to APSVis.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL apsimuigraph(const char* csvFiles)
   {
   vector<string> fileNames;
   boost::split(fileNames, csvFiles, boost::is_any_of(","));

   // write response file.
   char TempFolder[1024];
   GetTempPath(sizeof TempFolder, TempFolder);   
   string responseFile = string(TempFolder) + "\\response.file";
   ofstream out(responseFile.c_str());
   for (unsigned i = 0; i != fileNames.size(); ++i)
      out << fileNames[i] << endl;
   out.close();


   // pass response file to apsimoutlook.
   string command = "\"" + getApsimDirectory() + "\\Model\\apsimui.exe\" /ApsimGraph " + responseFile;
   WinExec(command.c_str(), SW_SHOW);
   }
//---------------------------------------------------------------------------
// Send all files to APSVis.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL runapsimgraph(const char* csvFiles)
   {
   // pass response file to apsimoutlook.
   string command = "\"" + getApsimDirectory() + "\\Model\\apsimui.exe\" /ApsimGraph \"" + csvFiles + "\"";
   WinExec(command.c_str(), SW_SHOW);
   }

//---------------------------------------------------------------------------
// Send all files to Apsim.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL runFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   boost::split(fileNames, csvFiles, boost::is_any_of(","));

   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      string command = "\"" + getApsimDirectory() + "\\Model\\apsimrun.exe\" \"" + fileNames[file] + "\"";

      // run command
      WinExec(command.c_str(), SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Convert all files to SIM format.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL createSimFiles(const char* files)
   {
   vector<string> fileNames;
   boost::split(fileNames, files, boost::is_any_of(","));

   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      string command;
      if (fileNames[file].find(".con") != string::npos)
         command = "\"" + getApsimDirectory() + "\\Model\\contosim.exe \" \"" + fileNames[file] + "\"";
      else if (fileNames[file].find(".apsim") != string::npos)
         command = "\"" + getApsimDirectory() + "\\Model\\apsimtosim.exe \" \"" + fileNames[file] + "\"";

      // run command
      WinExec(command.c_str(), SW_SHOW);
      }
   }

//---------------------------------------------------------------------------
// View a .out file.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL viewFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   boost::split(fileNames, csvFiles, boost::is_any_of(","));

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string command;
      if (fileNames[f].find(".out") != string::npos)
         command = getApsimDirectory() + "\\Model\\Viewer.exe";
      else
         command = "notepad ";
      command += " \"" + fileNames[f] + "\"";
      WinExec(command.c_str(), SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Open an interface file.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL interfaceFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   boost::split(fileNames, csvFiles, boost::is_any_of(","));

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string command = "explorer \"" + fileNames[f] + "\"";
      WinExec(command.c_str(), SW_SHOW);
      }
   }

//---------------------------------------------------------------------------
// Open an .apsim file.
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL apsimFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   boost::split(fileNames, csvFiles, boost::is_any_of(","));

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string command = getApsimDirectory() + "\\Model\\ApsimUI.exe \"" + fileNames[f] + "\"";
      WinExec(command.c_str(), SW_SHOW);
      }
   }

//---------------------------------------------------------------------------
// Probe a types file or plugin file
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL probeFile(const char* csvFiles)
   {
   vector<string> fileNames;
   boost::split(fileNames, csvFiles, boost::is_any_of(","));

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string command = getApsimDirectory() + "\\Model\\ProbeDll.exe \"" + fileNames[f] + "\"";
      WinExec(command.c_str(), SW_SHOW);
      }
   }

