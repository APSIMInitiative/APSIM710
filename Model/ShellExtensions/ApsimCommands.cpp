//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimCommands.h"
#include <general\string_functions.h>
#include <general\path.h>
#include <general\stream_functions.h>
#include <apsimshared\apsimdirectories.h>
#pragma package(smart_init)

//nb. windows does not care to much if you don't add the .exe suffix to a file (as you can see from some of the paths below)


//---------------------------------------------------------------------------
// Send all files to EXCEL.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall excelFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      // open input stream
      ifstream in (fileNames[file].c_str());

      // output output stream.
      Path OutPath (fileNames[file]);
      OutPath.Set_extension (".csv");
      ofstream out (OutPath.Get_path().c_str());

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
      ShellExecute (NULL, "open", OutPath.Get_path().c_str(), NULL, "", SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Send all files to APSVis.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsvisFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   // write response file.
   string responseFile = Path::getTempFolder().Get_path() + "\\response.file";
   ofstream out(responseFile.c_str());
   for (unsigned i = 0; i != fileNames.size(); ++i)
      out << fileNames[i] << endl;
   out.close();


   // pass response file to apsimoutlook.
   string command = "\"" + getApsimDirectory() + "\\Model\\apsvis.exe\" " + responseFile;
   WinExec(command.c_str(), SW_SHOW);
   }
//---------------------------------------------------------------------------
// Send all files to APSVis.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsimuigraph(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   // write response file.
   string responseFile = Path::getTempFolder().Get_path() + "\\response.file";
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
extern "C" _export void __stdcall runapsimgraph(const char* csvFiles)
   {
   // pass response file to apsimoutlook.
   string command = "\"" + getApsimDirectory() + "\\Model\\apsimui.exe\" /ApsimGraph \"" + csvFiles + "\"";
   WinExec(command.c_str(), SW_SHOW);
   }

//---------------------------------------------------------------------------
// Send all files to Apsim.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall runFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

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
extern "C" _export void __stdcall createSimFiles(const char* files)
   {
   vector<string> fileNames;
   Split_string(files, ",", fileNames);

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
extern "C" _export void __stdcall viewFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

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
// Send all files to ApsimReport
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsimReportFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      // pass response file to apsimoutlook.
      string command = "\"" + getApsimDirectory() + "\\Model\\ApsimReport.exe\" \"" + fileNames[f] + "\"";
      WinExec(command.c_str(), SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Open an interface file.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall interfaceFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string command = "explorer \"" + fileNames[f] + "\"";
      WinExec(command.c_str(), SW_SHOW);
      }
   }

//---------------------------------------------------------------------------
// Open an .apsim file.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsimFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string command = getApsimDirectory() + "\\Model\\ApsimUI.exe \"" + fileNames[f] + "\"";
      WinExec(command.c_str(), SW_SHOW);
      }
   }


