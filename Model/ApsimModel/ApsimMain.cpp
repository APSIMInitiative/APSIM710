#include <stdio.h>
#include <string>
#include <fstream>
#include <iostream>
#include <stdexcept>

#include <General/path.h>
#include <General/stl_functions.h>
#include <General/TreeNodeIterator.h>
#include <General/xml.h>
#include <General/exec.h>

#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/ApsimSystemData.h>
#include <ApsimShared/ApsimServiceData.h>
#include <ApsimShared/ApsimSimulationFile.h>
#include <ApsimShared/ApsimDirectories.h>
#include <ComponentInterface/Interfaces.h>

#include "Simulation.h"
#ifdef __WIN32__
  #include <windows.h>
#elif defined(__unix__) 
  #include <unistd.h>
  #include <limits.h>
  #define GetCurrentProcessId getpid
#endif

using namespace std;
using namespace protocol;

//---------------------------------------------------------------------------
// Convert the specified .apsim to a .sim and return the filename of the .sim
//---------------------------------------------------------------------------
string ConvertToSim(const string& apsimPath, string& simulationName)
   {
   // Create a command line to ApsimToSim
#ifdef __WIN32__
   string pathsep = "\\";
#else 
   string pathsep = "/";
#endif
   string CommandLine = "%APSIM%" + pathsep + "Model" + pathsep + "ApsimToSim.exe";
   replaceAll(CommandLine, "%APSIM%", getApsimDirectory());
   if (CommandLine.find_first_of(' ') != string::npos)
	   CommandLine = "\"" + CommandLine + "\"";

#ifndef __WIN32__
   CommandLine = "mono " + CommandLine; // Ensure mono is explicitly called for systems without binfmt_misc registration
#endif

   if (apsimPath.find_first_of(' ') != string::npos)
	   CommandLine += " \"" + apsimPath + "\"";
   else    
	   CommandLine += " " + apsimPath;

   if (simulationName != "")
      if (simulationName.find_first_of(' ') != string::npos)
         CommandLine += " \"" + simulationName + "\"";
      else
         CommandLine += " " + simulationName;

   // We need to send StdErr from ApsimToSim to a unique filename. Create that filename now.
   string uniqueFileName; 
   if (simulationName  == "")
	   uniqueFileName = fileDirName(apsimPath) + pathsep + fileTailNoExtension(apsimPath) + "." + itoa(GetCurrentProcessId());
   else
	   uniqueFileName = fileRoot(apsimPath)+ "." + itoa(GetCurrentProcessId());

   replaceAll(uniqueFileName, "\"", "");
   unsigned i = uniqueFileName.find_last_of("/");
   if (i != string::npos)
      uniqueFileName = uniqueFileName.substr(i+1);
   uniqueFileName += ".sims";

   CommandLine += " > " + (uniqueFileName.find_first_of(' ') != string::npos ? 
	                                   "\"" + uniqueFileName + "\"" : uniqueFileName) + " 2>&1";
   // Hack for quoting under win32 - see quoting rules in "cmd /?". If we've installed apsim in a dir with spaces,
   // then the executable will be quoted, and 
#ifdef __WIN32__   
   if (CommandLine[0] == '\"') 
      CommandLine = "\"" +  CommandLine + "\"";
#endif

   // exec ApsimToSim and read its stdout as the .sim file name.
   if (system(CommandLine.c_str()) != 0) {
       cout << "       Fatal  Error" << endl;
       cout << "-------------------" << endl;
       cout << "Error finding \"" << simulationName << "\" in apsim file " << apsimPath << endl;
   }
   ifstream in(uniqueFileName.c_str());
   string simPath;
   getline(in, simPath);
   in.close();

   if (simPath.find("Written ") == string::npos)
   {
      cout << simPath << endl;
      return "";
   }
   unlink(uniqueFileName.c_str());
   replaceAll(simPath, "Written ", "");
   return simPath;
   }

//---------------------------------------------------------------------------
// Main routine for running APSIM from .sim script.
//---------------------------------------------------------------------------
extern "C" int EXPORT STDCALL RunAPSIM(const char* sdml)
{
   int retcode = 0;
   try
      {
      Simulation simulation;
      simulation.go(sdml);
      }
   catch (const std::exception& error)
      {
      if (!Str_i_Eq(error.what(), "fatal")) 
         {        
         cout << "     Fatal  Error" << endl;
         cout << "-----------------" << endl;
         cout << error.what() << endl;
         cout.flush();
         }  
      retcode = 1;
      }
   catch (const int& code)
   {
       retcode = code;
   }
   catch (...)
      {
      cout << "     Fatal  Error" << endl;
      cout << "-----------------" << endl;
      cout << "An unknown exception has occurred in APSIM" << endl;
      cout.flush();
      retcode = 1;
      }
#ifndef __WIN32__
   _exit(retcode); // mono vm will (sometimes) deadlock when calling global destructors
#endif
   return(retcode);
}

      const char* Banner  = "     ###     ######     #####   #   #     #   \n"
                            "    #   #    #     #   #        #   ##   ##   \n"
                            "   #     #   #     #   #        #   ##   ##   \n"
                            "   #######   ######     #####   #   # # # #   \n"
                            "   #     #   #              #   #   #  #  #   \n"
                            "   #     #   #         #####    #   #  #  #   \n"
                            "                                              \n"
                            "                                              \n"
                            " The Agricultural Production Systems Simulator\n"
                            "             Copyright(c) APSRU               \n\n";

//---------------------------------------------------------------------------
// main entrypoint when running from command line.
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   if (argc == 1)
      {
      cout << "Usage: APSIM simfile" << endl;
      return 1;
      }

   cout << Banner;

   setlocale(LC_ALL, "English_Australia.1252");
   // Get the full path name to the sim file.
   std::string simPath = argv[1];
   #ifdef __WIN32__
      char Full_path[MAX_PATH];
      char *Ptr_to_name;
      if (GetFullPathName(argv[1], sizeof Full_path, Full_path, &Ptr_to_name) > 0)
         simPath = Full_path;
   #else
      char Full_path[PATH_MAX];
      char *rp;
      if ((rp = realpath(argv[1], Full_path)) != NULL)
          simPath = rp;
   #endif

   // Make sure the .sim file exists.
   if (!fileExists(simPath))
      {
      cout << "       Fatal  Error" << endl;
      cout << "-------------------" << endl;
      cout << "Cannot find simulation file: " << simPath.c_str() << endl;
      return 1;
      }
   // If simPath is actually a .apsim file then convert to sim.
   bool DeleteSim = false;
   if (ToLower(fileExtension(simPath)) == "apsim")
      {
      string simulationName = "";
      if (argc == 3)
         simulationName = argv[2];
      simPath = ConvertToSim(simPath, simulationName);
      DeleteSim = true;
      }
   if (simPath != "")
      {
      // Change the working directory to where the .sim file is.
      Path simFile(simPath);
      simFile.Change_directory();

      // get sdml contents.
      ifstream in(simPath.c_str());
      ostringstream sdml;
      sdml << in.rdbuf();
      in.close();
      if (DeleteSim)
         unlink(simPath.c_str());
      return ( RunAPSIM(sdml.str().c_str()));
      }
   return 1;
}

//---------------------------------------------------------------------------
// This is needed for LINUX compatibility. ProtocolManager seems to look for
// this during link.
extern "C" unsigned get_componentID(void)
   {
   return 0;
   }

