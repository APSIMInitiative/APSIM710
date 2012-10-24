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
#endif

using namespace std;
using namespace protocol;

//---------------------------------------------------------------------------
// Convert the specified .apsim to a .sim and return the filename of the .sim
//---------------------------------------------------------------------------
string ConvertToSim(const string& apsimPath, string& simulationName)
   {
   // Create a command line to ApsimToSim
   string CommandLine = "%apsim%\\Model\\ApsimToSim.exe \"" + apsimPath + "\"";
   if (simulationName != "")
      {
      replaceAll(simulationName, "Simulation=", "");
      replaceAll(simulationName, "simulation=", "");
      CommandLine += " \"" + simulationName + "\"";
      }
   CommandLine += " 2> apsim.tmp";
   replaceAll(CommandLine, "%apsim%", getApsimDirectory());

   // exec ApsimToSim and read its stdout as the .sim file name.
   system(CommandLine.c_str());
   ifstream in("apsim.tmp");
   string simPath;
   getline(in, simPath);
   in.close();

   if (simPath.find("Written ") == string::npos)
      return "";
   else
      {
      unlink("apsim.tmp");
      replaceAll(simPath, "Written ", "");
      return simPath;
      }
   }

//---------------------------------------------------------------------------
// Main routine for running APSIM from .sim script.
//---------------------------------------------------------------------------
extern "C" int EXPORT STDCALL RunAPSIM(const char* sdml)
{
   int code = 0;
   try
      {
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
      cout << Banner;

      Simulation simulation;
      simulation.go(sdml);
      code = 0;
      }
   catch (const exception& error)
      {
      cout << "APSIM  Fatal  Error" << endl;
      cout << "-----------------" << endl;
      cout << error.what() << endl;
      code = 1;
      }
   catch (...)
      {
      cout << "APSIM  Fatal  Error" << endl;
      cout << "-----------------" << endl;
      cout << "An unknown exception has occurred in APSIM" << endl;
      code = 1;
      }

   cout.flush();
   return code;
}


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
      if ((rp = realpath(argv[1], Full_path) != NULL)
          simPath = rp;
   #endif

   // Make sure the .sim file exists.
   if (!fileExists(simPath))
      {
      cout << "APSIM  Fatal  Error" << endl;
      cout << "-------------------" << endl;
      cout << "Cannot find simulation file: " << simPath.c_str() << endl;
      return 1;
      }
   // If simPath is actually a .apsim file then convert to sim.
   bool DeleteSim = false;
   if (fileExtension(simPath) == "apsim")
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
      RunAPSIM(sdml.str().c_str());
      }
}

//---------------------------------------------------------------------------
// This is needed for LINUX compatibility. ProtocolManager seems to look for
// this during link.
extern "C" unsigned get_componentID(void)
   {
   return 0;
   }

