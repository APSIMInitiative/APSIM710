#include <stdio.h>
#include <string>
#include <fstream>
#include <iostream>
#include <stdexcept>

#include <General/path.h>
#include <General/stl_functions.h>
#include <General/TreeNodeIterator.h>
#include <General/xml.h>

#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/ApsimSystemData.h>
#include <ApsimShared/ApsimServiceData.h>
#include <ApsimShared/ApsimSimulationFile.h>

#include <ComponentInterface/Interfaces.h>

#include "Simulation.h"

using namespace std;
using namespace protocol;
//---------------------------------------------------------------------------
int main(int argc, char **argv) {
   try
      {
      Simulation simulation;

      if (argc == 2)
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

         Path simFile(argv[1]);
         simFile.Change_directory();
         simulation.go(argv[1]);
         }
      else
         {
         cerr << "To run APSIM type : " << endl;
         cerr << "   apsim sim_file_name" << endl << endl;
         cerr << "Where sim_file_name is the name of your simulation file (.SIM)";
         return 1;
         }
      }
   catch (const exception& error)
      {
      cerr << "APSIM  Fatal  Error" << endl;
      cout << "APSIM  Fatal  Error" << endl;
      cout << "-----------------" << endl;
      cout << error.what() << endl;
      return 1;
      }
   catch (...)
      {
      cerr << "APSIM  Fatal  Error" << endl;
      cout << "APSIM  Fatal  Error" << endl;
      cout << "-----------------" << endl;
      cout << "An unknown exception has occurred in APSIM" << endl;
      return 1;
      }
   return 0;
}
//---------------------------------------------------------------------------
// This is needed for LINUX compatibility. ProtocolManager seems to look for
// this during link.
extern "C" unsigned get_componentID(void)
   {
   return 0;
   }
