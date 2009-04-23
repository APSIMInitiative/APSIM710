#include <fstream>
#include <stdexcept>
#include <string>
#ifdef __WIN32__
 #include <dir.h>
 #include <process.h>
 #define PATH_MAX 1024
#else
 #include <unistd.h>
#endif

#include <sys/stat.h>

#include <boost/date_time/gregorian/gregorian.hpp>

#include <General/string_functions.h>
#include <General/http.h>

#include <ApsimShared/ApsimDataFile.h>
#include <ComponentInterface/Component.h>

#include "../Input/StringVariant.h"
#include "../Input/InputComponent.h"
#include "SiloInputComponent.h"

using namespace std;

// ------------------------------------------------------------------
// Return a blank string when requested to indicate that we don't need a wrapper DLL.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }

// ------------------------------------------------------------------
// createComponent
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new SiloInputComponent;
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
SiloInputComponent::SiloInputComponent(void)
   {
   stationNumber = 0;
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
SiloInputComponent::~SiloInputComponent(void)
   {
   data.close();
   unlink(fileName.c_str());
   }
// ------------------------------------------------------------------
// INIT 2 - temporary
// ------------------------------------------------------------------
void SiloInputComponent::doInit2(void)
   {
   string msg = "SILOINPUT station number: " + itoa(stationNumber);
   writeString(msg.c_str());
   }
// ------------------------------------------------------------------
// Open the input file associtated with this module.
// ------------------------------------------------------------------
void SiloInputComponent::openInputFile(void)
   {
   // Find a unique temporary name to hold our data
   unsigned int pid = getpid();

   char wd[PATH_MAX];
   getcwd(wd, PATH_MAX);
   fileName = string(wd) + string("/temp") + itoa(pid) + string(".met");

   // Now the SILO station number 
   readParameter ("parameters", "station_number", stationNumber, 0, 100000);
   string requestString =
      string("http://192.168.0.60/cgi-bin/getData.tcl?format=apsim&station=") +
      itoa(stationNumber) +
      string("&ddStart=1&mmStart=1&yyyyStart=1800&ddFinish=31&mmFinish=12&yyyyFinish=2100");

   tHTTP http;
   bool ok = http.Get(fileName, requestString);
   
   // HTTP connection errors are caught here (eg webserver down / unreachable)
   if (!ok) {throw std::runtime_error(http.ErrorMessage());}    
   
   // But you still may get a zero-sized reponse, eg invalid station number.
   struct stat statbuf;
   if (stat(fileName.c_str(), &statbuf) < 0) 
       throw std::runtime_error("Temporary met file " + fileName + " is missing");

   if (statbuf.st_size == 0) 
       throw std::runtime_error("No data for station " + itoa(stationNumber) + 
                                 " appeared from silo.\n\nIs the station number correct?");

   data.open(fileName);
   }
