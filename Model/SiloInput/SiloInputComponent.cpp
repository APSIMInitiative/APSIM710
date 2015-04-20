#include <fstream>
#include <stdexcept>
#include <string>
#ifdef __WIN32__
 #include <process.h>
 #define PATH_MAX 1024
#else
 #include <unistd.h>
#endif

#include <sys/stat.h>

#include <General/string_functions.h>
#include <General/path.h>
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

extern "C" void STDCALL getDescriptionInternal(char* initScript,
                                                 char* description);   
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
   
extern "C" void STDCALL getDescriptionLengthInternal(char* initScript,
                                                 int* length);
// ------------------------------------------------------------------
// Return component description length.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL getDescriptionLength(char* initScript, int* length)
   {
   getDescriptionLengthInternal(initScript, length);
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
   if (fileName != "")
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

   fileName = getCurrentDirectory() + string("/temp") + itoa(pid) + string(".met");

   // Now the SILO station number 
   readParameter ("parameters", "station_number", stationNumber, 0, 100000);
   if (stationNumber == 0)  // This happens during GetDescription call.
      return;
   
   baseURL = getProperty("parameters", "url");
   string requestString =
      baseURL +
      string("?format=apsim&station=") + itoa(stationNumber) +
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
