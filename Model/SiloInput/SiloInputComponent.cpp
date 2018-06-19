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
#include <ApsimShared/ApsimDirectories.h>
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
   string msg = "SILO URL: " + getProperty("parameters", "url");
   writeString(msg.c_str());
   }
// ------------------------------------------------------------------
// Open the input file associtated with this module.
// ------------------------------------------------------------------
void SiloInputComponent::openInputFile(void)
    {
    // Find a unique temporary name to hold our data
    unsigned int pid = getpid();

#ifdef __WIN32__
    string pathsep = "\\";
#else 
    string pathsep = "/";
#endif

    fileName = getCurrentDirectory() + pathsep + string("temp") + itoa(pid) + string(".met");
    string csFileName = getCurrentDirectory() + pathsep + string("temp") + itoa(pid) + string(".cs");
    ofstream f(csFileName);

    f << "//css_ref System;\n\
using System;\n\
using System.Net;\n\
class Download {\n\
    static int Main(string[] args)\n\
    {\n\
        using (var client = new WebClient())\n\
        {\n\
            try {client.DownloadFile(\"" + getProperty("parameters", "url") + "\", @\"" + fileName + "\");}\n\
            catch (Exception e) {Console.WriteLine(@\"Error downloading to " + getCurrentDirectory() + "\n\" +  e.Message ); return(1);}\n\
            return(0);\n\
        }\n\
    }\n\
}";
    f.close();

    string CommandLine = "%APSIM%" + pathsep + "Model" + pathsep + "cscs.exe";
    replaceAll(CommandLine, "%APSIM%", getApsimDirectory());
    if (CommandLine.find_first_of(' ') != string::npos)
	   CommandLine = "\"" + CommandLine + "\"";

#ifndef __WIN32__
    CommandLine = "mono " + CommandLine + " -nl"; 
#else 
    CommandLine += " /nl"; 
#endif

    if (csFileName.find_first_of(' ') != string::npos)
        CommandLine += " \"" + csFileName + "\"";
    else 
        CommandLine += " " + csFileName;
    
    if (system(CommandLine.c_str()) != 0) {
       throw std::runtime_error("Download error");
    }    
    unlink(csFileName.c_str());

    data.open(fileName);
    }
