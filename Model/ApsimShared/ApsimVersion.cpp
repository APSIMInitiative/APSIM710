#include <string>
#include <vector>
#include <stdexcept>
#include <General/IniFile.h>
#include <General/platform.h>
#include "ApsimDirectories.h"
#include "ApsimVersion.h"
#include "ApsimSettings.h"

//---------------------------------------------------------------------------
// Return the APSIM version number
//---------------------------------------------------------------------------
std::string EXPORT getApsimVersion(void)
   {
   std::string versionString;
   ApsimSettings settings;
   settings.read("version|apsim", versionString, false);
   return versionString;
   }
