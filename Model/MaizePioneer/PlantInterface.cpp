#include <ComponentInterface2/ScienceAPI2.h>
using namespace std;

#include "PlantInterface.h"

using namespace Maize;
//-----------------------------------------------------------------------------
// Create an instance of the Plant module
//-----------------------------------------------------------------------------
extern "C" EXPORT PlantInterface* STDCALL createComponent(ScienceAPI2 & api)
   {
   return new PlantInterface(api);
   }

extern "C" void EXPORT STDCALL deleteComponent(PlantInterface* component)
   {
   delete component;
   }

