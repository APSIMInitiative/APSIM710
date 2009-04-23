#include <ComponentInterface2/ScienceAPI.h>
using namespace std;

#include "PlantInterface.h"


//-----------------------------------------------------------------------------
// Create an instance of the Plant module
//-----------------------------------------------------------------------------
extern "C" PlantInterface* STDCALL EXPORT createComponent(ScienceAPI & api)
   {
   return new PlantInterface(api);
   }

extern "C" void STDCALL EXPORT deleteComponent(PlantInterface* component)
   {
   delete component;
   }

