#include <ComponentInterface2/ScienceAPI.h>
using namespace std;

#include "PlantInterface.h"


//-----------------------------------------------------------------------------
// Create an instance of the Plant module
//-----------------------------------------------------------------------------
extern "C" EXPORT PlantInterface* STDCALL createComponent(ScienceAPI & api)
   {
   return new PlantInterface(api);
   }

extern "C" void EXPORT STDCALL deleteComponent(PlantInterface* component)
   {
   delete component;
   }

