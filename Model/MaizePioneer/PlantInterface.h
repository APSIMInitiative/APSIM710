//---------------------------------------------------------------------------
#ifndef PlantInterfaceH
#define PlantInterfaceH

class ScienceAPI2;

#include "Plant.h"
namespace Maize {
   // ------------------------------------------------------------------
   // This component acts as the interface between an instance of a
   // Plant model and an APSIM simulation.
   // ------------------------------------------------------------------
   class PlantInterface 
      {
      public:
         PlantInterface(ScienceAPI2 & api) : scienceAPI(api)
            {
            scienceAPI.subscribe("init1", nullFunction(&PlantInterface::onInit1));
            scienceAPI.subscribe("init2", nullFunction(&PlantInterface::onInit2));
            plant = NULL;
            };

         ~PlantInterface(void)
            {
            if (plant) delete plant;
            };

      private:
         Plant     *plant;    // The plant module
         ScienceAPI2  &scienceAPI;

         void onInit1(void)
            {
            plant = new Plant(scienceAPI);
            plant->plantInit1();
            }
         void onInit2(void)
            {
            plant->plantInit2();
            }
      };
#endif
   }
