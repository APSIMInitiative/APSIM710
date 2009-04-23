#ifndef ZadokH
#define ZadokH

#include "PlantInterface.h"
class Zadok : public plantThing
   {
   private:
      plantInterface& plant;
      float calcZadok();

   public:
      Zadok(ScienceAPI& scienceAPI, plantInterface& p);

   };
#endif

