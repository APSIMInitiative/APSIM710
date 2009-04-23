#ifndef CulmH
#define CulmH
#include "IndividualLeaf.h"

// Abstract class for leaf objects
class Culm
   {
   public:
   Culm(ScienceAPI& API, plantInterface& plant);
   virtual ~Culm() {};

   private:
      plantInterface& Plant;
      ScienceAPI& scienceAPI;
      float population;
      vector<IndividualLeaf> Leaf;

      interpolationFunction NodeAppRate;
   };

#endif

