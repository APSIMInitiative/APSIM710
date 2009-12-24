#include "StdPlant.h"

#include "StageBasedInterpolationFunction.h"
#include "Phenology/Phenology.h"
using namespace std;

StageBasedInterpolationFunction::StageBasedInterpolationFunction(plantInterface& p, ScienceAPI& API, const std::string& Name, const std::string& Units, const std::string& Description)
   : scienceAPI(API), plant(p)
   {
   this->Name = Name;

   if (f.readOptional(scienceAPI, "X"+Name+"StageCode" , "()", 1.0, 12.0
                    , "Y"+Name, "()", 0.0, 1e6))
      scienceAPI.exposeFunction(Name, Units, Description, FloatGetter(&StageBasedInterpolationFunction::value));
   }


float StageBasedInterpolationFunction::value(void)
   {
   return plant.phenology().doInterpolation(f);
   }





