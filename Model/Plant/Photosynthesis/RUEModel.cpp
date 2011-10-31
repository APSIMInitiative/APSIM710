#include "StdPlant.h"

#include "RUEModel.h"
#include "../Co2Modifier.h"
#include "../Environment.h"
#include "../Phenology/Phenology.h"
using namespace std;

RUEModel::RUEModel(ScienceAPI& scienceAPI, plantInterface& p)
   : PhotosynthesisModel(scienceAPI, p)
   {
//    RUE.read(scienceAPI,
//              "x_stage_rue", "()", 0.0, 1000.0,
//              "y_rue", "(g dm/mj)", 0.0, 1000.0);
   };

float RUEModel::PotentialDM (float radiationInterceptedGreen)   // (PFR)
   {
   float Diffuse_factor = plant.environment().DiffuseLightFactor();

   double stress_factor = min(min(min(plant.getTempStressPhoto(), plant.getNfactPhoto())
                               , plant.getOxdefPhoto()), plant.getPfactPhoto());

   return radiationInterceptedGreen  * getRUE() * Diffuse_factor * stress_factor * plant.getCo2Modifier()->rue();

   }

float RUEModel::getRUE(void) // (PFR)
   {
   return plant.phenology().doInterpolation(RUE);

   }

void RUEModel::Read(void)   // (PFR)
   {
    RUE.read(scienceAPI,
              "x_stage_rue", "()", 0.0, 1000.0,
              "y_rue", "(g dm/mj)", 0.0, 1000.0);


   }
