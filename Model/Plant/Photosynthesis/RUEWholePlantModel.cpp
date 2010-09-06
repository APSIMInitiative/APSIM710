#include "../StdPlant.h"

#include "RUEWholePlantModel.h"
#include "../Co2Modifier.h"
#include "../Environment.h"
#include "../Phenology/Phenology.h"
using namespace std;

RUEWholePlantModel::RUEWholePlantModel(ScienceAPI& scienceAPI, plantInterface& p)
   : PhotosynthesisModel(scienceAPI, p)
   {
//    RUE.read(scienceAPI,
//              "x_stage_rue", "()", 0.0, 1000.0,
//              "y_rue", "(g dm/mj)", 0.0, 1000.0);
   };

float RUEWholePlantModel::PotentialDM (float radiationInterceptedGreen)
   {
   float Diffuse_factor = plant.environment().DiffuseLightFactor();
   double stress_factor = min(min(min(plant.getTempStressPhoto(), plant.getNfactPhoto())
                               , plant.getOxdefPhoto()), plant.getPfactPhoto());

   return radiationInterceptedGreen  * getRUE() * Diffuse_factor * stress_factor * plant.getCo2Modifier()->rue();

   }

float RUEWholePlantModel::getRUE(void)
   {
   return cRUEWholePlant;
   }

void RUEWholePlantModel::Read(void)
   {
    scienceAPI.read("rue_whole_plant", cRUEWholePlant, 0.0f, 4.0f);                // (g DM/MJ)
   }

