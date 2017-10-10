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
    RUEFactor = 1.0;
    scienceAPI.expose("RUEFactor", "", "General Factor used to modify RUE in plant model", RUEFactor);
    scienceAPI.exposeWritable("RUEFactor", "", "RUEFactor", FloatSetter(&RUEModel::onSetRUEFactor));
    //system->addGettableVar("RUEFactor", RUEFactor, "", "RUEFactor");
   };

void RUEModel::onSetRUEFactor(float Factor)
   {
   	RUEFactor = Factor;
   }
float RUEModel::PotentialDM (float radiationInterceptedGreen)   // (PFR)
   {
   float Diffuse_factor = plant.environment().DiffuseLightFactor();

   double stress_factor = min(min(min(min(plant.getTempStressPhoto(), plant.getNfactPhoto()), plant.getOxdefPhoto()), plant.getPfactPhoto()),RUEFactor);

   return (float)(radiationInterceptedGreen  * getRUE() * Diffuse_factor * stress_factor * plant.getCo2Modifier()->rue());

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
void RUEModel::ZeroAll(void)
   {
    RUEFactor = 1.0;
   }   
