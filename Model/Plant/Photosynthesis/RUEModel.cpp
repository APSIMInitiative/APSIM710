#include "../StdPlant.h"

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

float RUEModel::Potential (float radiationInterceptedGreen)
   {
   float lat = plant.environment().Latitude();
   float Q = Q0(lat,plant.environment().dayOfYear());
   float T = plant.environment().radn()/Q;
   float X1 = 0.80 - 0.0017*lat + 0.000044*lat*lat;
   float A1 = (0.05 - 0.96)/(X1 - 0.26);
   float A0 = 0.05 - A1*X1;

   float Diffuse_fr = min(max(0.0,A0 + A1*T),1.0);  //Taken from Roderick paper Ag For Met(?)
   float Diffuse_factor = DiffuseLightFactor.value(Diffuse_fr);

   double stress_factor = min(min(min(plant.getTempStressPhoto(), plant.getNfactPhoto())
                               , plant.getOxdefPhoto()), plant.getPfactPhoto());

   return radiationInterceptedGreen  * plant.phenology().doInterpolation(RUE) * Diffuse_factor * stress_factor * plant.getCo2Modifier()->rue();

   }

void RUEModel::Read(void)
   {
    RUE.read(scienceAPI,
              "x_stage_rue", "()", 0.0, 1000.0,
              "y_rue", "(g dm/mj)", 0.0, 1000.0);

    DiffuseLightFactor.readOptional(scienceAPI,
              "x_diffuse_fr", "(0-1)", 0.0, 1.0,
              "y_rue_factor", "(0-1)", 0.0, 10.0, 1.0);  // Note this defaults to a value of 1 if not specified.

   }

float RUEModel::Q0(float lat, int day)
   {
   float DEC = 23.45 * sin(2. * 3.14159265 / 365.25 * (day - 82.25));
   float DECr = DEC * 2. * 3.14159265 / 360.;
   float LATr =    lat * 2. * 3.14159265 / 360.;
   float HS = acos(-tan(LATr) * tan(DECr));

   return  86400. * 1360. * (HS * sin(LATr) * sin(DECr) + cos(LATr) * cos(DECr) * sin(HS)) / 3.14159265 / 1000000.;

   }
