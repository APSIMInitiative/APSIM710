#include "../StdPlant.h"

#include "Phase.h"
#include "PhotoPhase.h"
#include "../Environment.h"
#include "../Root/RootBase.h"

void PhotoPhase::read()
//=======================================================================================
   {
   Phase::read();

   string key1 = "x_pp_"+name();
   string key2 = "y_tt_"+name();

   photo_tt.read(scienceAPI,
                  key1.c_str(), "h", 0.0, 100.0,
                  key2.c_str(), "dd", 0.0, 1e6);

   stressFunction.readOptional(scienceAPI, "x_sw_avail_ratio_" + name(), "()", 0.0, 1.0,
                                   "y_swdef_pheno_" + name(), "()", 0.0, 5.0);
   scienceAPI.read("twilight", twilight, -90.0f, 90.0f);
   string key = name()+"_pp_inductive_cutoff";
   if (!scienceAPI.readOptional(key, cutoff))
      cutoff = "end";
   }


void PhotoPhase::updateTTTargets()
//=======================================================================================
   {
   if (cutoff == "start")
      {
      if (tt == 0.0)
         {
         photoperiod = plant.environment().dayLength(twilight);
         target = photo_tt.value(photoperiod);
         }
      }
   else if (cutoff == "end")
      {
      photoperiod = plant.environment().dayLength(twilight);
      target = photo_tt.value(photoperiod);

      }
   else
      throw std::invalid_argument("Invalid cutoff for photoperiod inductive phase");
   }

string PhotoPhase::description()
//=======================================================================================
   {
   return photo_tt.description();
   }


float PhotoPhase::stress()
//=========================================================================
// Get the soil water availability ratio in the root zone
// and calculate the 0 - 1 stress factor for phenology.
// 1 is no stress, 0 is full stress.
   {
   if (plant.root().swAvailablePotential() > 0.0 && stressFunction.isInitialised())
      {
      float sw_avail_ratio = divide (plant.root().swAvailable(), plant.root().swAvailablePotential(), 1.0);
      sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0);
      return stressFunction.value(sw_avail_ratio);
      }
   else
      return 1.0;
   }

