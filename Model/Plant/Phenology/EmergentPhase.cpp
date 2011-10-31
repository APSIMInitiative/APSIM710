#include "StdPlant.h"

#include "EmergentPhase.h"

void EmergentPhase::OnSow(float sowingdepth)
//=======================================================================================
   {
   sowing_depth = sowingdepth;
   }
void EmergentPhase::read()
//=======================================================================================
   {
   Phase::read();
   scienceAPI.read("shoot_lag", shoot_lag, 0.0f, 1000.0f);
   scienceAPI.read("shoot_rate", shoot_rate, 0.0f, 1000.0f);

   rel_emerg_rate.read(scienceAPI,
                         "fasw_emerg", "()", 0.0, 1.0,
                         "rel_emerg_rate",  "()", 0.0, 1.0);
   }

void EmergentPhase::setupTTTarget()
//=======================================================================================
   {
   target = shoot_lag+sowing_depth*shoot_rate;
   }

string EmergentPhase::description()
//=======================================================================================
   {
   string s;
   s =  "         shoot_lag                  = "+ftoa(shoot_lag, "7.0")+ " (dd)\n";
   s += "         shoot_rate                 = "+ftoa(shoot_rate, "7.0")+ " (dd/mm)\n";

   return s;
   }

float EmergentPhase::stress()
   {
   return rel_emerg_rate[plant.getFaswSeed()];
   }

