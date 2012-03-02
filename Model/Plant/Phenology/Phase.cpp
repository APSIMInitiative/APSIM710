#include "StdPlant.h"

#include "Phase.h"
#include "../Environment.h"


Phase::Phase(ScienceAPI& api, plantInterface& p, const std::string& StartStageName, const std::string& endStageName)
   : scienceAPI(api), plant(p)
   {
   myName = StartStageName;
   EndStageName = endStageName;
   tt = target = days = 0.0;
   tt_after = 0.0;
   days_after = 0.0;
   DaysFromSowingToEndOfPhase = 0;
   scienceAPI.expose(myName + "TTTarget", "deg.days", myName + " thermal time target", target);
   scienceAPI.expose("TTAfter" + myName, "deg.days", myName + " - thermal time spent in this phase", tt_after);
   scienceAPI.exposeFunction("DaysAfter" + myName, "day", myName + " - days spent in this phase", IntGetter(&Phase::getDaysAfter));
   if (EndStageName != "")
      scienceAPI.exposeWritable("DaysTo" + EndStageName, "day", "Settable number of days to " + EndStageName, 
                                IntSetter(&Phase::setDaysTo) );
   }

bool operator == (const Phase &a, const Phase &b)
// ===================================================================================
   {
   return (a.name() == b.name());
   };


void Phase::read()
   {
   y_tt.read(scienceAPI,
               "x_temp", "oC", 0.0, 100.0,
               "y_tt", "oC days", 0.0, 100.0);
   }

float Phase::TT()
   {
   return linint_3hrly_temp (plant.environment().maxt(), plant.environment().mint(), &y_tt);
   }

void Phase::calcPhaseDevelopment(int das, float& dlt_tt_phenol, float& phase_devel)
   {
   dlt_tt_phenol = TT() * stress();

   if (DaysFromSowingToEndOfPhase > 0)
      {
      if (das >= DaysFromSowingToEndOfPhase)
         phase_devel = 1;
      else
         phase_devel = 0;
      }
   else
      phase_devel = (float)divide(getTT() + dlt_tt_phenol, getTTTarget(), 1.0);
   }

void Phase::setDaysTo(int NumDaysTo)
   {
   DaysFromSowingToEndOfPhase = NumDaysTo;   
   
   scienceAPI.write("     FIXED number of days from sowing to " + EndStageName + " = " + itoa(DaysFromSowingToEndOfPhase));
   scienceAPI.write("");
   scienceAPI.write("");
   
   }
      
