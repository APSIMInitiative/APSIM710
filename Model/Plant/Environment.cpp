#include "StdPlant.h"

#include "Environment.h"

Environment::Environment(ScienceAPI& api, const std::string& name)
//===========================================================================
   : plantThing(api, name)
   {
   year = 0;
   day_of_year = 0;
   latitude = 0.0;

   NewMet.mint = 0.0;
   NewMet.maxt = 0.0;
   NewMet.radn = 0.0;
   NewMet.rain = 0.0;

   scienceAPI.read("svp_fract", svp_fract, 0.0f, 1.0f);
   scienceAPI.read("co2_default", co2_default, 0.0f, 1000.0f);

   scienceAPI.subscribe ("newmet", NewMetFunction(&Environment::OnNewMet));
   scienceAPI.subscribe ("tick", TimeFunction(&Environment::OnTick));
   }

void Environment::OnNewMet(protocol::NewMetType &newmet)
//===========================================================================
// Field a NewMet event
  {
  NewMet = newmet;
  }

void Environment::OnTick(protocol::TimeType &Tick)
//=======================================================================================
// Event Handler for the Tick Event
   {
   double sd = (double)Tick.startday;
   jday_to_day_of_year(&sd, &day_of_year, &year);
   if (latitude == 0.0)
      scienceAPI.get("latitude", "deg", latitude, -90., 90.);

   if (!scienceAPI.getOptional("co2", "mg/kg", _co2, 0.0, 1500.0))
      _co2 = co2_default;
   }
float Environment::co2() const
//===========================================================================
   {
   return _co2;
   }

float Environment::vpd(float svp_fract, float maxt, float mint) const
//==========================================================================
   {
   float vpd = svp_fract * (svp(maxt) - svp(mint));
   return vpd;
   }


float Environment::svp(float temp) const
//==========================================================================
// function to get saturation vapour pressure for a given temperature in oC (kpa)
   {
   float val = 6.1078 *
            exp(17.269 * temp / (237.3 + temp)) *
            mb2kpa;
   return val;
   }

float Environment::vpdEstimate (void) const
//===========================================================================
   {
   return (vpd(svp_fract, maxt(), mint()));
   }

float Environment::dayLength(float sun_angle) const
//===========================================================================
   {
    return dayLength(day_of_year, sun_angle);
   }

float Environment::dayLength(int dyoyr, float sun_angle) const
//===========================================================================
   {
   return ::day_length(dyoyr, latitude, sun_angle);
   }

