#include "StdPlant.h"

#include "Environment.h"

Environment::Environment(ScienceAPI& api, const std::string& name)
//===========================================================================
   : plantThing(api, name)
   {
   year = 0;
   day_of_year = 0;
   latitude = 0.0;
   twilight = 0.0;

   NewMet.mint = 0.0;
   NewMet.maxt = 0.0;
   NewMet.radn = 0.0;
   NewMet.rain = 0.0;

   scienceAPI.read("svp_fract", svp_fract, 0.0f, 1.0f);
   scienceAPI.read("co2_default", co2_default, 0.0f, 1000.0f);
   scienceAPI.read("twilight", twilight, -90.0f, 90.0f);
   ReadDiffuseLightFactorTable();

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

float Environment::dayLength(void) const
//===========================================================================
   {
    return dayLength(day_of_year, twilight);
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

float Environment::deltaDayLength(void) const                                // Plant and Food Reserach
//===========================================================================
   {
    return dayLength(day_of_year, twilight) - dayLength(day_of_year-1, twilight);
   }

void Environment::ReadDiffuseLightFactorTable(void)
   {
    if (!DiffuseLightFactorTable.readOptional(scienceAPI,
              "x_diffuse_fr", "(0-1)", 0.0, 1.0,
              "y_rue_factor", "(0-1)", 0.0, 10.0))
        DiffuseLightFactorTable.setDefaultValue(1.0);  // Note this defaults to a value of 1 if not specified.
   }

float Environment::DiffuseLightFactor (void)
   {
   float Q = Q0(latitude, day_of_year);
   float T = radn()/Q;
   float X1 = 0.80 - 0.0017 * latitude + 0.000044*latitude*latitude;
   float A1 = (0.05 - 0.96) / (X1 - 0.26);
   float A0 = 0.05 - A1*X1;

   float Diffuse_fr = min(max(0.0, A0 + A1*T), 1.0);  //Taken from Roderick paper Ag For Met(?)
   return DiffuseLightFactorTable.value(Diffuse_fr);
   }

float Environment::Q0(float lat, int day)
   {
   float DEC = 23.45 * sin(2. * 3.14159265 / 365.25 * (day - 82.25));
   float DECr = DEC * 2. * 3.14159265 / 360.;
   float LATr =    lat * 2. * 3.14159265 / 360.;
   float HS = acos(-tan(LATr) * tan(DECr));

   return  86400. * 1360. * (HS * sin(LATr) * sin(DECr) + cos(LATr) * cos(DECr) * sin(HS)) / 3.14159265 / 1000000.;
   }
