#include "StdPlant.h"

#include "PlantLibrary.h"

using namespace std;

/*   ================================================================
 *    Conversion constants
 *   ================================================================
 *
 *   Short description:
 *     Globally used conversion constants
 */
// WEIGHT conversion
const float gm2kg = (float)(1.0/1000.0);   // constant to convert g to kg
const float kg2gm = 1000.0f;               // conversion of kilograms to grams
const float mg2gm = (float)(1.0/1000.0);   // conversion of mg to grams
const float t2g = (float)(1000.0*1000.0);  // tonnes to grams
const float g2t = (float)(1.0/ t2g);       // grams to tonnes
const float t2kg = 1000.0f;                // tonnes to kilograms
const float kg2t = (float)(1.0/ t2kg);     // kilograms to tonnes

// AREA conversion
const float ha2scm = (float)(10000.0*10000.0); // ha to sq cm
const float ha2sm = 10000.0f;              // conversion of hectares to sq metres
const float sm2ha = (float)(1.0/10000.0);  // constant to convert m^2 to hectares
const float sm2smm = 1000000.0f;           // conversion of square metres to square mm
const float smm2sm = (float)(1.0/1000000.0); // conversion factor of mm^2 to m^2
const float scm2smm = 100.0f;              // conversion factor of cm^2 to mm^2

// PRESSURE and related conversion
const float g2mm = (float)(1.0e3/1.0e6);  // convert g water/m^2 to mm water
                                          // 1 g water = 1,000 cubic mm and
                                          // 1 sq m = 1,000,000 sq mm
const float mb2kpa = (float)(100.0/1000.0);  // convert pressure mbar to kpa
                                          // 1000 mbar = 100 kpa

// LENGTH conversion
const float cm2mm = 10.0f;                 // cm to mm
const float mm2cm = (float)(1.0/10.0);     // conversion of mm to cm
const float mm2m = (float)(1.0/1000.0);    // conversion of mm to m
const float km2m  = 1000.0f;               // conversion of km to metres

// VOLUME conversion
const float cmm2cc = (float)(1.0/1000.0);     // conversion of cubic mm to cubic cm
const float conv_gmsm2kgha = 100.0f;       // convert g/sm -> kg/ha
const float conv_pc2fr = 0.01f;            // convert %age to fraction
const float pcnt2fract = (float)(1.0/100.0);  // convert percent to fraction
const float fract2pcnt = 100.0f;           // convert fraction to percent
const float mm2lpsm = 1.0f;                // mm depth to litres per sq m
const float lpsm2mm = 1.0f;                // litres per sq m to mm depth
const float day2hr  = 24.0f;               // days to hours
const float hr2s    = (float)(60.0*60.0);  // hours to seconds
const float s2hr    = (float)(1.0/hr2s);   // seconds to hours


void stageSubject::update() {
  for (std::list<observer*>::iterator o = observers.begin();
       o !=  observers.end();
       o++)
      (*o)->update();
};
void stageSubject::reset(){
   for (std::list<observer*>::iterator o = observers.begin();
        o !=  observers.end();
        o++)
       (*o)->reset();
};

stageSubject::stageSubject() {};


bool char2any(const char *str, int &value) {
   return (sscanf(str, "%d", &value) == 1);
}
bool char2any(const char *str, float &value) {
   return (sscanf(str, "%f", &value) == 1);
}
std::string any2string(float value) {
   return(ftoa(value, ".2"));
}
std::string any2string(int value) {
   return(itoa(value, 5));
}
#if 0
//+  Purpose
//     Calculate min and max crown temperatures.
void crop_crown_temp_nwheat( float tempmx        //Daily maximum temperature of the air (C)
                            ,float tempmn        //Daily minimum temperature of the air (C)
                            ,float snow          //Snow depth on the current day (mm)
                            ,float *tempcx       //Daily maximum of crown temperature (C)     - OUTPUT
                            ,float *tempcn)      //Daily minimum of crown temperature (C)     - OUTPUT
   {
   // Calculate max crown temperature
   if (tempmx < 0.)
        {
        *tempcx = 2.0 + tempmx * (0.4 + 0.0018 * pow(snow - 15., 2));
        }
   else
        {
        *tempcx = tempmx;
        }

   // Calculate min crown temperature
   if (tempmn < 0.)
        {
        *tempcn = 2.0 + tempmn * (0.4 + 0.0018 * pow(snow - 15., 2));
        }
   else
        {
        *tempcn = tempmn;
        }
   }

#endif
/* Purpose
*     returns the temperature for a 3 hour period.
*      a 3 hourly estimate of air temperature
*/
float temp_3hr (float tmax, float tmin, int period)
   {
   // Local Variables
   float period_no;              // period number
   float diurnal_range;          // diurnal temperature range for the
                                 //   day (oC)
   float t_deviation;            // deviation from day's minimum for this
                                 //    3 hr period
   float t_range_fract;          // fraction_of of day's range_of for this
                                 //   3 hr period

   // Implementation Section ----------------------------------
   if (period < 1)
      throw std::invalid_argument("3 hr. period number is below 1");
   else if (period > 8)
      throw std::invalid_argument("3 hr. period number is above 8");

   period_no = float(period);
   t_range_fract = (float)(0.92105
                   + 0.1140  * period_no
                   - 0.0703  * pow(period_no,2)
                   + 0.0053  * pow(period_no,3));

   diurnal_range = tmax - tmin;
   t_deviation = t_range_fract * diurnal_range;
   return  (tmin + t_deviation);
   }

/* Purpose
*     Eight interpolations of the air temperature are
*     calculated using a three-hour correction factor.
*     For each air three-hour air temperature, a value
*     is calculated.  The eight three-hour estimates
*     are then averaged to obtain the daily value.
*/
float linint_3hrly_temp (float tmax,          //(INPUT) maximum temperature (oC)
                         float tmin,          //(INPUT) maximum temperature (oC)
                         externalFunction *ttFn)
   {
   //Constants
   int num3hr = 24/3;     // number of 3 hourly temperatures

   // Local Variables
   int period;                  // three hourly period number
   float tot;                   // sum_of of 3 hr interpolations

   // Implementation Section ----------------------------------
   tot = 0.0;

   for(period = 1; period <= num3hr; period++)
      {
      // get mean temperature for 3 hr period (oC)
      float tmean_3hour = temp_3hr (tmax, tmin, period);
      tot = tot + ttFn->value(tmean_3hour);
      }
   return tot / (float)num3hr;
   }


//===========================================================================
void cproc_sw_demand1(float dlt_dm_pot_rue,      //(INPUT)  potential dry matter production with opt
                      float transp_eff,          //(INPUT)  transpiration efficiency (g dm/m^2/mm wa
                      float *sw_demand)          //(OUTPUT) crop water demand (mm)
//===========================================================================
/*  Purpose
*       Return crop water demand from soil by the crop (mm) calculated by
*       dividing biomass production limited by radiation by transpiration efficiency.
*
*  Mission Statement
*   Calculate the crop demand for soil water (based upon transpiration efficiency)
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed
*       970216 slw generalised
*
*/
   {
   // get potential transpiration from potential
   // carbohydrate production and transpiration efficiency
   *sw_demand = (float)divide (dlt_dm_pot_rue, transp_eff, 0.0);
   }

//==========================================================================
void cproc_transp_eff_co2_1(float vpd,        // (INPUT)
                          float transp_eff_cf,    // (INPUT)  transpiration efficiency coefficien
                          float co2_modifier,     // (INPUT)  te modifier of co2 level (0-1)
                          float *transp_eff)       // (OUTPUT) transpiration coefficient
//=========================================================================
/*  Purpose
*       Calculate today's transpiration efficiency from min,max temperatures and co2 level
*       and converting mm water to g dry matter (g dm/m^2/mm water)
*
*  Mission Statement
*       Calculate today's transpiration efficiency from VPD and CO2 level
*
*  Assumptions
*       the temperatures are > -237.3 oC for the svp function.
*       if co2_level=0.0 then co2_level=350ppm
*
*  Notes
*       Average saturation vapour pressure for ambient temperature
*       during transpiration is calculated as part-way between that
*       for minimum temperature and that for the maximum temperature.
*       Tanner & Sinclair (1983) used .75 and .67 of the distance as
*       representative of the positive net radiation (rn).  Daily SVP
*       should be integrated from about 0900 hours to evening when Radn
*       becomes negative.
*
*  Changes
*       21/5/2003 ad converted to BC++
*       20000721 ew developed from crop_transp_eff1 and added co2 effect on transp_eff
*
*  Sub-Program Arguments
*/
   {
   //  Local Variables

   // Implementation Section ----------------------------------
   //get vapour pressure deficit when net radiation is positive.

   vpd = l_bound (vpd, 0.01f);

   *transp_eff = (float)(divide (transp_eff_cf, vpd, 0.0) / g2mm);

   *transp_eff = *transp_eff * co2_modifier;
   }   
