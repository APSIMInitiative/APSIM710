#include "StdPlant.h"

//===========================================================================
void crop_pool_fraction_delta (const int num_part,   // (INPUT)  number of plant parts
                               float *fraction,      // (INPUT)  fraction of pools to detach
                               float *pool,          // (INPUT)  plant pool for detachment (g/m^2)
                               float *dlt_pool)      // (OUTPUT) change in plant pool
//===========================================================================
/*  Purpose
*      Multiply plant pool array by a part fraction array.
*
*  Mission Statement
*   Calculate change in %3 based on fractional decay rates.
*
*/
   {
   // Implementation Section ----------------------------------
   for(int part = 0; part < num_part; part++)
      {
      dlt_pool[part] = pool[part] * fraction[part];
      }
   }
//==========================================================================
void crop_store_value(int day_of_year,
                               int year,
                               float *array,
                               float value)
//==========================================================================
/*Purpose
 *   Stores a value in an annual circular array
 *Parameters
 *   day_of_year: day of year
 *   year:      year
 *   array:     storage array
 *   value:     value to be stored
 *Calls
 *   leap_year
 *   index origin 0 for this array!!
 */

   {
   array[day_of_year] = value;

   if (day_of_year == 365 && leap_year(year - 1))
      {
      array[366] = 0.0;
      }
   }

//==========================================================================
float crop_running_ave(int day_of_year,
                       int year,
                       float *array,
                       int number_of_days)
//==========================================================================
/*Purpose
 *   return the running average of an array over the last specified
 *   number of days.
 *Parameters
 *   dayOfYear:    day of year
 *   year:         year
 *   array:        array to use for average
 *   number_of_yays: number of days to average over
 *   index origin 0 for this array!!
 */

   {
   //Implementation
   int start_day = offset_day_of_year(year,
                                      day_of_year,
                                      -1 * number_of_days);

   return divide(sum_part_of_real(array, start_day, day_of_year, 366)
                , float(abs(number_of_days))
                , 0.0);
   }


//==========================================================================
void crop_part_fraction_delta (int part_no,            // (INPUT)
                               float *fraction,         // (INPUT)  fraction for each part
                               float part,             // (INPUT)  part value to use
                               float *dlt_part)        // (OUTPUT) change in part
//==========================================================================
/*Purpose
 *   Calculate change in a particular plant pool
 *Parameters
 *   part_no
 *   fraction[]
 *   part
 *   dlt_part
 *Calls
 */

   {
   *dlt_part = part * fraction[part_no];
   }

void crop_part_fraction_delta (float fraction,         // (INPUT)  fraction for each part
                               float part,             // (INPUT)  part value to use
                               float *dlt_part)        // (OUTPUT) change in part
//==========================================================================
/*Purpose
 *   Calculate change in a particular plant pool
 *Parameters
 *   part_no
 *   fraction[]
 *   part
 *   dlt_part
 *Calls
 */

   {
   *dlt_part = part * fraction;
   }


//===========================================================================
void crop_radn_int0(float cover_green,
                    float fr_intc_radn,
                    float radn,
                    float *radn_int)
//===========================================================================

/*  Purpose
*       Radiation intercepted by leaves (mj/m^2)
*
*  Mission Statement
*   Calculate radiation interception using green crop cover
*
*  Changes
*     010994 jngh specified and programmed
*     090695 psc  change extinction coef with row spacing
*     970216 slw generalised to avoid common blocks
*
*  Constant Values
*      character  my_name*(*)     ! name of procedure
*      parameter (my_name = 'crop_radn_int0')
*
*  Sub-Program Arguments
*      float *cover_green           ! (INPUT)  fraction of radiation reaching the canopy that is intercepted by the green leaves of the canopy (0-1)
*      float *fr_intc_radn          ! (INPUT)  fraction of radiation intercepted by canopy
*      float *radn                  ! (INPUT)  solar radiation (Mj/m^2/day)
*      float *radn_int              ! (OUTPUT) radiation intercepted by leaves (mj/m^2)
*
*/
   {
   //Local Varibles
   // Implementation Section ----------------------------------
   if (reals_are_equal (fr_intc_radn, 0.0))
      {
      // we need to calculate our own interception
      *radn_int = cover_green * radn;
      }
   else
      {
      // interception has already been calculated for us
      *radn_int = fr_intc_radn * radn;
      }
   }


//==============================================================================
extern "C" EXPORT void STDCALL crop_radn_int1(float *extinction_coef, float *fr_intc_radn,
      float *lai, float *radn, float *radn_int)
//==============================================================================

/*  Purpose
*       This routine returns the radiation intercepted by leaves (mj/m^2)
*
*  Mission Statement
*   Calculate radiation interception using an extinction coefficient.
*
*  Changes
*      060495 nih taken from template
*      970216 slw generalised to avoid common blocks
*
*  Constant Values
*      character  my_name*(*)     ! name of procedure
*      parameter (my_name = 'crop_radn_int1')
*
*  Sub-Program Arguments
*      float *extinction_coef       ! (INPUT)  radiation extinction coefficient ()
*      float *fr_intc_radn          ! (INPUT)  fraction of radiation intercepted by canopy
*      float *lai                   ! (INPUT)  live plant green lai
*      float *radn                  ! (INPUT)  solar radiation (Mj/m^2/day)
*      float *radn_int              ! (OUTPUT) radiation intercepted by leaves (mj/m^2)
*/
   {

   //  Local Variables
   float cover;            // fraction of radn that is intercepted
                           // by leaves (0-1) (m^2/m^2)
   // Implementation Section ----------------------------------
   if (fabs(*fr_intc_radn)<= 1.0E-6)
      {
            // we need to calculate our own interception

            // this equation implies that leaf interception of
            // solar radiation obeys Beer's law

      cover = 1.0 - exp (- 1 * *extinction_coef * *lai);
      *radn_int = cover * *radn;
      }

   else
      {
            // interception has already been calculated for us
      *radn_int = *fr_intc_radn * *radn;
      }
   
   }

//---------------------------------------------------------------------------
int SameValue(float a, float b, float error_margin=1.0E6) {
	  if (fabs(a-b) < error_margin) return 1;
	  return 0;
}
int SameValue(float a, float b) {
     return(SameValue(a,b,1.0E6));
}


// AIEEEEE when will this stuff ever die??XXXXX
// Floating point to ascii (string).
string ftoa(double Float, char *fmtspec)
   {
   char fbuf[80], buf[80];
   sprintf(fbuf, "%%%sf", fmtspec);
   sprintf(buf, fbuf, Float);
   return(string(buf));
   }

//integer to ascii
string itoa(int value, int width)
   {
   char fbuf[80], buf[80];
   sprintf(fbuf, "%%%dd", width);
   sprintf(buf, fbuf, value);
   return(string(buf));
   }

string addUnitsToDDML(const string& ddml, const string& units)
   {
   string returnString = ddml;
   unsigned pos = returnString.find("/>");
   if (pos != string::npos)
      returnString = returnString.substr(0, pos) + " unit=\"" + units + "\"/>";
   return returnString;
   }


