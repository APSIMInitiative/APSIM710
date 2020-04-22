//------------------------------------------------------------------------------------------------
#include <stdio.h>

#include <limits>
#include <float.h>
#include <ComponentInterface2/ScienceAPI2.h>
using namespace std;

#include "PlantComponents.h"
#include "Utilities.h"


// Conversion from a Julian date to a Gregorian calendar date.
// Reference: Fliegel, H. F. and van Flandern, T. C. (1968).
//    Communications of the ACM, Vol. 11, No. 10 (October, 1968).
void JulianToCalendar(double jDay,int &day,int &month,int &year)
   {
   double work = jDay + 68569.0;
   double work0 = int(4.0 * work / 146097.0);
   work = work - int((146097.0 * work0 + 3.0) / 4.0);
   double yy = int(4000.0 * (work + 1.0) / 1461001.0);

   work = work - int(1461.0 * yy / 4.0) + 31.0;
   double mm = int(80.0 * work / 2447.0);
   double dayd = work - int(2447.0 * mm / 80.0);

   work = int(mm / 11.0);
   double monthd = mm + 2.0 - 12.0 * work;
   double yeard = 100.0 * (work0 - 49.0) + yy + work;

   day = int(dayd + 0.5);
   month = int(monthd + 0.5);
   year = int(yeard + 0.5);
   }
//------------------------------------------------------------------------------------------------

int CalendarToJulian(int day,int month,int year)
   {
   if (year > 1582)
      {
      // Fliegel calculations
      double quotnt = int ((month - 14.0)/12.0);
      return day - 32075
         + int(1461.0* (year + 4800.0 + quotnt) /4.0)
         + int(367.0* (month - 2.0 - quotnt*12.0) /12.0)
         - int(3.0 * int((year + 4900.0 + quotnt) /100.0) /4.0);
      }
   else return 0;
   }
//------------------------------------------------------------------------------------------------
//------ sum up a vector of double
//------------------------------------------------------------------------------------------------
double sumVector(vector<double> vec)
   {
   double vecSum = 0.0;
   for(unsigned i=0;i < vec.size();i++)vecSum += vec[i];
   return vecSum;
   }
//------------------------------------------------------------------------------------------------
double avgVector(vector<double> vec)
   {
   if(!vec.size())return 0.0;
   return sumVector(vec)/vec.size();
   }
//------------------------------------------------------------------------------------------------
double movingAvgVector(vector<double> &vec, int sz)
   {
   if(!vec.size())return 0.0;
   for(int i=0;i < (int)vec.size() - sz; i++)
      vec.erase(vec.begin());
   return avgVector(vec);
   }
//------------------------------------------------------------------------------------------------//------------------------------------------------------------------------------------------------
double maxVector(vector<double> vec)
   {
   double vecMax = 1e-23;
   for(unsigned i=0;i < vec.size();i++)
      if(vec[i] > vecMax)vecMax = vec[i];
   return vecMax;
   }
//------------------------------------------------------------------------------------------------
double sumVector(vector<double> vec, int index)
   {
   double vecSum = 0.0;
   for(int i=0;i < index;i++)vecSum += vec[i];
   return vecSum;
   }
//------------------------------------------------------------------------------------------------
double sumVector(vector<double> vec, int from, int to)
   {
   double vecSum = 0.0;
   for(int i=from;i < to;i++)vecSum += vec[i];
   return vecSum;
   }
//------------------------------------------------------------------------------------------------
//---  Checks if a variable lies outside lower and upper bounds.
//------------------------------------------------------------------------------------------------
void checkRange(ScienceAPI2 &api, double value, double lower, double upper, const std::string &vName)
   {
   float epsilon = FLT_EPSILON;

   char msg[512];
   char m1[] = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
"                      APSIM Warning Error\n"
"                      -------------------\n";
   char m2[] = "     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";


   if(lower > upper)
      {
      sprintf(msg,
         "%s     %s: Lower bound (%f) exceeds upper bound (%f)\n        Variable is not checked\n%s",
         m1, vName.c_str(),lower, upper, m2);
      api.write(msg);
      }
   else if (value > upper + epsilon)                   //is the value too big?
      {
      sprintf(msg,
              "%s     %s = %f\n        exceeds upper limit of %f\n%s",
              m1, vName.c_str(),value,upper, m2);
      api.write(msg);
      }
   else if (value + epsilon < lower)                  //is the value too small?
      {
      sprintf(msg,
              "%s     %s = %f\n        less than lower limit of %f\n%s",
              m1, vName.c_str(), value, lower, m2);
      api.write(msg);
         }
   }
 /* TODO : Needs looking at! */
//XX Needs to be replaced with try/catch of EOverflow/EUnderflow etc..    XXXXXXXXXXXXXXXXXXXXXXX
//===========================================================================
double divide (double dividend, double divisor, double default_value)
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values
   double LARGEST = 1.0e30;    //largest acceptable no. for quotient
   double SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   double nought = 0.0;

   //Local Varialbes
   double quotient;

   //Implementation
   if(isEqual(dividend, 0.0))      //multiplying by 0
      {
      quotient = 0.0;
      }
   else if(isEqual(divisor, 0.0))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(fabs(divisor) < 1.0)            //possible overflow
      {
      if(fabs(dividend) > fabs(LARGEST * divisor)) //overflow
         {
         quotient = default_value;
         }
      else
         {
         quotient = dividend / divisor;          //ok
         }
      }
   else if(fabs(divisor) > 1.0)             //possible underflow
      {
      if(fabs(dividend) < fabs(SMALLEST * divisor))    //underflow
         {
         quotient = nought;
         }
      else
         {
         quotient = dividend / divisor;                //ok
         }
      }
   else
      {
      quotient = dividend / divisor;                   //ok
      }
   return quotient;
   }
//------------------------------------------------------------------------------------------------
//-------- function to get index number from a vector
// --       index is the number where Sigma(i=0,index){items[i]} > value
//------------------------------------------------------------------------------------------------
int findIndex(double value, vector<double> items)
   {
   unsigned index;
   double accum = 0.0;
   for(index =0;index < items.size();index++)
      {
      accum += items[index];
      if(accum > value)break;
      }
   return index == items.size() ? index - 1 : index;
   }
//------------------------------------------------------------------------------------------------
//-------- function to copy a vector to a vector
//------------------------------------------------------------------------------------------------
void fillVector(vector<double> &temp,vector<double> &newVect)
   {
   newVect.clear();
   for(unsigned i=0;i < temp.size();i++)
      {
      newVect.push_back(temp[i]);
      }
   temp.empty();
   }
//------------------------------------------------------------------------------------------------

double layerProportion(vector<double> dLayer,double rootDepth,int rootLayer)
   {
   double layerTop = sumVector(dLayer, rootLayer);
   double layerBottom = sumVector(dLayer, rootLayer+1);

   return divide(rootDepth - layerTop,layerBottom - layerTop);
   }
//------------------------------------------------------------------------------------------------

double bound(double value,double lower, double upper)
   {
   if(value < lower)return lower;
   else return Min(value,upper);
   }
//------------------------------------------------------------------------------------------------


//------------------------------------------------------------------------------------------------
//----------- Table Function constructor
//------------------------------------------------------------------------------------------------
TableFn::TableFn(ScienceAPI2 &P, string xName,string yName)
   {
   P.read(xName,"",true,x);
   P.read(yName,"",true,y);
   }
//------------------------------------------------------------------------------------------------
//----------- Table Function read
//------------------------------------------------------------------------------------------------
void TableFn::read(ScienceAPI2 &P, string xName,string yName)
   {
   P.read(xName,"",true,x);
   P.read(yName,"",true,y);
   }
//------------------------------------------------------------------------------------------------
//----------- Table Function constructor
//------------------------------------------------------------------------------------------------
TableFn::TableFn(vector<double> xVec,vector<double> yVec)
   {
   x = xVec;
   y = yVec;
   }
//------------------------------------------------------------------------------------------------
//----------- Table Function load
//------------------------------------------------------------------------------------------------
void TableFn::load(vector<double> xVec,vector<double> yVec)
   {
   x = xVec;
   y = yVec;
   }
//------------------------------------------------------------------------------------------------
//------------ Return a y value from a table function
//------------------------------------------------------------------------------------------------
double TableFn::value(double v) const
   {
   // find which sector of the function that v falls in
   unsigned sector;
   if(x.size() == 0)return 0;             // /* TODO :  */fix: needs work so that this is never called
   for(sector = 0;sector < x.size();sector++)
      if(v < x[sector] || isEqual(v,x[sector]))break;

   if(sector == 0)return y[0];
   if(sector == x.size())return y[y.size()-1];
   if(isEqual(v,x[sector]))return y[sector]; // prevent roundoff errors
   // y = mx + c
   double slope = isEqual(x[sector],x[sector-1]) ? 0 :
                        (y[sector]-y[sector-1])/(x[sector]-x[sector-1]);

   return y[sector-1] + slope * (v - x[sector - 1]);
   }

/* TODO : Needs Work */
//==========================================================================
void accumulate (double value,             //  (INPUT) value to add to array
                 vector<double> &array,            //  (INPUT/OUTPUT) array to split
                 double p_index,           //  (INPUT) current p_index no
                 double dlt_index)         //  (INPUT) increment in p_index no
//==========================================================================
/* Purpose
*     Accumulates a value in an array, at the specified index.
*     If the increment in index value changes to a new index, the value
*     is distributed proportionately between the two indices of the array.
*
*  Mission Statement
*      Accumulate %1 (in array %2)
*
* Changes
*       090994 jngh specified and programmed
*       090795 jngh corrected aportioning of value
*       250996 jngh changed so it always adds instead of reset at changeover
*                    to new phase
*                    corrected to take account of special case when p_index
*                    is integral no.
*       210898 igh  added checking to make sure index > 0
*
* Calls
*/
   {
   // Local Variables
   int current_index;           // current index number ()
   double fract_in_old;           // fraction of value in last index
   double index_devel;            // fraction_of of current index elapsed ()
   int new_index;                // number of index just starting ()
   double portion_in_new;         // portion of value in next index
   double portion_in_old;         // portion of value in last index

   // (implicit) assert(dlt_index <= 1.0);
   current_index = int(p_index);

   // make sure the index is something we can work with
   if(current_index >= 0)
      {
      index_devel = p_index - floor(p_index) + dlt_index;
      if (index_devel >= 1.0)
         {
         // now we need to divvy
         new_index = (int) (p_index + Min(1.0, dlt_index));
         if (isEqual(fmod(p_index,1.0),0.0))
            {
            fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
            portion_in_old = fract_in_old * (value + array[current_index])-
                                 array[current_index];
            }
         else
            {
            fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
            portion_in_old = fract_in_old * value;
            }
         portion_in_new = value - portion_in_old;
         array[current_index] = array[current_index] + portion_in_old;
         array[new_index] = array[new_index] + portion_in_new;
         }
      else
         {
         array[current_index] = array[current_index] + value;
         }
      }
   else
      {
     throw std::runtime_error("accumulate index < 0!!");
      }
   }


/* TODO : Check all this */
double dayLength (int doy, double latitude, double twilight)
   {
   const double  aeqnox = 79.25 ;//  average day number of autumnal equinox
   const double  pi = 3.14159265359 ;
   const double  dg2rdn =  (2.0*pi) /360.0 ; // convert degrees to radians
   const double  decsol = 23.45116 * dg2rdn ; // amplitude of declination of sun
                                            //   - declination of sun at solstices.
   // cm says here that the maximum declination is 23.45116 or 23 degrees 27 minutes.
   // I have seen else_where that it should be 23 degrees 26 minutes 30 seconds - 23.44167
   const double  dy2rdn =  (2.0*pi) /365.25 ; // convert days to radians
   const double  rdn2hr = 24.0/(2.0*pi)  ; // convert radians to hours

   //+ Local Variables
   double alt;    // twilight altitude limited to max/min sun altitudes end of twilight
                  //   - altitude of sun. (radians)
   double altmn;  // altitude of sun at midnight
   double altmx;  // altitude of sun at midday
   double clcd;   // cos of latitude * cos of declination
   double coshra; // cos of hour angle - angle between the sun and the meridian.
   double dec;    // declination of sun in radians - this is the angular distance at solar
                  //   noon between the sun and the equator.
   double hrangl; // hour angle - angle between the sun and the meridian (radians).
   double hrlt;   // day_length in hours
   double latrn;  // latitude in radians
   double slsd;   // sin of latitude * sin of declination
   double sun_alt;// angular distance between sunset and end of twilight - altitude of sun. (radians)
   // Twilight is defined as the interval between sunrise or sunset and the time when the
   // true centre of the sun is 6 degrees below the horizon.
   // Sunrise or sunset is defined as when the true centre of the sun is 50' below the horizon.

   //- Implementation Section ----------------------------------

   sun_alt = twilight * dg2rdn;

   // calculate daylangth in hours by getting the solar declination (radians) from the day of year,
   // then using the sin and cos of the latitude.

   // declination ranges from -.41 to .41 (summer and winter solstices)

   dec = decsol*sin (dy2rdn* ((double)doy - aeqnox));

   // get the max and min altitude of sun for today and limit the twilight altitude between these.

   if (isEqual(fabs(latitude), 90.0))
      {
      //coshra = sign (1.0, -dec) * sign (1.0, lat); XXsign???
      }
   else
      {
      latrn = latitude*dg2rdn;
      slsd = sin(latrn)*sin(dec);
      clcd = cos(latrn)*cos(dec);

      altmn = asin(Min(Max(slsd - clcd, -1.0), 1.0));
      altmx = asin(Min(Max(slsd + clcd, -1.0), 1.0));
      alt = Min(Max(sun_alt, altmn), altmx);

      // get cos of the hour angle
      coshra = (sin (alt) - slsd) /clcd;
      coshra = Min(Max(coshra, -1.0), 1.0);
      }

   // now get the hour angle and the hours of light
   hrangl = acos (coshra);
   hrlt = hrangl*rdn2hr*2.0;
   return hrlt;
   }
//------------------------------------------------------------------------------------------------
//------- Calculate change in %3 based on fractional decay rates.
//------------------------------------------------------------------------------------------------
void calcPoolFractionDelta (int numParts, vector<double> fraction, vector<double> pool,
        vector<double> &dltPool)
   {
   for(int i = 0; i < numParts; i++)
      {
      dltPool[i] = pool[i] * fraction[i];
      }
   }
//------------------------------------------------------------------------------------------------
//------- Calculate change in a particular plant pool
//------------------------------------------------------------------------------------------------
void calcPartFractionDelta (int partNo, vector<double> fraction, double part,
      double &dltPart)
   {
   dltPart = part * fraction[partNo];
   }
//------------------------------------------------------------------------------------------------
//------- convert string names from start_grain_fill to StartGrainFill
string convertName(string name)
   {
   // go through and capilise the first letter and any letter following an underscore
   // removing underscores
   string newName;
   bool upper = false;
   for(unsigned i=0;i < name.size();i++)
      {
      if(i==0 || upper)
         {
         newName.push_back((char)toupper(name[i]));
         upper = false;
         }
      else if(name[i] == '_')
         upper = true;
      else
         newName.push_back(name[i]);
      }
   return newName;
   }
//------------------------------------------------------------------------------------------------
void DVecToFVec(vector<float> &fVec, vector<double> dVec)
   {
   fVec.clear();
   for(unsigned i = 0; i < dVec.size(); i++)
      {
      fVec.push_back((float)dVec[i]);
      }
   }
//------------------------------------------------------------------------------------------------
void FVecToDVec(vector<double> *dVec, vector<float> fVec)
   {
   (*dVec).clear();
   for(unsigned i = 0; i < fVec.size(); i++)
      {
      (*dVec).push_back(fVec[i]);
      }
   }
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
double DegToRad(double degs)
{
	return degs * PI / 180.0;
}
//------------------------------------------------------------------------------------------------
double RadToDeg(double rads)
{
	return rads * 180.0 / PI;
}
//-----