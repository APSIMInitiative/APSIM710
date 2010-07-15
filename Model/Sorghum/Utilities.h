//---------------------------------------------------------------------------

#ifndef UtilitiesH
#define UtilitiesH


#include <vector>
#include <string>
#include <math.h>

#include "conversions.h"
using namespace std;


class ScienceAPI2;

//------------------------------------------------------------------------------------------------


//      crop status names
typedef enum {out, dead, alive} Status;
//typedef enum {warning, fatal} errSeverity;
inline bool isEqual(float A, float B, float C) {return(fabs(A-B)<C);}
inline bool isEqual(float A, float B) {return(fabs(A-B)<1.0E-6);}

#define Max(a, b)  (((a) > (b)) ? (a) : (b))
#define Min(a, b)  (((a) < (b)) ? (a) : (b))

//------------------------------------------------------------------------------------------------
void checkRange(ScienceAPI2 &api, float value, float lower, float upper, const std::string &msg);

int   findIndex(float value, std::vector<float> items);
void  fillVector(vector<float> &temp,std::vector<float> &newVect);
float layerProportion(std::vector<float> dLayer,float rootDepth,int rootLayer);
float sumVector(vector<float> vec);
float avgVector(vector<float> vec);
float movingAvgVector(vector<float> &vec, int sz);
float maxVector(vector<float> vec);
float sumVector(vector<float> vec, int index);
float sumVector(vector<float> vec, int from, int to);
float divide (float dividend, float divisor, float default_value = 0.0);
float bound(float value,float lower, float upper);
float dayLength (int doy, float latitude, float twilight);

void  accumulate (float value, std::vector<float> &array, float p_index, float dlt_index);
void  calcPoolFractionDelta (int numParts, std::vector<float> fraction, std::vector<float> pool,
        std::vector<float> &dltPool);

void  calcPartFractionDelta (int partNo, std::vector<float> fraction, float part,
      float &dltPart);


void  JulianToCalendar(float jDay,int &day,int &month,int &year);
int   CalendarToJulian(int day,int month,int year);

string convertName(string name);


//------------------------------------------------------------------------------------------------
class Today
   {
   public:
   int   doy;                      // (Julian) day number of year
   int   day, month, year;         // day of month etc..
   float radn;                     // solar radiation (Mj/m^2/day)
   float minT;                     // minimum air temperature (oC)
   float maxT;                     // maximum air temperature (oC)
   float avgT;                     // average air temperature (oC)
   float rain;                     // rain in mm
   float vp;                       // VP
   float getPhotoPeriod(float latitude,float twilight){return dayLength (doy,latitude,twilight);}
   };
//------------------------------------------------------------------------------------------------
// class to handle table functions

class TableFn
   {
   public:
   std::string xName,yName;
   std::vector<float> x;
   std::vector<float> y;

   TableFn(void){};
   TableFn(ScienceAPI2 &api,  std::string xName, std::string yName);
   TableFn(std::vector<float> xVec,std::vector<float> yVec);
   void  read(ScienceAPI2 &api,  std::string xName, std::string yName);
   void  load(std::vector<float> xVec,std::vector<float> yVec);
   float value(float v) const;
   };
//------------------------------------------------------------------------------------------------
#endif
