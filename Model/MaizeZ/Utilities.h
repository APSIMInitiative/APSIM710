//---------------------------------------------------------------------------

#ifndef UtilitiesH
#define UtilitiesH


#include <vector>
#include <string>
#include <math.h>

#include "conversions.h"
using namespace std;
#define M_PI 3.1415

class ScienceAPI2;

//class ScienceAPI;

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
double sumVector(vector<double> vec);
float avgVector(vector<float> vec);
float movingAvgVector(vector<float> &vec, int sz);
float maxVector(vector<float> vec);
double maxVector(vector<double> vec);
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
// diurnal stuff

void HourlyTemperature(float maxT, float minT, vector<double> TAirParam, int doy,
                                 float LatR, vector<double> &Temperature);

void CalcRadiation( float ApsimRad, int doy, double LatR, vector<double> &rad);
float CalcSolarDeclination(int doy);
float CalcDayLength(float LatR, float SolarDec);
float CalcSolarRadn(float RATIO,float DayL,float LatR,float SolarDec); // solar radiation
float GlobalRadiation(float oTime, float latitude, float SolarDec, float DayLH, float Solar);
void CalcSVP(vector<double> TAir, vector<double> &SVP);
float CalcSVP(float Tair);
void CalcRH(double tMax, double tMin, vector<double> SVP, double RHMax,double RHMin,
               vector <double> &RH);
void CalcVPDair(vector<double> TAir, vector<double> RH, vector<double> &VPDair);
void CalcTDemand(vector <double> hRadn, float cover, float RUE, float TEc, vector<double> VPDair,
                float sw_demand, vector<double> &Td);
void CalcTSupply(float dailySupply,vector <double> demand, vector <double> &supply);
float CalcPsi(float ftsw);

void CalcTLeaf(vector <float> supply, vector <float> demand, TableFn *LeafTemp,
                                                               vector <float> &TLeaf);
void CalcVPDairLeaf(vector <float> TAir, vector <float> TLeaf, vector <float> RH,
         vector <float> &VPDairLeaf);
void CalcVPDeq(vector <float> hRadn, vector <float> VPDairLeaf, vector <float> &VPDeq);
void CalcHLER(float T0, float a, float b, float c, vector <float> TLeaf,
         vector <float> VPDairLeaf, float psi, vector <float> &LER);

//------------------------------------------------------------------------------------------------

#endif
