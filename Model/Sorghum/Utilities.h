//---------------------------------------------------------------------------

#ifndef UtilitiesH
#define UtilitiesH


#include <vector>
#include <string>
#include <math.h>

#include "conversions.h"
using namespace std;

#define PI 3.14159265358979



class ScienceAPI2;

//------------------------------------------------------------------------------------------------


//      crop status names
typedef enum {out, dead, alive} Status;
//typedef enum {warning, fatal} errSeverity;
inline bool isEqual(double A, double B, double C) {return(fabs(A-B)<C);}
inline bool isEqual(double A, double B) {return(fabs(A-B)<1.0E-6);}

#define Max(a, b)  (((a) > (b)) ? (a) : (b))
#define Min(a, b)  (((a) < (b)) ? (a) : (b))

//------------------------------------------------------------------------------------------------
void checkRange(ScienceAPI2 &api, double value, double lower, double upper, const std::string &msg);

int   findIndex(double value, std::vector<double> items);
void  fillVector(vector<double> &temp,std::vector<double> &newVect);
double layerProportion(std::vector<double> dLayer,double rootDepth,int rootLayer);

double sumVector(vector<double>);
double avgVector(vector<double>);
double movingAvgVector(vector<double>&, int);
double maxVector(vector<double>);
double sumVector(vector<double>, int);
double sumVector(vector<double>, int, int);

double divide (double dividend, double divisor, double default_value = 0.0);
double bound(double value,double lower, double upper);
double dayLength (int doy, double latitude, double twilight);

void  accumulate (double value, std::vector<double> &array, double p_index, double dlt_index);
void  calcPoolFractionDelta (int numParts, std::vector<double> fraction, std::vector<double> pool,
        std::vector<double> &dltPool);

void  calcPartFractionDelta (int partNo, std::vector<double> fraction, double part,
      double &dltPart);


void  JulianToCalendar(double jDay,int &day,int &month,int &year);
int   CalendarToJulian(int day,int month,int year);

string convertName(string name);


//------------------------------------------------------------------------------------------------
class Today
   {
   public:
   int   doy;                      // (Julian) day number of year
   int   day, month, year;         // day of month etc..
   double radn;                     // solar radiation (Mj/m^2/day)
   double minT;                     // minimum air temperature (oC)
   double maxT;                     // maximum air temperature (oC)
   double avgT;                     // average air temperature (oC)
   double rain;                     // rain in mm
   double vp;                       // VP
   double getPhotoPeriod(double latitude,double twilight){return dayLength (doy,latitude,twilight);}
   };
//------------------------------------------------------------------------------------------------
// class to handle table functions

class TableFn
   {
   public:
   std::string xName,yName;
   std::vector<double> x;
   std::vector<double> y;

   TableFn(void){};
   TableFn(ScienceAPI2 &api,  std::string xName, std::string yName);
   TableFn(std::vector<double> xVec,std::vector<double> yVec);
   void  read(ScienceAPI2 &api,  std::string xName, std::string yName);
   void  load(std::vector<double> xVec,std::vector<double> yVec);
   double value(double v) const;
   };
//------------------------------------------------------------------------------------------------

void DVecToFVec(vector<float>&, vector<double>);
void FVecToDVec(vector<double>*, vector<float>);

double DegToRad(double degs);
double RadToDeg(double rads);


#endif
