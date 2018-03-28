//---------------------------------------------------------------------------

#ifndef UtilitiesH
#define UtilitiesH


#include <vector>
#include <string>
#include <math.h>

#include "conversions.h"
using namespace std;
#ifndef M_PI
#define M_PI 3.1415
#endif

class ScienceAPI2;

//class ScienceAPI;

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

int   findIndex(double, std::vector<double>);
int   CalendarToJulian(int, int, int);
double layerProportion(std::vector<double>,double,int);
double sumVector(vector<double>);
double avgVector(vector<double>);
double movingAvgVector(vector<double>&, int);
double maxVector(vector<double>);
double sumVector(vector<double>, int);
double sumVector(vector<double>, int, int);
double divide (double, double, double default_value = 0.0);
double bound(double, double, double);
double dayLength (int, double, double);
void  fillVector(vector<double>&,std::vector<double>&);
void  accumulate (double, std::vector<double>&, double, double);
void  calcPoolFractionDelta (int, std::vector<double>, std::vector<double>, std::vector<double>&);
void  calcPartFractionDelta (int, std::vector<double>, double, double&);
void  JulianToCalendar(double,int&,int&,int&);
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
//------------------------------------------------------------------------------------------------
class TableFn
   {
   public:
      std::string xName,yName;
      std::vector<double> x;
      std::vector<double> y;

      TableFn(void){};
      TableFn(ScienceAPI2&,  std::string, std::string);
      TableFn(std::vector<double>, std::vector<double>);
      void  read(ScienceAPI2 &api,  std::string xName, std::string);
      void  load(std::vector<double>,std::vector<double>);
      double value(double) const;
   };
//------------------------------------------------------------------------------------------------
// diurnal routines
void HourlyTemperature(double, double, vector<double>, int, double, vector<double>&);
void CalcRadiation( double, int, double, vector<double>&);
void CalcSVP(vector<double>, vector<double>&);
void CalcRH(double, double, vector<double>, double, double, vector <double>&);
void CalcVPDair(vector<double>, vector<double>, vector<double>&);
void CalcTDemand(vector<double>, double , double, double, vector<double>, double, vector<double>&);
void CalcTSupply(double,vector<double>, vector <double>&);
void CalcTLeaf(vector<double>, vector<double>, TableFn*, vector<double>&);
void CalcVPDairLeaf(vector<double>, vector<double>, vector<double>, vector<double>&);
void CalcVPDeq(vector<double>, vector<double>, vector<double>&);
void CalcHLER(double, double, double, double, vector<double>, vector<double>, double, vector<double>&);
double CalcSolarDeclination(int);
double CalcDayLength(double, double);
double CalcSolarRadn(double, double, double, double); // solar radiation
double GlobalRadiation(double, double, double, double, double);
double CalcSVP(double);
double CalcPsi(double);

void DVecToFVec(vector<float>&, vector<double>);
void FVecToDVec(vector<double>*, vector<float>);

//------------------------------------------------------------------------------------------------

#endif
