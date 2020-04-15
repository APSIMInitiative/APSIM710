//---------------------------------------------------------------------------

#ifndef UtilitiesH
#define UtilitiesH


#include <vector>
#include <string>
#include <math.h>

#include "conversions.h"
using namespace std;
#define PI 3.14159265358979
#define M_PI 3.14159

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
double valVector(vector<double>, double);
double divide (double, double, double default_value = 0.0);
double bound(double, double, double);
double dayLength (int, double, double);
void  fillVector(vector<double>&,std::vector<double>&);
void  accumulate (double, std::vector<double>&, double, double);
void  calcPoolFractionDelta (int, std::vector<double>, std::vector<double>, std::vector<double>&);
void  calcPartFractionDelta (int, std::vector<double>, double, double&);
void  JulianToCalendar(double,int&,int&,int&);
string convertName(string name);
double DegToRad(double);
double RadToDeg(double);
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

void DVecToFVec(vector<float>&, vector<double>);
void FVecToDVec(vector<double>*, vector<float>);

//------------------------------------------------------------------------------------------------
// diurnal stuff
void HourlyTemperature(double maxT, double minT, vector<double> TAirParam, int doy,
                       double LatR, vector<double> &Temperature);
void CalcRadiation( double ApsimRad, int doy, double LatR, vector<double> &rad);
double CalcSolarDeclination(int doy);
double CalcDayLength(double LatR, double SolarDec);
double CalcSolarRadn(double RATIO,double DayL,double LatR,double SolarDec); // solar radiation
double GlobalRadiation(double oTime, double latitude, double SolarDec, double DayLH, double Solar);
void GlobalRadiation(double oTime, double latitude, double SolarDec, double DayLH, double Solar,
                     double &sunAngle, double &ITot, double &IDiff, double &IDir);
void CalcSVP(vector<double> TAir, vector<double> &SVP);
double CalcSVP(double Tair);
void CalcRH(double tMax, double tMin, vector<double> SVP, double RHMax,double RHMin,
            vector <double> &RH);
void CalcVPDair(vector<double> TAir, vector<double> RH, vector<double> &VPDair);
void CalcTDemandSupply(vector <double> hRadn, double cover, double RUE, double TEc, vector<double> VPDair,
                       double maxFlux, vector<double> Hbiomass, bool didCanopy, vector<double> &Tdemand, vector<double> &TdemandFluxLimited);
void CalcLimitTDemandSupply(vector <double> Hbiomass, double cover, double TEc, vector<double> VPDair,
                       double maxTrans, double maxTransEffect, vector<double> &Tdemand, vector<double> &TdemandFluxLimited);
void CalcTSupply(double dailySupply,vector <double> demand, vector <double> &supply);
double CalcPsi(double);
void CalcTLeaf(vector<double>, vector<double>, TableFn*, vector<double>&);
void CalcVPDeq(vector<double>, vector<double>, vector<double>&);
void CalcVPDairLeaf(vector<double>, vector<double>, vector<double>, vector<double>&);
void calcHourlyWeatherVars(double latitude,int doy, double radn, double maxT, double minT, vector<double> &TAirParam,
                    vector<double> &hRadn, vector<double> &TAir, vector<double> &SVP, vector<double> &RH, vector<double> &VPDair);
//------------------------------------------------------------------------------------------------
#endif
