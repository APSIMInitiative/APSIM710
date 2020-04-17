//---------------------------------------------------------------------------

#ifndef CanopyPhotosynthesisH
#define CanopyPhotosynthesisH

#include <vector>
#include <string>

namespace Maize {

class LayerModel;
class Plant;
class PlantInterface;
//---------------------------------------------------------------------------
class CanopyPhotosynthesis   : public PlantPart
   {
   private:

   public:
      CanopyPhotosynthesis(ScienceAPI2 &,Plant* p);
      ~CanopyPhotosynthesis(void);

      Plant *plant;
      PlantInterface *plantInterface;
      void phenologyEvent(int iStage);
      double calcNDemand(void);
      double calcPDemand(void);
      void updateVars(void);

      void initialize(void);
      void readParams (void);
      void doRegistrations(void);

      void calcVars(double, double, double, double, int, double*, double*);
      void calcSLNGradient(double);
      void canopyPhoto(double);
      void dailyCanopyPInt(double);

      double calcShadeRatio(double);

      // Parameters ----------------------------------------------------------
      std::vector<double> leafAngles;

      double pMax;          //asymptote of Pmax - SLN response curve
      double n0;            //a fitted coefficient in PMAX equation
      double sunAngle;      //sun angle (solar elevation above horizon, in radian)
      double biomConv;      //conversion coefficient for CHO to biomass g/g
      double scatterCoef;   //leaf light scattering coefficient
      double slnHeadPC;     //the difference as a percentage of the sln at the top layer compared to the middle
      
      double freq;          //frequency of the calculation (s)
      int nLayers;         //number of canopy layers
      double alpha;         //a fitted coefficient in PMAX equation

      //StateVars
      int day;             //day number fo year
      double latR;          //latitude in radians
      //double latD;          //latitude in degrees
      double solarDec;      //solar declination (Radians)
      double dayL;          //daylength (radians)
      double dayLH;         //daylength (hours)
      double solarRad;      //extra-terrestrial irradiance MJ/m2/s
      double solarRadG;     //solar radiation at the ground
      double IMax;          //solar noon total incoming solar radiation MJ/m2/s
      double ITot;          //total incoming solar radiation MJ/m2/s
      double IDir ;         //direct beam incomign solar radiation MJ/m2/s
      double IDiff;         //diffuse incoming solar radiation MJ/m2/s

      double biomassCan;    //canopy assimilation g/m2/sec
      double laiCan;        //LAI of whole canopy
      double radInt;        //canopy radiation interception MJ/m2/sec
      double biomassDay;    //canopy assimilation g/m2/day
      double radIntDay;     //canopy radiation interception MJ/m2/day
      double rueDay;        //RUE for whole canopy for the day g/MJ
      double radDirDay;     //direct beam solar radiation MJ/m2/day
      double radDiffDay;    //diffuse solar radiation MJ/m2/day

      //LayerModels
      LayerModel *laiLayers;
      LayerModel *slnLayers;
      LayerModel *leafAngleLayers;

      // Calculated in CanopyP (time dependent)
      std::vector<double> slns;
      std::vector<double> lais;

      std::vector<double> rue;          //radiation use efficiency g/MJ; instantaneous
      std::vector<double> laiSunLit;    //sunlit LAI
      std::vector<double> laiSH;        //shaded LAI
      std::vector<double> ISunLit;      //light intensity on sunlit leaves MJ/m2/s /m2 leaf area
      std::vector<double> ISH;          //light intensity on shaded leaves MJ/m2/s/m2 leaf area
      std::vector<double> PMax;         //asymptote of photosynthesis-light response curve mg/m2 leaf area/s
      std::vector<double> sumLAI;       //cumulative LAI from top of canopy m2/m2
      std::vector<double> sumF;         //proportion light intercepted to that point in the canopy
      std::vector<double> F;            //proportion light intercepted in canopy layer
      std::vector<double> sumLAISunLit; //cumulative sunlit LAI from top of canopy m2/m2
      std::vector<double> cSunLit;      //photosynthesis by sunlit leaves mg/m2/s
      std::vector<double> cSH;          //photosynthesis by shaded leaves mg/m2/s
      std::vector<double> biomass;      //canopy assimilation g/m2/sec
      std::vector<double> K;            //extinction coefficient
      std::vector<double> G;            //shade projection coefficient

      std::vector<double> IBiomass;     //canopy instanteneous photosynthesis rate g/m2/s
      std::vector<double> IRadInt;      //canopy instanteneous radiation intercepted MJ/m2/s
      std::vector<double> IDirRad;      //canopy instanteneous incoming direct radiation g/m2/s
      std::vector<double> IDiffRad;     //canopy instanteneous incomeing diffuse radiation MJ/m2/s

      std::vector<double> HBiomass;     //canopy hourly photosynthesis rate g/m2/s
      std::vector<double> HRadInt;      //canopy hourly radiation intercepted MJ/m2/s
      std::vector<double> HDirRad;      //canopy hourly incoming direct radiation g/m2/s
      std::vector<double> HDiffRad;     //canopy hourly incomeing diffuse radiation MJ/m2/s

      bool didCalc;

      //Getters
      bool didCalcCanopyPhoto() { return didCalc; };
      double getInstantCanopyBiomass(int timeStep);
      double getInstantCanopyRadInt(int timeStep);
      double getInstantDirectRad(int timeStep);
      double getInstantDiffuseRad(int timeStep);
      double getHourlyCanopyBiomass(int timeStep);
      const std::vector<double>& getHourlyCanopyBiomass() { return HBiomass; };
      double getHourlyCanopyRadInt(int timeStep);
      double getHourlyDirectRad(int timeStep);
      double getHourlyDiffuseRad(int timeStep);
      double getCanopyBiomass(void);
      double getCanopyRadInt(void);
      double getDirectRad(void);
      double getDiffuseRad(void);
      double getTotalRad(void);
      double getMaxRad(void);
      double getSunAngle(void);
      double getSolarDecl(void);
      double getDayLength(void);
      double getDayLengthInHour(void);
      double getSolarRad(void);
      double getGroundSolarRad(void);
      double getLayerRUE(int Layer);
      double getLayerLAISUN(int Layer);
      double getLayerLAISH(int Layer);
      double getLayerISunLit(int Layer);
      double getLayerISH(int Layer);
      double getLayerPPMax(int Layer);
      double getLayerSumLAI(int Layer);
      double getLayerSumF(int Layer);
      double getLayerF(int Layer);
      double getLayerLIASunLit(int Layer);
      double getLayerCSun(int Layer);
      double getLayerCSH(int Layer);
      double getLayerBiomass(int Layer);
      double getLayerK(int Layer);
      double getLayerG(int Layer);
      double getDailyCanopyBiomass(void);
      double getDailyCanopyRadInt(void);
      double getDailyCanopyRUE(void);
      double getDailyDirectRad(void);
      double getDailyDiffuseRad(void);
      double getDailyExtictCoef(void);
   };
}
#endif


