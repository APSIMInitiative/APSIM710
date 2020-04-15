//---------------------------------------------------------------------------
#include "Plant.h"
#include "CanopyPhotosynthesis.h"
#include "LayerModel.h"
//#include <system.hpp>

using namespace Maize;


//---------------------------------------------------------------------------
// Hammer, G. L. and Wright, G. C. 1994. A theoretical analysis of nitrogen and radiation
// effects on radiation use efficiency in peanut. Aust. J. Agric. Res. 45:575-589
//
// This class module implements the framework for prediction of RUE proposed by the above authors
//
// Written by Zhanshan Dong in September 2005
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
//Constructor
//---------------------------------------------------------------------------
CanopyPhotosynthesis::CanopyPhotosynthesis(ScienceAPI2 &api, Plant *p) : PlantPart(api)
   {
   plant = p;

   readParams();
   doRegistrations();
   initialize();
   }
//---------------------------------------------------------------------------
void CanopyPhotosynthesis::phenologyEvent(int iStage)
   {
   }
double CanopyPhotosynthesis::calcNDemand(void)
   {
   return 0.0;
   }
double CanopyPhotosynthesis::calcPDemand(void)
   {
   return 0.0;
   }
void CanopyPhotosynthesis::updateVars(void)
   {
   }
//---------------------------------------------------------------------------

void CanopyPhotosynthesis::initialize()
   {
   slnLayers = NULL;
   laiLayers = NULL;
   leafAngleLayers = NULL;
   didCalc = false;

   rueDay = 0.0;
   biomassDay = 0.0;
   radIntDay = 0.0;
   radDirDay = 0.0;
   radDiffDay = 0.0;

   for(int i = 0; i < nLayers; i++)
      {
      slns.push_back(0.0);
      lais.push_back(0.0);
      rue.push_back(0.0);
      laiSunLit.push_back(0.0);
      laiSH.push_back(0.0);
      ISunLit.push_back(0.0);
      ISH.push_back(0.0);
      PMax.push_back(0.0);
      sumLAI.push_back(0.0);
      sumF.push_back(0.0);
      F.push_back(0.0);
      sumLAISunLit.push_back(0.0);
      cSunLit.push_back(0.0);
      cSH.push_back(0.0);
      biomass.push_back(0.0);
      K.push_back(0.0);
      G.push_back(0.0);
      }
   }
//---------------------------------------------------------------------------
void CanopyPhotosynthesis::readParams (void)
   {
   scienceAPI.read("pMax",   "", 0, pMax);
   scienceAPI.read("n0",   "", 0, n0);
   scienceAPI.read("alpha",   "", 0, alpha);
   scienceAPI.read("BiomassConv",   "", 0, biomConv);
   scienceAPI.read("scatterCoef",   "", 0, scatterCoef);
   scienceAPI.read("slnHeadPC",   "", 0, slnHeadPC);
   
   scienceAPI.read("frequencyS",   "", 0, freq);
   scienceAPI.read("LeafAngles",   "", 0, leafAngles);

   nLayers = leafAngles.size();
   }
//---------------------------------------------------------------------------
void CanopyPhotosynthesis::doRegistrations()
   {

      scienceAPI.expose("RUE_L", "g/MJ", "RUE calculated by layered canopy",false, rueDay);
      scienceAPI.expose("biomass_L", "g/m2/day", "canopy assimilation",false, biomassDay);
      scienceAPI.expose("rad_int_L", "MJ/m2/day", "canopy radiation interception", false,radIntDay);
      scienceAPI.expose("dir_rad_L", "MJ/m2/day", "direct beam solar radiation", false,radDirDay);
      scienceAPI.expose("diff_rad_L", "MJ/m2/day", "diffuse solar radiation", false,radDiffDay);
   }
//---------------------------------------------------------------------------
void CanopyPhotosynthesis::calcVars(double sln, double lai,  double latD, double rad,
         int day, double *rueday, double *radday)
   {
   didCalc = false;
   /* TODO : Check - Is this number too high (0.5) Try 0.3*/
   nLayers = leafAngles.size(); // was initiated in void CanopyPhotosynthesis::readParams (string cultivar)
   /*if(lai / double(nLayers) < 0.3)
      {
      return;
//  Commented out by zhanshan, Oct 30, 2007
//  Reason: the model returns errors with nLayers = 1
//      nLayers = 1;
      }*/
/*   while(lai / double(nLayers) < 0.3)
      {
//  Commented out by zhanshan, Oct 30, 2007
//  Reason: the model returns errors with nLayers = 1
      nLayers--;
      if (nLayers == 1) return;
      }*/

   calcSLNGradient(sln);
   for(int i = 0; i < nLayers; i++)
      {
      lais[i] = lai / double(nLayers);
      }
   latR = DegToRad(latD);

   solarDec = CalcSolarDeclination(day);
   dayL = CalcDayLength(latR, solarDec);    // radians
   dayLH = RadToDeg(2.0 / 15.0 * dayL);     // hours
   double RATIO = 0.75; //Hammer, Wright (Aust. J. Agric. Res., 1994, 45)
   solarRad = CalcSolarRadn(RATIO,latR, dayL, solarDec);
   solarRadG = rad;
   IMax = solarRadG * (1.0 + sin(2.0 * PI * 0.5 + 1.5 * PI)) / (dayLH * 60.0 * 60.0);

   //Create LayerModels
   if(slnLayers)
      {
      delete slnLayers;
      delete laiLayers;
      delete leafAngleLayers;
      }

   double *Layers = new double[nLayers];
   for(int i = 0; i < nLayers; i++)
      {
      Layers[i] = i + 1;
      }

   slnLayers = new LayerModel(Layers, &slns[0], nLayers);
   laiLayers = new LayerModel(Layers, &lais[0], nLayers);
   leafAngleLayers = new LayerModel(Layers, &leafAngles[0], nLayers);

   delete []Layers;

   dailyCanopyPInt(freq);

   *rueday = rueDay;
   *radday = radIntDay;
   
   didCalc = true;
   }
//---------------------------------------------------------------------------
void CanopyPhotosynthesis::calcSLNGradient(double sln)
   {
   double slnTop = sln + sln * slnHeadPC / 100.0;
	double slnBot = sln - sln * slnHeadPC / 100.0;

   if(nLayers > 1)
      {
         double slnInc = (slnTop - slnBot) / (nLayers - 1);

      slns[0] = slnTop;

      for (int i = 1; i < nLayers; i++)
         {
         slns[i] = slns[i - 1] - slnInc;
         }
      }
   //Overide      /* TODO: override this by setting the slnHeadPC to 0 */
   for(int i = 0; i < nLayers; i++)
      {
      slns[i] = 1.0;
      }
   }
//---------------------------------------------------------------------------
//Destructor
//---------------------------------------------------------------------------
CanopyPhotosynthesis::~CanopyPhotosynthesis(void)
   {
   };
//---------------------------------------------------------------------------
// Calculate light intercepted and sunlit and shaded leaf area for each canopy layer
// Calculation by canopy layers; layer 0 is at top of canopy
//---------------------------------------------------------------------------
void CanopyPhotosynthesis::canopyPhoto(double oTime)
   {
   // Calculate global radiation from latitude, day of year, and time of day
   GlobalRadiation(oTime,latR,solarDec,dayLH,solarRadG,sunAngle,ITot,IDiff,IDir);

   // Initialize variables
   for(int i = 0; i < nLayers; i++)
      {
      sumLAI[i] = 0;
      sumF[i] = 0;
      sumLAISunLit[i] = 0;
      ISunLit[i] = 0;
      G[i] = calcShadeRatio(leafAngleLayers->GetVal(i + 1));
      K[i] = G[i] / sin(sunAngle);
      }

   // Calculate light intercepted and sunlit and shaded leaf area for each canopy layer
   sumLAI[0] = laiLayers->GetVal(1);
   sumF[0] = 1.0 - exp(-K[0] * sumLAI[0]);
   F[0] = sumF[0];
   sumLAISunLit[0] = sumF[0] / K[0];
   laiSunLit[0] = sumLAISunLit[0];
   laiSH[0] = laiLayers->GetVal(1) - laiSunLit[0];

   for(int i = 1; i < nLayers; i++)
      {
      sumLAI[i] = sumLAI[(i - 1)] + laiLayers->GetVal(i + 1);
      sumF[i] = 1.0 - exp(-K[i] * sumLAI[i]);
      F[i] = Max(sumF[i] - sumF[(i - 1)], 0.000001);
      sumLAISunLit[i] = sumF[i] / K[i];
      laiSunLit[i] = Max(sumLAISunLit[i] - sumLAISunLit[i - 1], 0.000001);
      laiSH[i] = laiLayers->GetVal(i + 1) - laiSunLit[i];
      }

   laiCan = sumLAI[(nLayers - 1)];

   // Calculate light intensity for sunlit and shaded leaf area for each canopy layer
   for(int i = 0; i < nLayers; i++)
      {
      ISunLit[i] = IDir * F[i] / laiSunLit[i] + IDiff * F[i] / laiLayers->GetVal(i + 1);
      }

   ISH[0] = IDiff * F[0] / laiLayers->GetVal(1) +
            scatterCoef * (ISunLit[0] * laiSunLit[0]) / (laiSH[0] + laiSH[1]);

   for(int i = 1; i < nLayers - 1; i++)
      {
      ISH[i] = IDiff * F[i] / laiLayers->GetVal(i + 1) +
            scatterCoef * (ISunLit[(i - 1)] * laiSunLit[(i - 1)]) / (laiSH[(i - 1)] + laiSH[i]) +
            scatterCoef * (ISunLit[i] * laiSunLit[i]) / (laiSH[i] + laiSH[(i + 1)]);
      }
// Maybe we need reconsider about comment this line out???!!!!
// The following code is not equivalent to the one in the FOR-loop, Zhanshan, Oct 17, 2007
   ISH[(nLayers - 1)] = IDiff * F[(nLayers -1)] / laiLayers->GetVal(nLayers) +
            scatterCoef * (ISunLit[(nLayers - 2)] * laiSunLit[(nLayers - 2)]) / (laiSH[(nLayers - 2)] + laiSH[(nLayers - 1)]) +
            scatterCoef * (ISunLit[(nLayers - 1)] * laiSunLit[(nLayers - 1)]) / laiSH[(nLayers - 1)];

   // calculate RUE for each canopy layer from photosynthesis of sunlit and shade leaves in each layer

   for(int i = 0; i < nLayers; i++)
      {
      PMax[i] = pMax * (2.0 / (1.0 + exp(-alpha * (slnLayers->GetVal(i + 1) - n0))) - 1.0);
      cSunLit[i] = laiSunLit[i] * PMax[i] * (1.0 - exp(-5000.0 * ISunLit[i] / PMax[i]));
      cSH[i] = laiSH[i] * PMax[i] * (1.0 - exp(-5000.0 * ISH[i] / PMax[i]));
      if(ITot)
         {
         rue[i] = biomConv / 1000.0 * (cSunLit[i] + cSH[i]) / (F[i] * ITot);
         }
      biomass[i] = biomConv / 1000.0 * (cSunLit[i] + cSH[i]);
      }

   // calculate assimilation and radiation intercepted for the entire canopy
   biomassCan = 0;
   for(int i = 0; i < nLayers; i++)
      {
      biomassCan += biomass[i];
      }
   radInt = sumF[(nLayers - 1)] * ITot;
   }

//---------------------------------------------------------------------------
// Duncan et al. 1967. Simulating photosynthesis in plant communities. Hilgardia, 38(4):181-205
// This function implements the shaderatio function in Appendix A
// Written by Zhanshan Dong in Septempber 2005
//
// LA - leaf angle (degree)
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::calcShadeRatio(double LA)
   {
   // Duncan et al.
   double sunAngleR;  //Solar elevation (radians)
   double beta;    //leaf angle (radians)
   double theta0;  //(radians)
   double SR;
   sunAngleR = sunAngle;
   beta = DegToRad(90.0-LA);  // relative to vertical line perpendicular to horizon
   if(beta <= sunAngleR)
      {
      SR = cos(beta) * sin(sunAngleR);
      }
   else
      {
      theta0 = acos(1.0 / tan(beta) * tan(sunAngleR));
      SR = (2.0 / PI * sin(beta) * cos(sunAngleR) * sin(theta0) +
            ((90.0 - RadToDeg(theta0)) / 90.0) * cos(beta) * sin(sunAngleR));
      }
   return SR;
   }
//---------------------------------------------------------------------------
// Calculate canopy photosynthesis by euler integration
// Calculate BIO, RAD & RUE for the day; Gaussian integration
// calculate DIRRAD & DIFRAD for the day; Gaussian integration
// TimeStep in seconds
// 
//---------------------------------------------------------------------------
void CanopyPhotosynthesis::dailyCanopyPInt(double timeStep)
   {
   double fullDayLength = 24.0 * 60 * 60 / timeStep;
   int fullDayLengthInt = int(fullDayLength);
   double DayLength = dayLH * 60.0 * 60 / timeStep;
   int DayLengthInt = int(DayLength);

   double DuskDawnFract = (DayLength - DayLengthInt) / 2; //the remainder part of the hour at dusk and dawn
   int DawnTime = int((12.0 - (dayLH / 2.0)) * (60.0 * 60.0) / timeStep);
   double oTime = 0.0;

   IBiomass.clear();
   IRadInt.clear();
   IDirRad.clear();
   IDiffRad.clear();
   for(int j = 0; j < fullDayLengthInt ; j++)
      {
      IBiomass.push_back(0.0);
      IRadInt.push_back(0.0);
      IDirRad.push_back(0.0);
      IDiffRad.push_back(0.0);
      }

   //first partial
   double DuskDawnPart = DuskDawnFract/DayLength;
   double stepFract = 1.0/double(DayLengthInt);
   canopyPhoto(DuskDawnPart);
   if (biomassCan == biomassCan && radInt == radInt && IDir == IDir && IDiff == IDiff) // Test For NaN
      {
      IBiomass[DawnTime] = biomassCan * DuskDawnFract;
      IRadInt[DawnTime]  = radInt     * DuskDawnFract;
      IDirRad[DawnTime]  = IDir       * DuskDawnFract;
      IDiffRad[DawnTime] = IDiff      * DuskDawnFract;
      }
   //Run Through Whole Day
   for(int i = 0; i < DayLengthInt -1; i++)
      {
      oTime = DuskDawnPart + ((i + 1) * stepFract);
      canopyPhoto(oTime);
      IBiomass[DawnTime + i + 1] = biomassCan;
      IRadInt[DawnTime + i + 1]  = radInt;
      IDirRad[DawnTime + i + 1]  = IDir;
      IDiffRad[DawnTime + i + 1] = IDiff;
      }
   //Add Last Partial
   canopyPhoto(1);
   if (biomassCan == biomassCan && radInt == radInt && IDir == IDir && IDiff == IDiff) // Test For NaN
      {
      IBiomass[DawnTime + DayLengthInt + 1] = biomassCan * DuskDawnFract;
      IRadInt[DawnTime + DayLengthInt + 1]  = radInt     * DuskDawnFract;
      IDirRad[DawnTime + DayLengthInt + 1]  = IDir       * DuskDawnFract;
      IDiffRad[DawnTime + DayLengthInt + 1] = IDiff      * DuskDawnFract;
      }

   biomassDay = 0;
   radIntDay = 0;
   radDirDay = 0;
   radDiffDay = 0;
   biomassDay = sumVector(IBiomass) * timeStep;
   radIntDay  = sumVector(IRadInt)  * timeStep;
   radDirDay  = sumVector(IDirRad)  * timeStep;
   radDiffDay = sumVector(IDiffRad) * timeStep;
   rueDay = biomassDay / radIntDay;

   //Get Hourly Totals
   HBiomass.clear();
   HRadInt.clear();
   HDirRad.clear();
   HDiffRad.clear();
   for(int j = 0; j < 24 ; j++)
      {
      HBiomass.push_back(0.0);
      HRadInt.push_back(0.0);
      HDirRad.push_back(0.0);
      HDiffRad.push_back(0.0);
      }

   int partsPerHour = fullDayLengthInt / 24;
   int thehour = 0;
   for(int i = 0; i < fullDayLengthInt; i++)
      {
      thehour = i / partsPerHour;
      HBiomass[thehour] += IBiomass[i];
      HRadInt[thehour]  += IRadInt[i];
      HDirRad[thehour]  += IDirRad[i];
      HDiffRad[thehour] += IDiffRad[i];
      }
   for(int j = 0; j < 24 ; j++)
      {
      HBiomass[j] *= timeStep;
      HRadInt[j]  *= timeStep;
      HDirRad[j]  *= timeStep;
      HDiffRad[j] *= timeStep;
      }
   }
//---------------------------------------------------------------------------
//canopy assimilation g/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getInstantCanopyBiomass(int timeStep)
   {
   return IBiomass[timeStep];
   }
//---------------------------------------------------------------------------
//canopy radiation interception MJ/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getInstantCanopyRadInt(int timeStep)
   {
   return IRadInt[timeStep];
   }
//---------------------------------------------------------------------------
//direct incoming radiation MJ/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getInstantDirectRad(int timeStep)
   {
   return IDirRad[timeStep];
   }
//---------------------------------------------------------------------------
//diffuse incoming radiation MJ/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getInstantDiffuseRad(int timeStep)
   {
   return IDiffRad[timeStep];
   }
//---------------------------------------------------------------------------
//canopy assimilation g/m2
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getHourlyCanopyBiomass(int timeStep)
   {
   return HBiomass[timeStep];
   }
//---------------------------------------------------------------------------
//canopy radiation interception MJ/m2
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getHourlyCanopyRadInt(int timeStep)
   {
   return HRadInt[timeStep];
   }
//---------------------------------------------------------------------------
//direct incoming radiation MJ/m2
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getHourlyDirectRad(int timeStep)
   {
   return HDirRad[timeStep];
   }
//---------------------------------------------------------------------------
//diffuse incoming radiation MJ/m2
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getHourlyDiffuseRad(int timeStep)
   {
   return HDiffRad[timeStep];
   }
//---------------------------------------------------------------------------
//canopy assimilation g/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getCanopyBiomass(void)
   {
   return biomassCan;
   }
//---------------------------------------------------------------------------
//canopy radiation interception MJ/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getCanopyRadInt(void)
   {
   return radInt;
   }
//---------------------------------------------------------------------------
//direct incoming radiation MJ/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getDirectRad(void)
   {
   return IDir;
   }
//---------------------------------------------------------------------------
//diffuse incoming radiation MJ/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getDiffuseRad(void)
   {
   return IDiff;
   }
//---------------------------------------------------------------------------
//total incoming solar radiation MJ/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getTotalRad(void)
   {
   return ITot;
   }
//---------------------------------------------------------------------------
//solar noon total incoming solar radiation MJ/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getMaxRad(void)
   {
   return IMax;
   }
//---------------------------------------------------------------------------
//sun angle (solar elecation above horizon)
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getSunAngle(void)
   {
   return sunAngle;
   }
//---------------------------------------------------------------------------
//solar declination (Radians)
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getSolarDecl(void)
   {
   return solarDec;
   }
//---------------------------------------------------------------------------
//daylength (radians)
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getDayLength(void)
   {
   return dayL;
   }
//---------------------------------------------------------------------------
// daylength (hours)
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getDayLengthInHour(void)
   {
   return dayLH;
   }
//---------------------------------------------------------------------------
//extra-terrestrial irradiance MJ/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getSolarRad(void)
   {
   return solarRad;
   }
//---------------------------------------------------------------------------
//solar radiation at ground calculated as the product of SOLAR and RATIO MJ/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getGroundSolarRad(void)
   {
   return solarRadG;
   }
//---------------------------------------------------------------------------
// Calculated in CanopyP (time dependent)
//radiation use efficiency g/MJ; instantaneous
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerRUE(int Layer)
   {
   return rue[Layer];
   }
//---------------------------------------------------------------------------
//sunlit LAI
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerLAISUN(int Layer)
   {
   return laiSunLit[Layer];
   }
//---------------------------------------------------------------------------
//shaded LAI
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerLAISH(int Layer)
   {
   return laiSH[Layer];
   }
//---------------------------------------------------------------------------
//light intensity on sunlit leaves MJ/m2/s /m2 leaf area
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerISunLit(int Layer)
   {
   return ISunLit[Layer];
   }
//---------------------------------------------------------------------------
//light intensity oo shaded leaves MJ/m2/s/m2 leaf area
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerISH(int Layer)
   {
   return ISH[Layer];
   }
//---------------------------------------------------------------------------
//asymptote of photosynthesis-light response curve mg/m2 leaf area/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerPPMax(int Layer)
   {
   return PMax[Layer];
   }
//---------------------------------------------------------------------------
//cumulative LAI from top of canopy m2/m2
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerSumLAI(int Layer)
   {
   return sumLAI[Layer];
   }
//---------------------------------------------------------------------------
//proportion light intercepted to that point in the canopy
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerSumF(int Layer)
   {
   return sumF[Layer];
   }
//---------------------------------------------------------------------------
//proportion light intercepted in canopy layer
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerF(int Layer)
   {
   return F[Layer];
   }
//---------------------------------------------------------------------------
//cumulative sunlit LAI from top of canopy m2/m2
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerLIASunLit(int Layer)
   {
   return laiSunLit[Layer];
   }
//---------------------------------------------------------------------------
//photosynthesis by sunlit leaves mg/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerCSun(int Layer)
   {
   return cSunLit[Layer];
   }
//---------------------------------------------------------------------------
//photosynthesis by shaded leaves mg/m2/s
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerCSH(int Layer)
   {
   return cSH[Layer];
   }
//---------------------------------------------------------------------------
//canopy assimilation g/m2/sec
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerBiomass(int Layer)
   {
   return biomass[Layer];
   }
//---------------------------------------------------------------------------
//extinction coefficient
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerK(int Layer)
   {
   return K[Layer];
   }
//---------------------------------------------------------------------------
//shade projection coefficient
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getLayerG(int Layer)
   {
   return G[Layer];
   }
//---------------------------------------------------------------------------
//canopy assimilation g/m2/day
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getDailyCanopyBiomass(void)
   {
   return biomassDay;
   }
//---------------------------------------------------------------------------
//canopy radiation interception MJ/m2/day
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getDailyCanopyRadInt(void)
   {
   return radIntDay;
   }
//---------------------------------------------------------------------------
//RUE for whole canopy for the day g/MJ
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getDailyCanopyRUE(void)
   {
   return rueDay;
   }
//---------------------------------------------------------------------------
//direct beam solar radiation MJ/m2/day
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getDailyDirectRad(void)
   {
   return radDirDay;
   }
//---------------------------------------------------------------------------
//diffuse solar radiation MJ/m2/day
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getDailyDiffuseRad(void)
   {
   return radDiffDay;
   }
//---------------------------------------------------------------------------
//
//---------------------------------------------------------------------------
double CanopyPhotosynthesis::getDailyExtictCoef(void)
   {
   double radInt =  getDailyCanopyRadInt();
   double lai = laiLayers->GetVal(1) * nLayers;

   double frint = radInt / solarRadG;

   return  -1 *(log(1 - frint) / lai);
   }






