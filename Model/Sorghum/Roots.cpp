//------------------------------------------------------------------------------------------------

//#include <ComponentInterface2/Variant.h>
#include <ComponentInterface2/DataTypes.h>
#include "Plant.h"
#include "Roots.h"
using namespace Sorghum;
//------------------------------------------------------------------------------------------------
//------ Roots Constructor
//------------------------------------------------------------------------------------------------
Roots::Roots(ScienceAPI2& api, Plant *p) : PlantPart(api)
   {
   plant = p;
   name = "Roots";
   partNo = 0;

   initialize();
   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Roots Destructor
//------------------------------------------------------------------------------------------------
Roots::~Roots()
   {
   }
//------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Roots::doRegistrations(void)
   {
   scienceAPI.expose("RootLength"    ,"mm/mm2","Root length",                 false, rootLength);
   scienceAPI.expose("RLV"           ,"mm/mm3","Root length volume in layers",false, rlvFactor);
   scienceAPI.expose("RootDepth"     ,"mm"    ,"Depth of roots",              false, rootDepth);
   scienceAPI.expose("root_depth"    ,"mm"    ,"Depth of roots",              false, rootDepth);
   scienceAPI.expose("RootFront"     ,"mm"    ,"Depth of root front",         false, rootFront);
   scienceAPI.expose("RootGreenWt"   ,"g/m^2" ,"Green root dry weight",       false, dmGreen);
   scienceAPI.expose("RootSenescedWt","g/m^2" ,"Senesced root dry weight",    false, dmSenesced);
   scienceAPI.expose("RootGreenN"    ,"g/m^2" ,"N in live root",              false, nGreen);
   scienceAPI.expose("RootGreenNConc","%"     ,"Live root N concentration",   false, nConc);
   scienceAPI.expose("RootSenescedN" ,"g/m^2" ,"Senesced root dry weight",    false, nSenesced);
   scienceAPI.expose("RootNDemand"   ,"g/m^2" ,"Today's N demand from roots", false, nDemand);
   scienceAPI.exposeFunction("RootProportion", "0-1", "Root proportion in layers",
                    FloatArrayFunction(&Roots::getRP));
   scienceAPI.expose("RootGreenP"    ,"g/m^2" ,"P in live root",              false, pGreen);
   scienceAPI.expose("RootSpread"   ,"mm" ,"Lateral root distance", false, rootSpread);
	
   }
//------------------------------------------------------------------------------------------------
//------- React to a newProfile message
//------------------------------------------------------------------------------------------------
void Roots::onNewProfile(NewProfileType &v /* message */)
   {
   FVecToDVec(&dLayer, v.dlayer);

   // dlayer may be changed from its last setting due to erosion
   profileDepth = sumVector(dLayer);      // depth of soil profile (mm)
   nLayers = dLayer.size();
   /* TODO : Insert new root profile and llDep code for change in profile due to erosion */

   rootLength.assign (nLayers,0.0);
   rlvFactor.assign  (nLayers,0.0);
   dltRootLength.assign           (nLayers,0.0);
   dltScenescedRootLength.assign  (nLayers,0.0);
   rootProportion.assign  (nLayers,0.0);

   /* TODO : Check validity of ll,dul etc as in crop_check_sw */
   /* TODO : why does this not include no3 */
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Roots::initialize(void)
   {
   rootDepth = 0.0;
	   rootFront    = 0.0;
   currentLayer = 0;
   leftDist = 0.0;
   rightDist = 0.0;
   rootSpread = 0.0;
   lastLayerPropn = 0.0;
   lastLayerPropn = 0.0;
	   dltRootDepth = 0.0;
   dltRootFront = 0.0;
   lastLayerPropn = 0.0;


   if(nLayers > 0 && nLayers < 100)
   {
   rootLength.assign(nLayers, 0.0);
   rlvFactor.assign(nLayers,0.0);
   rootProportion.assign(nLayers,0.0);
   }
   PlantPart::initialize();
   }
//------------------------------------------------------------------------------------------------
//------- Read root parameters
//------------------------------------------------------------------------------------------------
void Roots::readParams (void)
   {
   scienceAPI.read("xf", "", 0, xf);

   initialRootDepth = plant->getSowingDepth();
   scienceAPI.read("dm_root_init",         "", 0, initialDM);
   scienceAPI.read("specific_root_length", "", 0, specificRootLength);
   scienceAPI.read("dm_root_sen_frac",     "", 0, dmRootSenFrac);
   scienceAPI.read("root_depth_rate",      "", 0, rootDepthRate);
   rootDepthRate.insert(rootDepthRate.begin(),0);  // for compatibility with fortran

   swRoot.read(scienceAPI, "x_sw_ratio", "y_sw_fac_root");
   rldFn.read(scienceAPI,  "x_plant_rld","y_rel_root_rate");

   // nitrogen
   scienceAPI.read("initialRootNConc", "", 0, initialNConc);
   scienceAPI.read("targetRootNConc",  "", 0, targetNConc);

   // phosphorus
   pMaxTable.read(scienceAPI, "x_p_stage_code","y_p_conc_max_root");
   pMinTable.read(scienceAPI, "x_p_stage_code","y_p_conc_min_root");
   pSenTable.read(scienceAPI, "x_p_stage_code","y_p_conc_sen_root");
   scienceAPI.read("p_conc_init_root", "", 0, initialPConc);

   //Root Angle
   try
	   {
      scienceAPI.get("UseRootAngle", "", 0, useRootAngle);
      scienceAPI.get("RootAngle", "", 0, rootAngle);
	   }
   catch(...)
	   {
      useRootAngle = 0;
      rootAngle = 0;
	   }

   //Set root angle to use a semicircular pattern if root angle is turned off
   if(!useRootAngle)
      {
      rootAngle = 45;
      }

   // dh - this happens on sowing in new apsim
   calcInitialLength();
   leftDist = plant->getRowSpacing() * (plant->getSkipRow() - 0.5);
   rightDist = plant->getRowSpacing() * 0.5;

   }
//------------------------------------------------------------------------------------------------
void Roots::process(void)
   {
   // root length
   if(stage > germination)
      {
      calcRootDistribution();
      calcSenLength();
      }
   }
//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void Roots::phenologyEvent(int stage)
   {
   switch (stage)
      {
   case germination :
      //calcInitialLength();
      //leftDist  = plant->getRowSpacing() * (plant->getSkipRow() - 0.5);
      //rightDist = plant->getRowSpacing() * 0.5;

         break;
      case emergence :
         dmGreen = initialDM * plant->getPlantDensity();
         nGreen = initialNConc * dmGreen;
         pGreen = initialPConc * dmGreen;
         ExternalMassFlowType EMF;
         EMF.PoolClass = "crop";
         EMF.FlowType = "gain";
         EMF.DM = 0.0;
         EMF.N  = nGreen * gm2kg/sm2ha;
         EMF.P  = pGreen * gm2kg/sm2ha;
         EMF.C = 0.0; // ?????
         EMF.SW = 0.0;
         scienceAPI.publish("ExternalMassFlow", EMF);
         break;
      }
   }
//------------------------------------------------------------------------------------------------
//------- at the end of the day, update state variables   -  called from plant->process
//------------------------------------------------------------------------------------------------
void Roots::updateVars(void)
   {
   stage = plant->phenology->currentStage();
   // update root variables by daily deltas
   dltRootDepth = calcDltRootDepth(plant->phenology->currentStage());
   rootDepth += dltRootDepth;
   dltRootFront = calcDltRootFront(plant->phenology->currentStage());
   rootFront += dltRootFront;

   calcRootProportions();
   // calculate current root layer
   currentLayer = findIndex(rootDepth, dLayer);
   // calculate proportion of this layer occupied
   lastLayerPropn = layerProportion();

   dmGreen += dltDmGreen - dltDmSenesced;
   dmSenesced += dltDmSenesced;
   nGreen  += dltNGreen  - dltNSenesced;
   nSenesced += dltNSenesced;
   nConc = divide(nGreen,dmGreen,0) * 100;
   }
//------------------------------------------------------------------------------------------------
//-------
//------------------------------------------------------------------------------------------------
double Roots::calcNDemand(void)
   {
   // ROOT demand to keep root [N] at targetRootNConc
   double nRequired = (dmGreen + dltDmGreen) * targetNConc;   // g/m2
   nDemand = Max(nRequired - nGreen,0.0);
   return nDemand;
   }
//------------------------------------------------------------------------------------------------
double Roots::layerProportion(void)
   {
   // calculates the proportion of the current root layer that is populated by roots
   double layerTop    = sumVector(dLayer, currentLayer);
   double layerBottom = sumVector(dLayer, currentLayer+1);

   return divide(rootDepth - layerTop,layerBottom - layerTop);
   }
//------------------------------------------------------------------------------------------------
void Roots::calcInitialLength(void)
   {
	// initial root depth
	//jkb changes made to line up with pmf (should have been a bug)		
	rootDepth = initialRootDepth;
	rootFront = initialRootDepth;
	dltRootDepth = initialRootDepth;
	dltRootFront = initialRootDepth;

   }
//------------------------------------------------------------------------------------------------
void Roots::calcSenLength(void)
   {
   double senescedLength = dltDmSenesced / sm2smm * specificRootLength;
   double rootLengthSum = sumVector(rootLength);
   for(int i=0;i <= currentLayer;i++)
      {
      dltScenescedRootLength[i] = senescedLength *
                                 divide(rootLength[i],rootLengthSum);
      }
   }
//------------------------------------------------------------------------------------------------
//-------- Calculate the increase in rooting depth.
//------------------------------------------------------------------------------------------------
// Calculate the increase in root length density in each rooted layer based upon soil hospitality,
// moisture and fraction of layer explored by roots.
//------------------------------------------------------------------------------------------------
void Roots::calcRootDistribution(void)
   {
   double rlvFactorTotal = 0.0;
   for(int layer = 0; layer <= currentLayer; layer++)
      {
      double rld = divide (rootLength[layer],dLayer[layer]);
      double plantRld = divide (rld, plant->getPlantDensity());
      double branchingFactor = rldFn.value(plantRld);
      rlvFactor[layer] = (swAvailFactor(layer) * branchingFactor * xf[layer] *
                  divide(dLayer[layer],rootDepth * 10));
      rlvFactor[layer] = Max(rlvFactor[layer],1e-6);
      rlvFactorTotal += rlvFactor[layer];
      }
   double dltLengthTot = dltDmGreen / sm2smm * specificRootLength;
   for(int layer = 0; layer <= currentLayer; layer++)
      {
      dltRootLength[layer] = dltLengthTot * divide(rlvFactor[layer],rlvFactorTotal);
      /* ADTODO : is this right???*/
      rootLength[layer] += dltRootLength[layer];
      }
   }
//------------------------------------------------------------------------------------------------
double Roots::calcDltRootDepth(double stage)
   {
   // sw available factor of root layer
   double swFactor = swAvailFactor(currentLayer);
   if (swFactor < 1.0) {
	   int tm = 0;
   }

   //float adjRootDepthRate = calcAdjRootDepthRate(rootDepthRate[int (stage)]);
   //dltRootDepth  = adjRootDepthRate * swFactor * xf[currentLayer];
   dltRootDepth  = rootDepthRate[int (stage)] * swFactor * xf[currentLayer];
   // constrain it by the maximum depth that roots are allowed to grow
   dltRootDepth = Min(dltRootDepth,profileDepth - rootDepth);

   return dltRootDepth;
   }
//------------------------------------------------------------------------------------------------
double Roots::calcDltRootFront(double stage)
   {
   // calculate the root front
	double swFactor = swAvailFactor(currentLayer);
   dltRootFront  = rootDepthRate[int (stage)] * swFactor * xf[currentLayer];

   double maxFront = sqrt(pow(rootDepth,2) + pow(leftDist,2));
   dltRootFront = Min(dltRootFront, maxFront - rootFront);
   return dltRootFront;
   }
//------------------------------------------------------------------------------------------------
double Roots::swAvailFactor(int layer)
   {
   // get the soil water availability fraction of the current layer
   double swAvailRatio = plant->water->swAvailRatio(layer);
   // use this value in the swRoot table function
   return swRoot.value(swAvailRatio);
   }
//------------------------------------------------------------------------------------------------
void Roots::partitionDM(double dltDM)
   {
   dltDmGreen = dltDM;
   }
//------------------------------------------------------------------------------------------------
void Roots::calcSenescence(void)
   {
   dltDmSenesced = dmGreen * dmRootSenFrac;
   double senNConc = divide(nGreen,dmGreen);
   dltNSenesced  = dltDmSenesced * senNConc;
   }
//------------------------------------------------------------------------------------------------
//Get functions for registration
//------------------------------------------------------------------------------------------------
void Roots::getRootLength(vector<float> &result)
   {
   DVecToFVec(result, rootLength);
   }
//------------------------------------------------------------------------------------------------
void Roots::getRLV(vector<float> &result)
   {
   DVecToFVec(result, rlvFactor);
   }
//------------------------------------------------------------------------------------------------
void Roots::getRP(vector<float> &result)
   {
	result.clear();
   for (int layer = 0; layer < nLayers; layer++)
      if (layer <= currentLayer)
        result.push_back(RootProportionInLayer(layer));
      else
         result.push_back(0.0);
   }
//------------------------------------------------------------------------------------------------
double Roots::calcPDemand(void)
   {
   // ROOT P demand
   double rel_growth_rate = divide(plant->biomass->getDltDMPotRUE(),
      plant->biomass->getAboveGroundBiomass(),0.0);

   double deficit = pConcMax() * dmGreen * (1.0 + rel_growth_rate) - pGreen;

   pDemand = Max(deficit,0.0);
   return pDemand;
   }
//------------------------------------------------------------------------------------------------
void Roots::incorporateResidue(void)
   {
   //Root residue incorporation    called from plantActions doEndCrop

   if(!(totalBiomass() > 0.0))return;

   vector <double> dmIncorp;
   vector <double> nIncorp;
   vector <double> pIncorp;
   double rootLengthSum = sumVector(rootLength);

   double carbon = totalBiomass() * gm2kg /sm2ha;
   double n = totalN() * gm2kg /sm2ha;
   double p = totalP() * gm2kg /sm2ha;
   for (unsigned layer = 0; layer < dLayer.size(); layer++)
      {
      dmIncorp.push_back(carbon * divide(rootLength[layer],rootLengthSum,0.0));
      nIncorp.push_back(n * divide(rootLength[layer],rootLengthSum,0.0));
      pIncorp.push_back(p * divide(rootLength[layer],rootLengthSum,0.0));
      }

   FOMLayerType IncorpFOM;
   IncorpFOM.Type = plant->getCropType();
   for (unsigned i = 0; i != dmIncorp.size(); i++)
      {
      FOMLayerLayerType Layer;
      Layer.FOM.amount = dmIncorp[i];
      Layer.FOM.N = nIncorp[i];
      Layer.FOM.P = pIncorp[i];
      Layer.CNR = 0;
      Layer.LabileP = 0;
      IncorpFOM.Layer.push_back(Layer);
      }
   scienceAPI.publish("IncorpFOM", IncorpFOM);
   }
//------------------------------------------------------------------------------------------------
double Roots::RootProportionInLayer(int layer)
   {
   /* Row Spacing and configuration (skip) are used to calculate semicircular root front to give
   proportion of the layer occupied by the roots. */
   double top;
   if(layer == 0)top = 0;
   else top = sumVector(dLayer,layer);
   if(top > rootDepth)return 0.0;
   double bottom = top + dLayer[layer];

   double rootArea = getRootArea(top, bottom, rootFront, rightDist);    // Right side
   rootArea += getRootArea(top, bottom, rootFront, leftDist);          // Left Side
   double soilArea = (rightDist + leftDist) * (bottom - top);

   return Max(0.0, divide(rootArea, soilArea));
  }
//------------------------------------------------------------------------------------------------
double Roots::getRootArea(double top, double bottom, double rootLength, double hDist)
   {
      if(rootLength == 0.0)
      {
      return 0.0;
      }

   double depth, depthInLayer;

   rootSpread = rootLength * tan(DegToRad(rootAngle));   //Semi minor axis

   if(rootLength >= bottom)
      {
      depth = (bottom - top) / 2.0 + top;
      depthInLayer = bottom - top;
      }
   else
      {
      depth = (rootLength - top) / 2.0 + top;
      depthInLayer = rootLength - top;
      }
   double xDist = rootSpread * sqrt(1 - (pow(depth,2) / pow(rootLength,2)));

   return Min(depthInLayer * xDist, depthInLayer * hDist);
   }
//------------------------------------------------------------------------------------------------
double Roots::calcAdjRootDepthRate(double rootDepthRate)
   {
   //Calculate the circular equivalent length
   double circLength = rootDepth * sqrt(tan(DegToRad(rootAngle)));
   double maxLength = circLength + rootDepthRate;

   //Calculate the eliptical eqivalent
   double ellipLength = maxLength / sqrt(tan(DegToRad(rootAngle)));

   return ellipLength - rootDepth;
   }
//------------------------------------------------------------------------------------------------
void Roots::calcRootProportions()
   {
   for(int i = 0; i < nLayers; i++)
      {
      //Set global for reporting
      rootProportion[i] = RootProportionInLayer(i);
      }
   }
//------------------------------------------------------------------------------------------------


