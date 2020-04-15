//------------------------------------------------------------------------------------------------
#include <stdio.h>
#include "Plant.h"
#include "LeafSlowWilting.h"
#include "Utilities.h"
#include "CanopyPhotosynthesis.h"

using namespace Maize;
//------------------------------------------------------------------------------------------------
//------ LeafSlowWilting Constructor
//------------------------------------------------------------------------------------------------
LeafSlowWilting::LeafSlowWilting(ScienceAPI2 &api, Plant *p) : Leaf(api, p)
   {
	   initialize();
	   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Destructor
//------------------------------------------------------------------------------------------------
LeafSlowWilting::~LeafSlowWilting()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void LeafSlowWilting::doRegistrations(void)
   {
   scienceAPI.expose("hBio",					"" ,"", true, hBio);
   scienceAPI.expose("transDemand",				"" ,"", false, transDemand);
   scienceAPI.expose("transDemandFluxLimited",  "" ,"", false, transDemandFluxLimited);
   }
//--------------------------------------------------------------------------------------------------
// Initial variables
//--------------------------------------------------------------------------------------------------
void LeafSlowWilting::initialize(void)
   {
   Leaf::initialize();
   //for (int i = 0; i < 24; i++)
   //   hBio.push_back(0.0);
   }
//------------------------------------------------------------------------------------------------
//-----------  read leaf slow wilting parameters
//------------------------------------------------------------------------------------------------
void LeafSlowWilting::readParams (void)
   {
   Leaf::readParams();
   // slow wilting stuff
   maxFlux = 0;
   scienceAPI.read( "maxFlux",  "", false, maxFlux, 0, 200);

   readTParams();
   }
//------------------------------------------------------------------------------------------------
//-----------  read leaf slow wilting parameters
//------------------------------------------------------------------------------------------------
void LeafSlowWilting::readTParams (void)
   {
   scienceAPI.get( "latitude",  "", 0, latitude, -90.0f, 90.0f);
   scienceAPI.get( "maxLag",     "", false, maxLag, -10.0f, 10.0f);
   scienceAPI.get( "nightCoef",  "", false, nightCoef, -10.0f, 10.0f);
   scienceAPI.get( "minLag",     "", false, minLag, -10.0f, 10.0f);
   TAirParam.push_back(maxLag);
   TAirParam.push_back(nightCoef);
   TAirParam.push_back(minLag);
   }
//------------------------------------------------------------------------------------------------
//Slow Wilting
void LeafSlowWilting::calcSWSupplyDemand(double &actDemand, double &maxSupply)
   {
   calcHourlyVars();

   double RUE = plant->getRUECf();
   double TEc = plant->getTranspEffCf();

   bool didCanopy = canPhoto->didCalcCanopyPhoto();
   hBio = canPhoto->getHourlyCanopyBiomass();
   
   CalcTDemandSupply(hRadn, coverGreen, RUE, TEc, VPDair, maxFlux, hBio, didCanopy, transDemand, transDemandFluxLimited);
   
   //Hydraulic Conductance Limit Mech for Roots 
   if (hydCondLimitMechanism == "Roots")
      {
      actDemand = sumVector(transDemand);
      maxSupply = sumVector(transDemandFluxLimited);
      if (actDemand == maxSupply)
         {
         maxSupply = -1.0;
         }
      }
   //Hydraulic Conductance Limit Mech for Leaf 
   else
      {
      actDemand = sumVector(transDemandFluxLimited);
	  double fullDemand = sumVector(transDemand);
	  if (fullDemand == actDemand)
	     {
		 maxSupply = -1.0;
	     }
	  else
	     {
         maxSupply = actDemand;
         }
      }
   }
//------------------------------------------------------------------------------------------------
//Slow Wilting
void LeafSlowWilting::calcHourlyVars()
   {
   hRadn.clear();
   TAir.clear();
   SVP.clear();
   RH.clear();
   VPDair.clear();
   transDemandFluxLimited.clear();
   transDemand.clear();
   hBio.clear();
   for (int i = 0; i < 24; i++)
      hBio.push_back(0.0);


	calcHourlyWeatherVars(latitude,plant->today.doy,plant->today.radn,plant->today.maxT,plant->today.minT, TAirParam,
						  hRadn, TAir, SVP, RH, VPDair);
   }
//------------------------------------------------------------------------------------------------