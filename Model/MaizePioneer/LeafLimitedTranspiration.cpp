//------------------------------------------------------------------------------------------------
#include <stdio.h>
#include "Plant.h"
#include "LeafLimitedTranspiration.h"
#include "Utilities.h"
#include "CanopyPhotosynthesis.h"

using namespace Maize;
//------------------------------------------------------------------------------------------------
//------ LeafLimitedTranspiration Constructor
//------------------------------------------------------------------------------------------------
LeafLimitedTranspiration::LeafLimitedTranspiration(ScienceAPI2 &api, Plant *p) : LeafSlowWilting(api, p)
   {
	   initialize();
	   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Destructor
//------------------------------------------------------------------------------------------------
LeafLimitedTranspiration::~LeafLimitedTranspiration()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void LeafLimitedTranspiration::doRegistrations(void)
   {
   scienceAPI.expose("adjustedHBio","","",false,adjustedHBio);
   scienceAPI.expose("VPDair","","",false,VPDair);
   }
//--------------------------------------------------------------------------------------------------
// Initial variables
//--------------------------------------------------------------------------------------------------
void LeafLimitedTranspiration::initialize(void)
   {
   LeafSlowWilting::initialize();
   adjustedHBio.assign(24,0.0);
   }
//------------------------------------------------------------------------------------------------
//-----------  read leaf limited transpiration parameters
//------------------------------------------------------------------------------------------------
void LeafLimitedTranspiration::readParams (void)
   {
   Leaf::readParams();
   
   scienceAPI.read("maxTrans", "", false, maxTrans);
   scienceAPI.read("maxTransEffect", "", false, maxTransEffect);
  
   readTParams();

   // Force it to use the Canopy Photosynthesis model
   useLeafAngles = true;
   }
//------------------------------------------------------------------------------------------------
//Limited Transpiration
void LeafLimitedTranspiration::calcSWSupplyDemand(double &actDemand, double &maxSupply)
   {
   calcHourlyVars();

   double RUE = plant->getRUECf();
   double TEc = plant->getTranspEffCf();

  
   hBio = canPhoto->getHourlyCanopyBiomass();
   
   CalcLimitTDemandSupply(hBio, coverGreen, TEc, VPDair, maxTrans, maxTransEffect, transDemand, transDemandFluxLimited);
   
   //Hydraulic Conductance Limit Mech for Roots 
    if (hydCondLimitMechanism == "Roots")
      {
      actDemand = sumVector(transDemand);
      maxSupply = sumVector(transDemandFluxLimited);
      if (abs(actDemand - maxSupply) < .001) // Better Than Testing Equivalence
         {
         maxSupply = -1.0;
         }
      }
   //Hydraulic Conductance Limit Mech for Leaf 
   else
      {
      actDemand = sumVector(transDemandFluxLimited);
      maxSupply = -1.0;
      }
   for (int i = 0; i < 24; i++)
	     {
		  adjustedHBio[i] = transDemandFluxLimited[i] * TEc / VPDair[i] / 10e-4; 
	     }
   }
//------------------------------------------------------------------------------------------------
void LeafLimitedTranspiration::balanceWater(double &swDemand,double &totalSupply)
   {
   double RUE = plant->getRUECf();
   double TEc = plant->getTranspEffCf();
   bool didCanopy = canPhoto->didCalcCanopyPhoto();

   double maxTransTarget = maxTrans * (totalSupply/swDemand);
   if (plant->das == 0 || plant ->das == 1)
      return;

   double effect = 1.0;
   CalcLimitTDemandSupply(hBio, coverGreen, TEc, VPDair, maxTransTarget, effect, transDemand, transDemandFluxLimited);
   double newDemand = sumVector(transDemandFluxLimited);
   double diff = totalSupply - newDemand;
   int direction = 0;
   double adjust = .1;
   int count = 0;
   while (abs(diff) > 0.01)
      {
      if (diff > 0)
         {
         if (direction == -1)
            adjust *= .1;
         maxTransTarget += adjust;
         direction = 1;
         }
      else
         {
         if (direction == 1)
            adjust *= .1;
         maxTransTarget -= adjust;
         direction = -1;
         }
      CalcLimitTDemandSupply(hBio, coverGreen, TEc, VPDair, maxTransTarget, effect, transDemand, transDemandFluxLimited);
      newDemand = sumVector(transDemandFluxLimited);
      diff = totalSupply - newDemand;
      count++;
      }
   for (int i = 0; i < 24; i++)
      {
		adjustedHBio[i] = transDemandFluxLimited[i] * TEc / VPDair[i] / 10e-4; 
      }
   }
//------------------------------------------------------------------------------------------------
double LeafLimitedTranspiration::calcDltDMPotTE(void)
   {
   return sumVector(adjustedHBio);
   }
//------------------------------------------------------------------------------------------------