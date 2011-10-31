#include "StdPlant.h"

#include "GrainPart.h"
#include "../Co2Modifier.h"
#include "FruitCohortFN.h"
#include "PodPartFN.h"
#include "../Population.h"
#include "../Environment.h"
using namespace std;

fruitPodPartFN::fruitPodPartFN(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : fruitPodPart(scienceAPI, p, name)
   {
   }

void fruitPodPartFN::onInit1(protocol::Component *system)
//=======================================================================================
{
   fruitPodPart::onInit1(system);


}

void fruitPodPartFN::prepare(void)
//=======================================================================================
   {
   fruitPodPart::prepare();

   }

void fruitPodPartFN::update(void)
//=======================================================================================
{
   fruitPodPart::update();
}

void fruitPodPartFN::onFlowering(void)
//=======================================================================================
{
   fruitPodPart::onFlowering();
}

// set the minimum weight of part; used for retranslocation to grain
void fruitPodPartFN::onStartGrainFill(void)
//=======================================================================================
{
   fruitPodPart::onStartGrainFill();
}



void fruitPodPartFN::doDmDemand(float dlt_dm_supply)
//=======================================================================================
{
   float dlt_dm_supply_by_pod = 0.0;  // FIXME
   dlt_dm_supply += dlt_dm_supply_by_pod;

   float dm_grain_demand = plant->Grain().dltDmPotentialGrain();

   if (dm_grain_demand <= 0.0)
   {
      // we are in flowering phase
      DMGreenDemand = dlt_dm_supply * plant->phenology().doInterpolation(*fracPod)  - dlt_dm_supply_by_pod;
   }
   else
   {
      DMGreenDemand = dm_grain_demand * plant->phenology().doInterpolation(*fracPod)  - dlt_dm_supply_by_pod;
   }
}

void fruitPodPartFN::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   Retranslocation.AddNonStructuralDM(DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0));
   }

float fruitPodPartFN::dltDmRetranslocateSupply(float DemandDifferential)
//=======================================================================================
   {
   float DMPartPot = Green.DM() + Retranslocation.DM();
   float DMPartAvail = DMPartPot - Green.StructuralDM();
   DMPartAvail = l_bound (DMPartAvail, 0.0);
   float DltDmRetransPart = min (DemandDifferential, DMPartAvail);
   Retranslocation.AddNonStructuralDM(-DltDmRetransPart);   //XXXX this is a bad thing..
   return DltDmRetransPart;
   }

void fruitPodPartFN::zeroAllGlobals(void)
//=======================================================================================
{
   fruitPodPart::zeroAllGlobals();
}

void fruitPodPartFN::zeroDeltas(void)
//=======================================================================================
{
   fruitPodPart::zeroDeltas();
}


void fruitPodPartFN::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
{
   fruitPodPart::readSpeciesParameters(system, sections);

}

void fruitPodPartFN::doProcessBioDemand(void)
   //===========================================================================
{
}

void fruitPodPartFN::doTECO2()          // (OUTPUT) transpiration coefficient
   //==========================================================================
{
   cproc_transp_eff_co2_1(plant->environment().vpdEstimate()
                          , plant->phenology().doInterpolation(TECoeff)
                          , co2Modifier->te()
                          , &transpEff);
}

