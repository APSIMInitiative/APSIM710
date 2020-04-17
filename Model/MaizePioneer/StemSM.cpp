//------------------------------------------------------------------------------------------------
#include "Plant.h"
#include "StemSM.h"
using namespace Maize;
//------------------------------------------------------------------------------------------------
//------ Stem Constructor
//------------------------------------------------------------------------------------------------
StemSM::StemSM(ScienceAPI2 &api, Plant *p) : Stem(api, p) 
   {   
   initialize();
   doRegistrations();
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void StemSM::doRegistrations(void)
   {
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void StemSM::initialize(void)
   {
   Stem::initialize();
   dmLabile = 0;
   dltDmLabile = 0;
   }
//------------------------------------------------------------------------------------------------
//------ read Stem parameters
//------------------------------------------------------------------------------------------------
void StemSM::readParams (void)
   {
Stem::readParams();
   scienceAPI.read("stemRSCfrac","",0,stemRSCfrac);
   }
//------------------------------------------------------------------------------------------------
//------ update Stem variables
//------------------------------------------------------------------------------------------------
void StemSM::updateVars(void)
   {
   Stem::updateVars();
   
   // Pioneer Cohort
   dmLabile += dltDmLabile;

   dltDmLabile = 0.0;
   dmLabile += dmRetranslocate;
   }
//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void StemSM::phenologyEvent(int iStage)
   {
   Stem::phenologyEvent(iStage);
   switch (iStage)
      {
   
      case flowering:
         // Pioneer Cohort
         //double dmgreenplant = dmGreen / density;
         //double labilepercent = max(0.0, min(2.0 * (dmgreenplant - 40) / 100, .35));
         //dmLabile = labilepercent * dmGreen;
         // old way
         dmLabile = stemRSCfrac * dmGreen;                  // Eqn[11i]

         break;
      }
   }
//------------------------------------------------------------------------------------------------
      // Pioneer Cohort
//------------------------------------------------------------------------------------------------
double StemSM::dmRetransAvailable(void)
   {
   // calculate carbon available for translocation to grain
   // this is the fraction of the labile pool that can be moved in 1 day rlcRate
   stemWtAvail = retransRate * dmLabile;
   return stemWtAvail;
   }

//------------------------------------------------------------------------------------------------
      // Pioneer Cohort
//------------------------------------------------------------------------------------------------
double StemSM::partitionLabileDM(double carbo)
   {
   // after anthesis, excess carbon produced and not used in the grain,
   // goes into the labile store
   // also there is a minimum that must return to the store (minPGR)
   dltDmLabile += carbo;
   dltDmGreen += carbo;
   return 1;
   }
//------------------------------------------------------------------------------------------------

