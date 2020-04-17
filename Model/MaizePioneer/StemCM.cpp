//------------------------------------------------------------------------------------------------
#include "Plant.h"
#include "StemCM.h"
using namespace Maize;
//------------------------------------------------------------------------------------------------
//------ Stem Constructor
//------------------------------------------------------------------------------------------------
StemCM::StemCM(ScienceAPI2 &api, Plant *p) : Stem(api, p) 
   {   
   initialize();
   doRegistrations();
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void StemCM::doRegistrations(void)
   {
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void StemCM::initialize(void)
   {
   Stem::initialize();
   dmLabile = 0;
   dltDmLabile = 0;
   }
//------------------------------------------------------------------------------------------------
//------ read Stem parameters
//------------------------------------------------------------------------------------------------
void StemCM::readParams (void)
   {
Stem::readParams();
   scienceAPI.read("stemRSCfrac","",0,stemRSCfrac);
   }
//------------------------------------------------------------------------------------------------
//------ update Stem variables
//------------------------------------------------------------------------------------------------
void StemCM::updateVars(void)
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
void StemCM::phenologyEvent(int iStage)
   {
   Stem::phenologyEvent(iStage);
   switch (iStage)
      {
   
   case flowering :
      // Pioneer Cohort
      dmLabile = stemRSCfrac * dmGreen;                  // Eqn[11i]
         
      break;
      }
   }
//------------------------------------------------------------------------------------------------
      // Pioneer Cohort
//------------------------------------------------------------------------------------------------
double StemCM::dmRetransAvailable(void)
   {
   // calculate carbon available for translocation to grain
   // this is the fraction of the labile pool that can be moved in 1 day rlcRate
   stemWtAvail = retransRate * dmLabile;
   return stemWtAvail;
   }

//------------------------------------------------------------------------------------------------
      // Pioneer Cohort
//------------------------------------------------------------------------------------------------
double StemCM::partitionLabileDM(double carbo)
   {
   // after anthesis, excess carbon produced and not used in the grain,
   // goes into the labile store
   // also there is a minimum that must return to the store (minPGR)
   dltDmLabile += carbo;
   dltDmGreen += carbo;
   return 1;
   }
//------------------------------------------------------------------------------------------------

