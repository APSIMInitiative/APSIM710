#include <stdio.h>
#include <ComponentInterface2/Variant.h>

#include "Plant.h"
#include "BiomassSM.h"
#include "StemSM.h"

using namespace Maize;
//---------------------------------------------------------------------------
//------ Biomass Constructor
//------------------------------------------------------------------------------------------------
BiomassSM::BiomassSM(ScienceAPI2 &api, Plant *p) : Biomass(api, p)
   {
   initialize();
   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Biomass Destructor
//------------------------------------------------------------------------------------------------
BiomassSM::~BiomassSM()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void BiomassSM::doRegistrations(void)
   {
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void BiomassSM::initialize(void)
   {
   Biomass::initialize();
   }
//------------------------------------------------------------------------------------------------
void BiomassSM::process(void)
   {
   calcBiomassTE();
   calcDltBiomass();
   // biomass partitioning
   calcPartitioning();
   // biomass retranslocation
   if(stage >= fi && stage <= endGrainFill)
      calcRetranslocation();

   }
//------------------------------------------------------------------------------------------------
//------ read Biomass parameters
//------------------------------------------------------------------------------------------------
void BiomassSM::readParams (void)
   {
   Biomass::readParams();
   minPGR = 0;
   scienceAPI.read("minPGR","",1,minPGR);
   }
//------------------------------------------------------------------------------------------------
//-------------------  Partitioning
//------------------------------------------------------------------------------------------------
void BiomassSM::calcPartitioning(void)
   {
   // Roots
   // Root must be satisfied. The roots don't take any of the carbohydrate produced
   //  - that is for tops only.  Here we assume that enough extra was produced to meet demand.
   // Thus the root growth is not removed from the carbo produced by the model.
   double biomPool = dltDM;

   int currentPhase = (int) stage;
   plant->roots->partitionDM(ratioRootShoot[currentPhase] * biomPool);

   //  leaf and stem to fi then rachis as well to flag
   if(stage >= emergence && stage < flag)
      {
      // leaf first
      biomPool -= plant->leaf->partitionDM(biomPool);

      //if stage > fi give some to rachis and cob
      if(stage >= fi)
         {
         biomPool -= plant->rachis->partitionDM(biomPool * stem2FlowerFrac);
         biomPool -= plant->grain->partitionDM(biomPool);		// cob demand
         }

      // rest to stem
      plant->stem->partitionDM(biomPool);
      }
   else if(stage >= flag && stage < flowering)
      {
      // we only have rachis and stem growth here
      biomPool -= plant->rachis->partitionDM(biomPool * stem2FlowerFrac);
      biomPool -= plant->grain->partitionDM(biomPool);		// cob demand
      plant->stem->partitionDM(biomPool);
      }
   else if(stage >= flowering && stage < maturity)
      {
      //grain filling starts - stem continues when it can
      biomPool -= plant->grain->partitionDM(biomPool);
      ((StemSM*)plant->stem)->partitionLabileDM(biomPool);
      //plant->stem->partitionDM(biomPool);
      }
   else
      {
      plant->stem->partitionDM(biomPool);
      }
   }
//------------------------------------------------------------------------------------------------
void BiomassSM::calcRetranslocation(void)
   {
   calcBiomassRetranslocation();
   }
//------------------------------------------------------------------------------------------------
// Calculate plant dry matter delta's due to retranslocation to grain (g/m^2)
void BiomassSM::calcBiomassRetranslocation(void)
   {
  
   // get available supply from labile pool
   double labileSupply = plant->stem->dmRetransAvailable();
   double retransToGrain = plant->grain->dmRetrans(labileSupply);
   ((StemSM*)plant->stem)->partitionLabileDM(retransToGrain);
   }

