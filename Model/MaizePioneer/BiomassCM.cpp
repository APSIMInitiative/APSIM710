#include <stdio.h>
#include <ComponentInterface2/Variant.h>

#include "Plant.h"
#include "BiomassCM.h"
#include "StemCM.h"

using namespace Maize;
//---------------------------------------------------------------------------
//------ Biomass Constructor
//------------------------------------------------------------------------------------------------
BiomassCM::BiomassCM(ScienceAPI2 &api, Plant *p) : Biomass(api, p)
   {
   initialize();
   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Biomass Destructor
//------------------------------------------------------------------------------------------------
BiomassCM::~BiomassCM()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void BiomassCM::doRegistrations(void)
   {
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void BiomassCM::initialize(void)
   {
   Biomass::initialize();
   }
//------------------------------------------------------------------------------------------------
void BiomassCM::process(void)
   {
	calcBiomassTE();
   calcDltBiomass();
	   // biomass partitioning
   calcPartitioning();
	   // biomass retranslocation
   if(stage >= emergence && stage <= endGrainFill)
      calcRetranslocation();

   }
//------------------------------------------------------------------------------------------------
//------ read Biomass parameters
//------------------------------------------------------------------------------------------------
void BiomassCM::readParams (void)
   {
   Biomass::readParams();
   minPGR = 0;
   scienceAPI.read("minPGR","",1,minPGR);
   }
//------------------------------------------------------------------------------------------------
//-------------------  Partitioning
//------------------------------------------------------------------------------------------------
void BiomassCM::calcPartitioning(void)
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

      //if stage > fi give some to rachis
      if(stage >= fi)
         biomPool -= plant->rachis->partitionDM(biomPool * stem2FlowerFrac);

      // rest to stem
      plant->stem->partitionDM(biomPool);
      }
   else if(stage >= flag && stage < flowering)
      {
      // we only have rachis and stem growth here
      biomPool -= plant->rachis->partitionDM(biomPool * stem2FlowerFrac);
      plant->stem->partitionDM(biomPool);
      }
   else if(stage >= flowering && stage < maturity)
      {
      //grain filling starts - stem continues when it can
      biomPool -= plant->grain->partitionDM(biomPool);
      ((StemCM*)plant->stem)->partitionLabileDM(biomPool);
      //plant->stem->partitionDM(biomPool);
      }
   else
      {
      plant->stem->partitionDM(biomPool);
      }
   }
//------------------------------------------------------------------------------------------------
void BiomassCM::calcRetranslocation(void)
   {
   calcBiomassRetranslocation();
   }
//------------------------------------------------------------------------------------------------
// Calculate plant dry matter delta's due to retranslocation to grain (g/m^2)
void BiomassCM::calcBiomassRetranslocation(void)
   {
   /* TODO :   this is a hack - it needs to be cleaned up   */
  // if(plant->grain->getFinalGrainNo() < 1)return;

   double gf = plant->grain->getGRFract();
   double biomPool = dltDM;
   
   // allocate (1-gf) * PGR0 (minPGR) to labile pool of stem
   double labilePGR0 = (1 - gf) * (minPGR);                        // Eqn[9i]

   // allocate minimum to stem labile pool
   ((StemCM*)plant->stem)->partitionLabileDM(Min(labilePGR0,biomPool));

   // if we do not have enough to supply the minimum PGR, we have none for grain
   if(biomPool < labilePGR0) 
      {
      plant->grain->calcDltCDemand(0.0);
      plant->grain->dmRetrans(0.0);
      return;
      }

   // SUPPLY
   biomPool -= labilePGR0;                                        // Eqn[9i]

   // get available supply from labile pool
   double labileSupply = gf * plant->stem->dmRetransAvailable();
   double totalSupply = labileSupply + biomPool;                   // Eqn[9ii]

   // Demand
   double totalDemand = plant->grain->calcDltCDemand(biomPool);    // Eqn[9iii]

   // allocation of biomass

   if(totalDemand >= totalSupply)                                 //  Eqn[10]
	  {
	  plant->grain->dmRetrans(totalSupply);                      // EGR
     double grainLeftoverDM = 0;
	  ((StemCM*)plant->stem)->partitionLabileDM(-labileSupply + grainLeftoverDM);
	  }
   else
	  {
	  plant->grain->dmRetrans(totalDemand);
     double grainLeftoverDM = totalDemand - totalSupply;
	  ((StemCM*)plant->stem)->partitionLabileDM(biomPool - totalDemand + grainLeftoverDM);
	  }
   }

