#include "../StdPlant.h"

#include "FruitCohortFN.h"
#include "../Environment.h"
#include "../ThingFactory.h"
using namespace std;

// ##############################################################
// DPH: Most of this file can be removed by relying on
// composite part to do the looping over parts for dmGreenVeg,
// dmSenescedVeg etc. We would need to put in lines in the grain
// parts to return 0.0 for the dmGreenVeg, dmSenescedVeg etc.
// ##############################################################

//  initialise data members.
FruitCohortFN::FruitCohortFN(XMLNode params, ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : FruitCohort(params, scienceAPI, p, name)
{
    string phenologyModel;
    scienceAPI.readOptional("phenology_model", phenologyModel);
    parameters = params;

    fruitPhenology = dynamic_cast<Phenology*> (createThing(parameters, scienceAPI, *plant, "Phenology", phenologyModel));
}

// destructor
FruitCohortFN::~FruitCohortFN()
   // ====================================================================
{
      delete fruitPhenology;
}

float FruitCohortFN::getDltTT(void)
   //===========================================================================
{
   return fruitPhenology->TT();
}

bool  FruitCohortFN::onDayOf(const string &what)
   //===========================================================================
{
   return (fruitPhenology->onDayOf(what));
}

void FruitCohortFN::onInit1(protocol::Component *system)
   //===========================================================================
   {
   zeroAllGlobals(); zeroDeltas();

   fruitPhenology->onInit1(system);

   grainPart = new fruitGrainPartFN(parameters, scienceAPI, plant, this, "grain");

   podPart = new fruitPodPartFN(scienceAPI, plant, "pod");

   add(podPart);
   myVegParts.push_back(podPart);
   supplyPools.push_back(podPart);

   add(grainPart);
   myGrainParts.push_back(grainPart);

   // call into base class.
   FruitCohort::onInit1(system);

   // register some other things.
   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   }

void FruitCohortFN::process (void)
//=======================================================================================
   {
      fruitPhenology->process ();
      FruitCohort::process();
   }


void FruitCohortFN::onPlantEvent(const string &event)
//=======================================================================================
   {
      fruitPhenology->onPlantEvent(event);
      FruitCohort::onPlantEvent(event);
   }

void FruitCohortFN::zeroAllGlobals(void)
   //===========================================================================
{
      fruitPhenology->zeroAllGlobals();
      FruitCohort::zeroAllGlobals();
}

void FruitCohortFN::zeroDeltas(void)
   //===========================================================================
{
      fruitPhenology->zeroDeltas();
      FruitCohort::zeroDeltas();
}

void FruitCohortFN::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
{
      fruitPhenology->readCultivarParameters(system, cultivar);

      rel_grainfill.read(scienceAPI, "x_temp_grainfill", "oC", 0.0, 80.0
                                      , "y_rel_grainfill", "-", 0.0, 1.0);

      scienceAPI.read("potential_fruit_filling_rate", p.potential_fruit_filling_rate, 0.0f, 1.0f);  //'(g/fruit/degday)'
      fracPod.read(scienceAPI, "x_fruit_stage_no_partition", "-", 0.0, 20.0
                              , "y_fruit_frac_pod", "-", 0.0, 2.0);

      sdr_min.read(scienceAPI, "x_stage_supply_demand_ratio", "-", 1.0, 100.0
                             , "y_supply_demand_ratio_min", "-", 0.0, 2.0);

      scienceAPI.read("dm_fruit_max", p.dm_fruit_max, 0.0f, 100.0f);

// Temporarily remove
      scienceAPI.read("dm_abort_fract", c.dm_abort_fract, 0.0f, 1.0f);
//      scienceAPI.read("fract_dm_fruit_abort_crit", c.fract_dm_fruit_abort_crit, 0.0f, 1.0f);
//      scienceAPI.read("fruit_phen_end", c.fruit_phen_end, 0.0f, 1.0f);

   FruitCohort::readCultivarParameters(system, cultivar);
}

void FruitCohortFN::readConstants(protocol::Component *system, const string &section)
   //===========================================================================
{
      fruitPhenology->readConstants(system, section);
      FruitCohort::readConstants(system, section);
}

void FruitCohortFN::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
{
      fruitPhenology->readSpeciesParameters(system, sections);
      FruitCohort::readSpeciesParameters(system, sections);
}

void FruitCohortFN::doGrainNumber(void)
   //===========================================================================
{
      doFruitNumber();
}

// Purpose
//     Get change in plant fruit number
void FruitCohortFN::doFruitNumber(void)
   //===========================================================================
    {
      if (flower_no > 0.0)
         {
         if (fruitPhenology->TTInPhase("fruiting") >= c.tt_flower_to_start_pod)
             dlt_fruit_no = flower_no;  // flowers become fruit
         else
             dlt_fruit_no = 0.0;
         }
      else
         dlt_fruit_no = 0.0;
  }

float FruitCohortFN::fruitNumber(void)
   //===========================================================================
  {
      return fruit_no;
  }

float FruitCohortFN::flowerNumber(void)
   //===========================================================================
  {
      return flower_no;
  }

void FruitCohortFN::giveFlowerNo(float dltFlowerNo)
   //===========================================================================
  {
      flower_no += dltFlowerNo;
  }

float FruitCohortFN::potentialGrainFillRate(void)
   //===========================================================================
{
   return podPart->removePodFraction(p.potential_fruit_filling_rate);
}

float FruitCohortFN::potentialCohortGrainFillRate(void)
   //===========================================================================
{
   return fruit_no * potentialGrainFillRate();
}

float FruitCohortFN::dltDmFruitMax(void)
   //===========================================================================
{
   float dlt_dm_fruit_max = p.dm_fruit_max - divide (Green.DM(), fruit_no, 0.0);
   return l_bound (dlt_dm_fruit_max, 0.0);                             //cohort dm demand - cohort stuff
}

float FruitCohortFN::dltDmGrainMax(void)
   //===========================================================================
{
   return podPart->removePodFraction(dltDmFruitMax());
}

//+  Purpose
//       Perform grain filling calculations
void FruitCohortFN::doDmDemand (float /*dlt_dm_supply_by_veg*/)
//   //===========================================================================
//        float *dlt_dm_grain_demand)                         //(OUTPUT)
{
//   doProcessBioDemand();
//   //       (OUTPUT) assimilate demand for reproductive part (g/m^2)
//   // calculate demands of reproductive parts
//   podPart->doDmDemand(dlt_dm_veg_supply);

//    cohort = 0;
//    do {
//       float dlt_dm_grain_demand = 0.0;
//       float dlt_dm_fruit_demand = 0.0;

       if (fruit_no > 0.0)
           {
           if (plant->phenology().inPhase("flowering"))                 //pod dm demand - pod stuff
               {                                                            //pod dm demand - pod stuff
               // we are in flowering phase                                 //pod dm demand - pod stuff
               float tt_fruit_age_max = fruitPhenology->TTTargetInPhase("fruiting");
               float dlt_fruit_age = divide(plant->phenology().TT()                     //pod dm demand - pod stuff
                                           , tt_fruit_age_max, 0.0);               //pod dm demand - pod stuff
               float dm_max = p.dm_fruit_max                                      //pod dm demand - pod stuff
                            * rel_grainfill.value(plant->environment().meant())                                            //pod dm demand - pod stuff
                            * fruit_no;                                           //pod dm demand - pod stuff
                                                                            //pod dm demand - pod stuff
               float dlt_dm_fruit_demand_pot = dm_max * dlt_fruit_age;                  //pod dm demand - pod stuff
               podPart->doDmDemand(dlt_dm_fruit_demand_pot);                                                             //pod dm demand - pod stuff
               }
           else if (plant->phenology().inPhase("grainfilling"))
               {
               // we are in grain filling stage
//               float frac_pod = linear_interp_real(g_current_stage[cohort]
//                                            ,c_x_stage_no_partition
//                                            ,c_y_frac_pod
//                                            ,c_num_stage_no_partition);
//               float frac_pod = fracPod.value(plant->getStageCode());

////               float potential_grain_filling_rate = divide(p.potential_fruit_filling_rate
////                                                     , 1.0 + frac_pod
////                                                     , 0.0);
//               float potential_grain_filling_rate = potentialGrainFillRate();

//               float dlt_dm_yield = grainPart->dltDmYieldPotential();                                   //cohort dm demand - cohort stuff

              // adjust for grain energy
//              float dlt_dm_grain_demand = oilPart->addEnergy(dlt_dm_yield);              //grain dm demand - grain stuff

//              dlt_dm_pod_demand = dlt_dm_yield * frac_pod;                              //pod dm demand - pod stuff
               podPart->doDmDemand(grainPart->dltDmYieldPotential());
               doProcessBioDemand();
//
//               dlt_dm_fruit_demand = dmGreenDemand();  //cohort dm demand - cohort stuff
//
//               float dlt_dm_fruit_max = dltDmFruitCohortMax();                         //cohort dm demand - cohort stuff
//                      - sum_real_array(&g_dm_fruit_green[cohort][pod], max_part-pod);         //cohort dm demand - cohort stuff
//               dlt_dm_fruit_max = l_bound (dlt_dm_fruit_max, 0.0);                             //cohort dm demand - cohort stuff
//
              // adjust the demands
//              if (dlt_dm_fruit_demand > dlt_dm_fruit_max)                             //grain dm demand - grain stuff
//                  {                                                                           //grain dm demand - grain stuff
//                  dlt_dm_grain_demand = dlt_dm_grain_demand                   //grain dm demand - grain stuff
//                                                 * divide (dlt_dm_fruit_max                   //grain dm demand - grain stuff
//                                                           , dlt_dm_fruit_demand      //grain dm demand - grain stuff
//                                                           , 0.0);                            //grain dm demand - grain stuff
//                  dlt_dm_fruit_demand = dlt_dm_fruit_max;
//                  }                                                                           //cohort dm demand - cohort stuff
              }
           else
              {
              // no changes
//              dlt_dm_grain_demand = 0.0;
//              dlt_dm_fruit_demand = 0.0;
              }
           }
       else
           {
           // no fruit
//           dlt_dm_grain_demand = 0.0;
//           dlt_dm_fruit_demand = 0.0;
           }
////       cohort++;
////       } while (cohort < g_num_fruit_cohorts);
////       pop_routine (my_name);
   }

