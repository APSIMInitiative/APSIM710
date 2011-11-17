#include "StdPlant.h"

#include "../Leaf/Leaf.h"
#include "Arbitrator.h"
#include "../CompositePart.h"
#include "../Root/RootBase.h"
#include "../Phenology/Phenology.h"
#include "WholePlantGenericArbitratorXY.h"

Arbitrator::Arbitrator(ScienceAPI& scienceAPI, plantInterface& p)
   : plantThing(scienceAPI, "arbitrator"), plant(p)
   {
   scienceAPI.read("n_retrans_option", n_retrans_option, 1, 2);
   scienceAPI.read("partitionparts", PartitionParts);
   scienceAPI.read("partitionrules", PartitionRules);
   scienceAPI.expose("dlt_dm", "g/m^2",  "Actual above_ground dry matter production", _DMSupply);
   zeroAllGlobals();
   }
void Arbitrator::zeroAllGlobals()
   {
   _DMSupply = 0.0;
   }
void Arbitrator::partitionDM()
   {
   // Calculate Actual DM increase from photosynthesis
   _DMSupply = plant.All().DMSupply();

   // Now calculate DM demands
   plant.All().doDmDemand(_DMSupply);

   plant.All().zeroDltDmGreen();

   float dm_remaining = _DMSupply;
   float dlt_dm_green_tot = 0.0;

   for (unsigned i = 0; i != PartitionParts.size(); i++)
      {
      plantPart& Part = plant.All().find(PartitionParts[i]);

      Part.zeroDltDmGreen();
      if (PartitionRules[i] == "magic")
         Part.giveDmGreen(ratioRootShoot() * DMSupply());
      else if (PartitionRules[i] == "seasonal")                  // (PFR)
         {
         float uptake = ratioRootPlant() * dm_remaining;
         Part.giveDmGreen(uptake);                                // (PFR)
         dm_remaining = dm_remaining - uptake;                    // Here total RUE is used so remaining discounts root uptake(PFR)
         dlt_dm_green_tot = dlt_dm_green_tot + uptake;
         }

      else
         {
         float uptake;
         if (PartitionRules[i] == "demand")
            uptake = min(Part.dmGreenDemand(), dm_remaining);
         else if (PartitionRules[i] == "frac")
            uptake = min(fracDMRemainingInPart(i) * dm_remaining,Part.dmGreenDemand());
         else if (PartitionRules[i] == "remainder")
            uptake = dm_remaining;
         else
            throw runtime_error("Unknown Partition Rule "+PartitionRules[i]);

         Part.giveDmGreen(uptake);
         dm_remaining = dm_remaining - uptake;
         dlt_dm_green_tot = dlt_dm_green_tot + uptake;
         }
      }


   if (!reals_are_equal(dlt_dm_green_tot, DMSupply(), 1.0E-4f))
       {
       string msg = "dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(DMSupply(), ".6");
       scienceAPI.warning(msg);
       }
   }

float Arbitrator::RelativeGrowthRate(void)
   {
   // the dlt_dm_pot_rue is only tops, thus either adjust it for roots or leave roots out of the divisor.
   return divide(TotalPotentialGrowthRate(), plant.All().Green.DM(), 0.0);
   }
float Arbitrator::TotalPotentialGrowthRate(void)
   {
   // the dlt_dm_pot_rue is only tops, thus adjust it for roots.
   return (1.0f + ratioRootShoot()) * plant.All().dltDmPotRue();
   }

void Arbitrator::doNRetranslocate(plantPart* fruitPart)
   {
   //=======================================================================================
   // Do Plant Nitrogen Retranslocation
   if (n_retrans_option == 1)
      {
      doNRetranslocate(fruitPart->nDemandGrain());
      }
   else if (n_retrans_option == 2)
      {
      doNRetranslocate(fruitPart->nDemandGrain2());  //FIXME
      }
   }

void Arbitrator::doNRetranslocate(float g_grain_n_demand)
   {
   // Calculate the nitrogen retranslocation from the various plant parts
   // to the grain.

   //! available N does not include roots or grain
   //! this should not presume roots and grain are 0.
   // grain N potential (supply)
   plant.Tops().doNRetranslocate(plant.Tops().availableRetranslocateN(), g_grain_n_demand);
   }

void Arbitrator::doNPartition(float g_n_fix_pot, float nDemandTotal, float& nFixUptake)
   {
   //=======================================================================================
   // Calculate the nitrogen partitioning in the plant

   // find the proportion of uptake to be distributed to
   // each plant part and distribute it.
   float nUptakeSum = plant.root().nUptake();     // total plant N uptake (g/m^2)
   plant.All().doNPartition(nUptakeSum, nDemandTotal, plant.All().nCapacity());

   // Check Mass Balance
   if (!reals_are_equal(plant.All().Growth.N() - nUptakeSum, 0.0))
      {
      string msg ="Crop dlt_n_green mass balance is off: dlt_n_green_sum ="
                    + ftoa(plant.All().Growth.N(), ".6")
                    + " vs n_uptake_sum ="
                    + ftoa(nUptakeSum, ".6");
      scienceAPI.warning(msg);
      }

   // Retranslocate N Fixed
   float nFixDemandTotal = l_bound (nDemandTotal - nUptakeSum, 0.0); // total demand for N fixation (g/m^2)
   nFixUptake = bound (g_n_fix_pot, 0.0, nFixDemandTotal);

   plant.All().doNFixRetranslocate (nFixUptake, nFixDemandTotal);
   }

void Arbitrator::doDmRetranslocate(plantPart* stemPart, plantPart* leafPart,
                                   plantPart* fruitPart)
   {
//+  Purpose
//     Calculate plant dry matter delta's due to retranslocation
//     to grain, pod and energy (g/m^2)

    vector<plantPart *>::iterator part;

    float dlt_dm_retrans_part;                    // carbohydrate removed from part (g/m^2)
    float dm_part_avail;                          // carbohydrate avail from part(g/m^2)
    float dm_retranslocate = 0.0;

//- Implementation Section ----------------------------------
   vector<plantPart *> supplyPoolsByVeg;
   supplyPoolsByVeg.push_back(stemPart);
   supplyPoolsByVeg.push_back(leafPart);


// now translocate carbohydrate between plant components
// this is different for each stage

    plant.All().dlt_dm_green_retrans_hack( 0.0 );

    float demand_differential_begin = fruitPart->dmDemandDifferential ();   //FIXME - should be returned from a fruitPart method
    float demand_differential = demand_differential_begin;

    // get available carbohydrate from supply pools
    for (part = supplyPoolsByVeg.begin(); part != supplyPoolsByVeg.end(); part++)
        {
           dm_part_avail = (*part)->dmRetransSupply();

           dlt_dm_retrans_part = min (demand_differential, dm_part_avail);

           //assign and accumulate

           dm_retranslocate += (*part)->dlt_dm_green_retrans_hack( - dlt_dm_retrans_part);

           demand_differential = demand_differential - dlt_dm_retrans_part;
        }

    float dlt_dm_retrans_to_fruit = - dm_retranslocate;

    fruitPart->doDmRetranslocate (dlt_dm_retrans_to_fruit, demand_differential_begin);

   // Finally, a mass balance check
//   float mbSum = 0.0;                                                    //FIXME - need to reinstate this check
//   for (vector<plantPart *>::iterator part = myParts.begin();
//        part != myParts.end();
//        part++)
//       mbSum += (*part)->dlt_dm_green_retrans();
//
//   if (fabs(mbSum) > 0.001)
//      {
//      string msg ="Crop dm retranslocate mass balance is off: error="
//              + ftoa(mbSum, ".6")
//              + "\n";
//      string msg;
//      for (vector<plantPart *>::iterator part = myParts.begin();
//           part != myParts.end();
//           part++)
//         msg += (*part)->name() + "=" +
//                  ftoa((*part)->dltDmGreenRetransUptake(), ".6") +"\n";
//
//      msg += "dlt_dm_retrans_to_fruit = " + ftoa(dlt_dm_retrans_to_fruit, ".6") + "\n";
//            fprintf(stdout,"%s",msg.c_str()) ;
//
//      parent->warningError(msg.c_str());
//      }
}

void Arbitrator::doNSenescedRetrans(plantPart* leafPart)
//=====================================================================
//      Derives seneseced plant nitrogen (g N/m^2)
   {
   float    dlt_n_in_senescing_leaf;
   float    navail;
   float    n_demand_tot;

   //! now get N to retranslocate out of senescing leaves
   plant.All().zeroDltNSenescedTrans();

   dlt_n_in_senescing_leaf = leafPart->Senescing.DM() * leafPart->Green.Nconc();

   n_demand_tot = plant.All().nDemand();

   navail = dlt_n_in_senescing_leaf - leafPart->Senescing.N();
   navail = bound(navail, 0.0, n_demand_tot);

   plant.All().doNSenescedRetrans(navail, n_demand_tot);
   }
