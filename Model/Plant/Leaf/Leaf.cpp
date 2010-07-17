#include "../StdPlant.h"

#include "../CompositePart.h"
#include "Leaf.h"
#include "GenericLeaf.h"
#include "CohortingLeaf.h"
#include "../Photosynthesis/PhotosynthesisModel.h"
#include "../Phenology/Phenology.h"
#include "../Environment.h"

using namespace std;

Leaf::Leaf(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : SimplePart(scienceAPI, p, name)
   {
   Photosynthesis=constructPhotosynthesisModel(scienceAPI, *p);
   }

void Leaf::onInit1(protocol::Component *)
   {
   scienceAPI.subscribe("canopy_water_balance", CanopyWaterBalanceFunction(&Leaf::onCanopyWaterBalance));
   }

void Leaf::onEndCrop(vector<string> &dm_type,
                     vector<float> &dlt_crop_dm,
                     vector<float> &dlt_dm_n,
                     vector<float> &dlt_dm_p,
                     vector<float> &fraction_to_residue)
{
   SimplePart::onEndCrop(dm_type, dlt_crop_dm, dlt_dm_n, dlt_dm_p, fraction_to_residue);
   zeroAllGlobals();
}

void Leaf::onCanopyWaterBalance(protocol::CanopyWaterBalanceType &CWB)
//=======================================================================================
// Handler for CanopyWaterBalance event
   {
   ExternalSWDemand = false;
   for (unsigned i = 0; i < CWB.Canopy.size(); i++)
      {
      if (CWB.Canopy[i].name == plant->Name())
         {
         sw_demand = CWB.Canopy[i].PotentialEp;
         ExternalSWDemand = true;
         }
      }
   }

void Leaf::doNConccentrationLimits(float modifier)
{
   SimplePart::doNConccentrationLimits(modifier);
   g.n_conc_crit *= modifier;
   if (g.n_conc_crit <= g.n_conc_min)
      throw std::runtime_error("Aiieeee!!\nnconc_crit < nconc_min!\nWhat's happened to CO2??");
}


float Leaf::dmRetransSupply(void)
  {
  float dm_part_avail = Green.NonStructuralDM();
  return (l_bound (dm_part_avail, 0.0));
  }

void Leaf::doSWDemand(float SWDemandMaxFactor)         //(OUTPUT) crop water demand (mm)
   //===========================================================================
   /*  Purpose
   */
   {
   if (ExternalSWDemand == true)
      {
      transpEff = dlt.dm_pot_rue/sw_demand;
      ExternalSWDemand = false;
      }
   else
      {
      //*       Return crop water demand from soil by the crop (mm) calculated by
      //*       dividing biomass production limited by radiation by transpiration efficiency.
      // get potential transpiration from potential
      // carbohydrate production and transpiration efficiency

   cproc_transp_eff_co2_1(plant->environment().vpdEstimate()
                          , plant->phenology().doLookup(c.transpEffCf)
                          , plant->getCo2Modifier()->te()
                          , &transpEff);

      cproc_sw_demand1 (dlt.dm_pot_rue - Respiration()
                        , transpEff
                        , &sw_demand_te);

          // Capping of sw demand will create an effective TE- recalculate it here
          // In an ideal world this should NOT be changed here - NIH

      float SWDemandMax = SWDemandMaxFactor * coverGreen() ;
      sw_demand = u_bound(sw_demand_te, SWDemandMax);
      transpEff = transpEff * divide(sw_demand_te, sw_demand, 1.0);
      }
   }


float Leaf::DMSupply(void)
   //===========================================================================
{
   //       Takes biomass production limited by radiation and discounted by water supply.
   if (plant->Tops().SWDemand() > 0.0)
      return dlt.dm_pot_rue * plant->getSwdefPhoto();
   else
      return 0.0;
}

float Leaf::coverTotal(void)
//=======================================================================================
{
   return 1.0 - (1.0 - coverLeaf.green) * (1.0 - coverLeaf.sen);
}

float Leaf::coverGreen(void)
//=======================================================================================
{
   return coverLeaf.green;
}

float Leaf::coverSen(void)
//=======================================================================================
{
   return coverLeaf.sen;
}

void Leaf::doCover (PlantSpatial &spatial)
   //===========================================================================
{

   //+  Purpose
   //     Calculate leaf cover

   //+  Changes
   //     19 Jan 2006 JNGH - Programmed and Specified

   //- Implementation Section ----------------------------------

    legnew_cover(spatial.rowSpacing()
                 , cXRowSpacing
                 , cYExtinctCoef
                 , cNumRowSpacing
                 , spatial.canopyFac()
                 , getLAI()
                 , &coverLeaf.green);


    legnew_cover (spatial.rowSpacing()
                 , cXRowSpacing
                 , cYExtinctCoefDead
                 , cNumRowSpacing
                 , spatial.canopyFac()
                 , getSLAI()
                 , &coverLeaf.sen);

}

void Leaf::doDmPotRUE (void )                    // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   //===========================================================================
   {
   //       Potential biomass (carbohydrate) production from
   //       photosynthesis (g/m^2).  The effect of factors such
   //       temperature and nutritional status of the plant are
   //       taken into account in the radiation use efficiency.

   dlt.dm_pot_rue = Photosynthesis->PotentialDM(radiationInterceptedGreen);
   }


