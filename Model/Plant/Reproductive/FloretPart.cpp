#include "StdPlant.h"

#include "../CompositePart.h"
#include "FloretPart.h"
#include "../Phenology/Phenology.h"
#include "../Population.h"
#include "../Environment.h"
using namespace std;

FloretPart::FloretPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : SimplePart(scienceAPI, p, name)
   {
   }

void FloretPart::onInit1(protocol::Component *system)
//=======================================================================================
{
   SimplePart::onInit1(system);

   system->addGettableVar("pai", gPai, "m^2/m^2", "Floret area index");
   system->addGettableVar("dlt_pai", gDlt_pai, "m^2/m^2", "Delta Floret area index");
   system->addGettableVar("dlt_dm_pot_rue_Floret", dlt.dm_pot_rue, "g/m^2", "Potential dry matter production via photosynthesis");

}

void FloretPart::update(void)
//=======================================================================================
{
   SimplePart::update();
   gPai += gDlt_pai;
}

void FloretPart::onHarvest(float /* cutting_height */, float remove_fr,
                             vector<string> &dm_type,
                             vector<float> &dlt_crop_dm,
                             vector<float> &dlt_dm_n,
                             vector<float> &dlt_dm_p,
                             vector<float> &fraction_to_residue)
//=======================================================================================
{
   onHarvest_GenericAboveGroundPart(remove_fr, dm_type, dlt_crop_dm, dlt_dm_n, dlt_dm_p, fraction_to_residue);
}



void FloretPart::doDmDemand(float dlt_dm_supply)
//=======================================================================================
{
   doDmDemand2(dlt_dm_supply);
}

void FloretPart::doDmDemand2(float dlt_dm_supply)
//=======================================================================================
{
   float dlt_dm_supply_by_Floret = 0.0;  // FIXME
   dlt_dm_supply += dlt_dm_supply_by_Floret;
   DMGreenDemand = dlt_dm_supply * fracFloret() - dlt_dm_supply_by_Floret;
}

void FloretPart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   Retranslocation.AddNonStructuralDM(DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0));
   }

float FloretPart::dltDmRetranslocateSupply(float DemandDifferential)
//=======================================================================================
   {
   float DMPartPot = Green.DM() + Retranslocation.DM();
   float DMPartAvail = DMPartPot - Green.StructuralDM();
   DMPartAvail = l_bound (DMPartAvail, 0.0);
   float DltDmRetransPart = min (DemandDifferential, DMPartAvail);
   Retranslocation.AddNonStructuralDM(DltDmRetransPart);  //XXXX this is a bad thing..
   return DltDmRetransPart;
   }

void FloretPart::zeroAllGlobals(void)
//=======================================================================================
{
   SimplePart::zeroAllGlobals();
   coverFloret.green = 0.0;
   coverFloret.sen   = 0.0;
   gPai = 0.0;
}

void FloretPart::zeroDeltas(void)
//=======================================================================================
{
   SimplePart::zeroDeltas();

   gDlt_pai = 0.0;
}


void FloretPart::readConstants (protocol::Component *system, const string &section)
   {
   SimplePart::readConstants(system, section);
   }

void FloretPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
{
   SimplePart::readSpeciesParameters(system, sections);

   scienceAPI.read("transp_eff_cf", c.transpEffCf, 0.0f, 1.0f);
   scienceAPI.read("extinct_coef_Floret", cExtinctionCoeffFloret, 0.0f, 1.0f);
   scienceAPI.read("spec_Floret_area", cSpec_Floret_area, 0.0f, 100000.0f);
   scienceAPI.read("rue_Floret", cRue_Floret, 0.0f, 3.0f);

   //    plant_transp_eff

   scienceAPI.read("svp_fract", cSvp_fract, 0.0f, 1.0f);

   cY_frac_Floret.read(scienceAPI, "x_stage_no_partition", "", 0.0f, 20.0f,
                                   "y_frac_Floret", "", 0.0f, 2.0f);
}

// Query
float FloretPart::coverTotal(void)
//=======================================================================================
{
   return 1.0 - (1.0 - coverFloret.green) * (1.0 - coverFloret.sen);
}

float FloretPart::coverGreen(void)
//=======================================================================================
{
   return coverFloret.green;
}

float FloretPart::coverSen(void)
//=======================================================================================
{
   return coverFloret.sen;
}

void FloretPart::doCover (PlantSpatial &spatial)

   //===========================================================================
{

   //+  Purpose
   //     Calculate Floret cover

   //+  Changes
   //     02 Feb 2005 JNGH - Programmed and Specified

   //+  Local Variables
   float cover;                // Floret cover in canopy
   float coverA;

   //- Implementation Section ----------------------------------

   if (gPai > 0.0)
      {
      coverA = 1.0 - exp(-cExtinctionCoeffFloret * gPai*spatial.canopyFac());
      cover = (float)divide (coverA, spatial.canopyFac(), 0.0);
      }
   else
      cover = 0.0;

   coverFloret.green = cover;
}

float FloretPart::fracFloret (void)
   //===========================================================================
{

   return plant->phenology().doInterpolation(cY_frac_Floret);
}


void FloretPart::doProcessBioDemand(void)
   //===========================================================================
{
}

float FloretPart::DMSupply(void)
   //===========================================================================
{
   //       Takes biomass production limited by radiation and discounted by water supply.
   if (plant->Tops().SWDemand() > 0.0)
      return dlt.dm_pot_rue * plant->getSwdefPhoto();
   else
      return 0.0;
}

void FloretPart::calcDlt_Floret_area (void)
   //===========================================================================
{
   gDlt_pai = Growth.DM() * cSpec_Floret_area * smm2sm;
}

void FloretPart::doDmPotRUE (void )                    // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   //===========================================================================
{
   //       Potential biomass (carbohydrate) production from
   //       photosynthesis (g/m^2).  The effect of factors such
   //       temperature and nutritional status of the plant are
   //       taken into account in the radiation use efficiency.

   double stress_factor = min(min(min(plant->getTempStressPhoto(), plant->getNfactPhoto())
                                  , plant->getOxdefPhoto()), plant->getPfactPhoto());

   dlt.dm_pot_rue = (float)((radiationInterceptedGreen * cRue_Floret) * stress_factor * plant->getCo2Modifier()->rue());
}

void FloretPart::doSWDemand(float SWDemandMaxFactor)         //(OUTPUT) crop water demand (mm)
   //===========================================================================
   /*  Purpose
   *       Return crop water demand from soil by the crop (mm) calculated by
   *       dividing biomass production limited by radiation by transpiration efficiency.
   */
{
   // get potential transpiration from potential
   // carbohydrate production and transpiration efficiency

   cproc_transp_eff_co2_1(plant->environment().vpdEstimate()
                          , plant->phenology().doLookup(c.transpEffCf)
                          , plant->getCo2Modifier()->te()
                          , &transpEff);

   cproc_sw_demand1 (dlt.dm_pot_rue
                     , transpEff
                     , &sw_demand_te);

       // Capping of sw demand will create an effective TE- recalculate it here
       // In an ideal world this should NOT be changed here - NIH

   float SWDemandMax = SWDemandMaxFactor * coverGreen() ;
   sw_demand = u_bound(sw_demand_te, SWDemandMax);
   transpEff = transpEff * divide(sw_demand_te, sw_demand, 1.0);
}


