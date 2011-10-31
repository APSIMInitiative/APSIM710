#include "StdPlant.h"

#include "OilPart.h"
using namespace std;

fruitOilPart::fruitOilPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : SimplePart(scienceAPI, p, name)
   {
   Vegetative.ClearPools();
   VegetativeTotal.ClearPools();
   Grain.AddPool(Green);
   GrainTotal.AddPool(Green);
   GrainTotal.AddPool(Senesced);
   }

void fruitOilPart::onInit1(protocol::Component *system)
//===========================================================================
   {
   SimplePart::onInit1(system);
   system->addGettableVar("dlt_dm_oil_conv_retrans", dmOil_conv_retranslocate, "g/m^2", "change in oil via retranslocation");
   system->addGettableVar("grain_oil_conc", cGrain_oil_conc, "%", "??");
   }

float fruitOilPart::grainEnergy(void)  {return gGrain_energy;}
//=======================================================================================

void fruitOilPart::zeroAllGlobals(void)
//=======================================================================================
// Zero all data
   {
   SimplePart::zeroAllGlobals();
   cCarbo_oil_conv_ratio  = 0.0;
   cGrain_oil_conc  = 0.0;
   gGrain_energy = 0.0;
   dmOil_conv_retranslocate = 0.0;
   }

void fruitOilPart::zeroDeltas(void)
//=======================================================================================
// Zero daily deltas
   {
   SimplePart::zeroDeltas();
   }


void fruitOilPart::onHarvest(float /* cutting_height */, float /*remove_fr*/,
                             vector<string> &dm_type,
                             vector<float> &dlt_crop_dm,
                             vector<float> &dlt_dm_n,
                             vector<float> &dlt_dm_p,
                             vector<float> &fraction_to_residue)
//=======================================================================================
// Event Handler for harvesting event
   {
   // biomass is removed, nothing is sent to surface residues..
   dm_type.push_back (myName);
   fraction_to_residue.push_back (0.0);
   dlt_crop_dm.push_back ((Green.DM()+Senesced.DM()) * gm2kg/sm2ha);
   dlt_dm_n.push_back    ((Green.N()+Senesced.N())  * gm2kg/sm2ha);
   dlt_dm_p.push_back    ((Green.P()+Senesced.P())  * gm2kg/sm2ha);

   Senesced.Clear();
   Green.Clear();
   }



void fruitOilPart::onFlowering(void)
//=======================================================================================
// Event Handler for Flowering event
   {  // do nothing
   }

void fruitOilPart::onStartGrainFill(void)
//=======================================================================================
// Event Handler for the start of grain filling event
   {  // do nothing
   }

void fruitOilPart::doBioGrainOil (void)    // for seed energy content (>= 1.0)
//=======================================================================================
//  Calculate grain oil factors
   {
   gGrain_energy = 1.0 + cGrain_oil_conc * (cCarbo_oil_conv_ratio - 1.0);
   bound_check_real_var (scienceAPI, gGrain_energy, 1.0, 2.0, "grain_energy");
   }

float fruitOilPart::energyAdjustHI (float harvestIndex)
//=======================================================================================
// Returns an adjustment factor for converting biomass to account for oil (high c) content
   {
   return divide (1.0
                 , 1.0 + harvestIndex*(gGrain_energy - 1.0)
                 , 0.0);
   }

float fruitOilPart::addEnergy (float DM)
//=======================================================================================
   {
   return DM * gGrain_energy;
   }

float fruitOilPart::removeEnergy (float DM)
//=======================================================================================
   {
   return divide (DM, gGrain_energy, 0.0);
   }

float fruitOilPart::dltDmGreen(void)
//=======================================================================================
   {
   return Growth.DM() ;
   }

float fruitOilPart::dltDmGreenRetransUptake(void)
//=======================================================================================
   {
   return (Retranslocation.DM() + dmOil_conv_retranslocate);
   }

void fruitOilPart::doDMDemand (float dlt_dm_grain_demand)                                                    //remove
//=======================================================================================
   {                                                                                                           //remove
	doDMDemandGrain (dlt_dm_grain_demand);
   }                                                                                                           //remove

void fruitOilPart::doDMDemandGrain (float dlt_dm_grain_demand)
//=======================================================================================
   {
   float dltDmOil = removeEnergy (dlt_dm_grain_demand) * cGrain_oil_conc;
   float dltDmOilConversion =  removeEnergy (dlt_dm_grain_demand) * (gGrain_energy - 1.0);
   DMGreenDemand = dltDmOil + dltDmOilConversion;
   }

float fruitOilPart::giveDmGreen(float delta)
//=======================================================================================
   {
   float d = divide (delta, cCarbo_oil_conv_ratio, 0.0);
   Growth.AddStructuralDM(d);
   return delta;
   }

void fruitOilPart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   float dltDM = DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0);
   Retranslocation.SetNonStructuralDM(divide (dltDM, cCarbo_oil_conv_ratio, 0.0));
   dmOil_conv_retranslocate = dltDM - Retranslocation.DM();
   }

float fruitOilPart::dmDemandDifferential(void)
//=======================================================================================
   {
   return dmGreenDemand() - (Growth.DM() * cCarbo_oil_conv_ratio);
   }

void fruitOilPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
   {
   SimplePart::readSpeciesParameters(system, sections);

   scienceAPI.read("carbo_oil_conv_ratio", cCarbo_oil_conv_ratio, 0.0f, 20.0f);
   scienceAPI.read("grain_oil_conc", cGrain_oil_conc, 0.0f, 1.0f);
   doBioGrainOil();
   }


