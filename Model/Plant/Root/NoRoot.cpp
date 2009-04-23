#include "StdPlant.h"

#include "NoRoot.h"
#include "RootGrowthOption1.h"
#include "RootGrowthOption2.h"

using namespace std;

NoRoot::NoRoot(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : RootBase(scienceAPI, p, name)
//=======================================================================================
// Constructor
   {
   }

void NoRoot::read()
   {
   scienceAPI.read("n_uptake_option", n_uptake_option, 1, 3);
   }
void NoRoot::write()
//=======================================================================================
// Write all parameters as a summary to stdout.
   {
   cout << "         No Roots At All - Potential uptakes assumed." << endl;
   }


float NoRoot::waterUptake (void)
//=======================================================================================
// Return the total daily water uptake from this root system
   {
   return SWDemand;
   }

void NoRoot::doWaterUptakeInternal (float sw_demand_in)
//=======================================================================================
// Calculate todays daily water uptake by this root system
   {
   SWDemand = sw_demand_in;
   }

void NoRoot::doPlantWaterStress (float /*sw_demand*/, SWStress *swStress)
//     ===========================================================
//         Get current water stress factors (0-1)
   {
   swStress->swDef = 1.0;
   }

//int NoRoot::find_layer_no(float depth)
////=======================================================================================
//// Return the index of the layer corresponding to the given depth
//   {
//   return 1;
//   }

float NoRoot::sw_avail_ratio(int /*layer*/)  //(INPUT) soil profile layer number
//===========================================================================
//     Get the soil water availability factor in a layer.  For a layer,
//     it is 1.0 unless the plant-extractable soil water declines
//     below a fraction of plant-extractable soil water capacity for
//     that layer.
   {
   return 1.0;
   }


//int NoRoot::find_layer_no(float depth,      // depth in profile
//                  float *dlayr,     // layer depth array
//                  int num_layers)   // lowest layer
////===========================================================================
//   {
//   return 1;
//   }
//
//int NoRoot::find_layer_no(float depth, const vector<float> &dlayer )
////===========================================================================
//   {
//   return 1;                                // index of
//   }
//


void NoRoot::plant_nit_supply()
//=======================================================================================
// Calculate Plant Nitrogen Supply
    {
    }


float NoRoot::peswTotal()
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   return 0.0;
   }

float NoRoot::pesw(int /*depth*/)
//=======================================================================================
// Calculate plant extractable soil water at the given depth.
   {
   return 1.0;
   }

float NoRoot::nUptake()
//=======================================================================================
// find the proportion of uptake to be distributed
   {
   return NDemand;
   }

float NoRoot::fasw(int /*depth*/)
//=======================================================================================
// calculate the fraction of available soil water at the given depth (mm)
   {
   return 1.0;
   }


void NoRoot::doNUptake(float /*sumNMax*/, float sumSoilNDemand, float nDemand, float /*n_fix_pot*/)
//=======================================================================================
//       Find nitrogen uptake.
    {
    if (n_uptake_option == 1)
       NDemand = nDemand ;
    else
       NDemand = sumSoilNDemand ;
    }

void NoRoot::onInit1(protocol::Component *system)
//=======================================================================================
// Perform all component initialisation.
   {
   SimplePart::onInit1(system);
   system->addGettableVar("ep",
               SWDemand, "mm", "ep");

   }

//+  Purpose
//       Plant transpiration and soil water extraction
void NoRoot::doWaterUptake (int /*option*/, float swDemand)
    {
    SWDemand = swDemand;
    }
