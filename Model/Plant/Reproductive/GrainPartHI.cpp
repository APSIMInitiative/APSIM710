#include "StdPlant.h"

#include "GrainPartHI.h"
#include "Environment.h"
#include "Phenology/Phenology.h"
#include "Arbitrators/arbitrator.h"

using namespace std;

inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

//  initialise data members.
fruitGrainPartHI::fruitGrainPartHI(XMLNode parameters, ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : fruitGrainPart(parameters, scienceAPI, p, name)
{
}

// destructor
fruitGrainPartHI::~fruitGrainPartHI()
{
}

ostream &operator<<(ostream &output, const fruitGrainPartHI /*&pool*/)
{
   //   output << "fruitGrainPartHI:" << endl;
   //   output << "   Green meal:    " << pool.green.meal << endl;
   //   output << "   Senesced meal: " << pool.senesced.meal << endl;
   //   output << "   Dead meal:     " << pool.dead.meal << endl << endl;
   output << endl;
   return output;
}

// Assigment operator
//  assign data members of object

const fruitGrainPartHI &fruitGrainPartHI::operator=(const fruitGrainPartHI &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for fruitGrainPartHI");
}

void fruitGrainPartHI::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
   {
   fruitGrainPart::readCultivarParameters (system, cultivar);

   //  plant_dm_grain_hi
   scienceAPI.read("x_pp_hi_incr", pX_pp_hi_incr, pNum_pp_hi_incr, 0.0f, 24.0f);
   scienceAPI.read("y_hi_incr", pY_hi_incr, pNum_pp_hi_incr, 0.0f, 1.0f);
   scienceAPI.read("x_hi_max_pot_stress", pX_hi_max_pot_stress, pNum_hi_max_pot, 0.0f, 1.0f);
   scienceAPI.read("y_hi_max_pot", pY_hi_max_pot, pNum_hi_max_pot, 0.0f, 1.0f);
   }

void fruitGrainPartHI::writeCultivarInfo (protocol::Component *system)
   //===========================================================================
{

   // report

   ostringstream msg;
   msg.flags ( ios::right | ios::fixed);
   msg.precision(2);
   msg << "   x_pp_hi_incr               = ";
   for (int i = 0; i < pNum_pp_hi_incr; i++)
      msg << setw(10) << pX_pp_hi_incr[i] << " ";
   msg << endl;

   msg.precision(4);
   msg << "   y_hi_incr                  = ";
   for (int i = 0; i < pNum_pp_hi_incr; i++)
      msg << setw(10) << pY_hi_incr[i] << " ";
   msg << endl;

   msg.precision(2);
   msg << "   x_hi_max_pot_stress        = ";
   for (int i = 0; i < pNum_hi_max_pot; i++)
      msg << setw(10) << pX_hi_max_pot_stress[i] << " ";
   msg << endl;

   msg << "   y_hi_max_pot               = ";
   for (int i = 0; i < pNum_hi_max_pot; i++)
      msg << setw(10) << pY_hi_max_pot[i] << " ";
   msg << ends;

   system->writeString (msg.str().c_str());

}

void fruitGrainPartHI::zeroAllGlobals(void)
{
   fruitGrainPart::zeroAllGlobals();

   fill_real_array (pX_pp_hi_incr, 0.0, max_table);

   fill_real_array (pX_pp_hi_incr, 0.0, max_table);
   fill_real_array (pY_hi_incr, 0.0, max_table);
   pNum_pp_hi_incr = 0;
   pNum_hi_max_pot = 0;
   fill_real_array (pX_hi_max_pot_stress, 0.0, max_table);
   fill_real_array (pY_hi_max_pot, 0.0, max_table);


}


void fruitGrainPartHI::doProcessBioDemand(void)
   //===========================================================================
{

   doDMDemandStress ();
   doDMDemandGrain ();
}

void fruitGrainPartHI::doDMDemandGrain(void)
   //===========================================================================
{
   //        Find grain demand for carbohydrate using harvest index (g/m^2)


   //+  Local Variables
   float dlt_dm_yield;                           // grain demand for carbohydrate (g/m^2)
   float dlt_dm_yield_unadj;                     // grain demand for carbohydrate, unadjusted
   float harvest_index;                          // last harvest index (g grain/g biomass)
   float hi_max_pot;                             // max potential HI due to stress
   float dm_tops_new;                            // new drymatter  tops (g/m^2)
   float harvest_index_new;                      // next harvest index (g grain/g biomass)
   float dm_grain_new;                           // new drymatter grain (g/m^2)
   float hi_incr;                                // harvest index increment per day
   float photoperiod;                            // hours of photosynthetic light (hours)
   float dlt_dm_grain_demand;                    // total dm demand of grain (g/m^2)

   //- Implementation Section ----------------------------------

   if (plant->phenology().inPhase("grainfill"))
      {

      hi_max_pot = linear_interp_real(gDm_stress_max.getAverage()
                                      ,pX_hi_max_pot_stress
                                      ,pY_hi_max_pot
                                      ,pNum_hi_max_pot);

      photoperiod = plant->environment().dayLength(cTwilight);

      hi_incr = linear_interp_real(photoperiod
                                   ,pX_pp_hi_incr
                                   ,pY_hi_incr
                                   ,pNum_pp_hi_incr);

      // effective grain filling period
      float dm_green_yield_parts = mealPart->Green.DM() + oilPart->Green.DM();

      harvest_index = divide (dm_green_yield_parts, plant->Tops().Total.DM(), 0.0);
      dm_tops_new = plant->Tops().Total.DM() + plant->arbitrator().DMSupply();    // unadjusted for Grain Energy in dlt_dm

      harvest_index_new = u_bound (harvest_index + hi_incr, hi_max_pot);

      dm_grain_new = dm_tops_new * harvest_index_new;            // unadjusted for Grain Energy in dlt_dm
      dlt_dm_yield_unadj = dm_grain_new - dm_green_yield_parts;  // unadjusted for Grain Energy in dlt_dm

      // adjust for grain energy

      dlt_dm_yield_unadj = bound (dlt_dm_yield_unadj, 0.0, dm_grain_new);

      dlt_dm_yield = dlt_dm_yield_unadj * oilPart->energyAdjustHI(harvest_index_new);  // finally adjust for Grain energy used from dlt_dm - this is the potential grain wt

      dlt_dm_grain_demand = oilPart->addEnergy(dlt_dm_yield);   // adding grain energy to potential new grain wt to get grain demand


      // delay grainfill after cold snap
      if (plant->environment().mint() <= pMinTempGrnFill)
         gDelayGrnFill = true;
      if (gDelayGrnFill)
         {
         dlt_dm_grain_demand = 0.0;
         gDaysDelayedGrnFill = gDaysDelayedGrnFill + 1;
         if (gDaysDelayedGrnFill == pDaysDelayGrnFill)
            {
            gDelayGrnFill = false ;
            gDaysDelayedGrnFill = 0;
            }
         }
      }
   else
      {
      // we are out of grain fill period
         dlt_dm_grain_demand = 0.0;
      }
   gDlt_dm_grain_demand = dlt_dm_grain_demand;

      oilPart->doDMDemandGrain(dlt_dm_grain_demand);
      mealPart->doDMDemandGrain(dlt_dm_grain_demand - oilPart->dmGreenDemand());
}


