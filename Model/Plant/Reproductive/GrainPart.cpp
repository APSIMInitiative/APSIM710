#include "StdPlant.h"

#include "GrainPart.h"
#include "../Environment.h"
#include "../Phenology/Phenology.h"
using namespace std;

inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

//  initialise data members.
fruitGrainPart::fruitGrainPart(XMLNode parameters, ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : CompositePart(parameters, scienceAPI, p, name)
{
   gDm_stress_max.setup(&gDlt_dm_stress_max);
   otherObservers.addObserver(&gDm_stress_max);
}

// destructor
fruitGrainPart::~fruitGrainPart()
{
}


void fruitGrainPart::onInit1(protocol::Component *system)
   //===========================================================================
{
   mealPart = new fruitMealPart(scienceAPI, plant, "meal");
   add(mealPart);

   oilPart = new fruitOilPart(scienceAPI, plant, "oil");
   add(oilPart);

   CompositePart::onInit1(system);

   system->addGettableVar("dlt_dm_grain_demand",gDlt_dm_grain_demand, "g/m^2", "??");
   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   setupGetFunction(system, "grain_wt", protocol::DTsingle, false, &fruitGrainPart::get_grain_wt, "g/m^2", "Weight of grain");
   setupGetFunction(system, "yield", protocol::DTsingle, false,&fruitGrainPart::get_yield,  "kg/ha", "Yield");
   setupGetFunction(system, "yieldwet", protocol::DTsingle, false,&fruitGrainPart::get_yield_wet,  "kg/ha", "Yield");
   system->addGettableVar("grain_n_demand", gN_grain_demand, "g/m^2", "N demand of grain");

   setupGetFunction(system, "n_grain_pcnt", protocol::DTsingle, false, &fruitGrainPart::get_n_conc_grain, "%", "N concentration in grain");
   setupGetFunction(system, "n_conc_grain", protocol::DTsingle, false, &fruitGrainPart::get_n_conc_grain, "%", "N concentration in grain");
   setupGetFunction(system, "grain_protein", protocol::DTsingle, false, &fruitGrainPart::get_grain_protein, "%", "grain protein content");
   setupGetFunction(system, "n_conc_meal", protocol::DTsingle, false, &fruitGrainPart::get_n_conc_meal, "%", "meal N content");
   setupGetFunction(system, "grain_n", protocol::DTsingle, false, &fruitGrainPart::get_grain_n, "g/m^2", "N in grain");

   setupGetFunction(system, "grain_p", protocol::DTsingle, false, &fruitGrainPart::get_grain_p, "g/m^2","P in grain");

   system->addGettableVar("grain_p_demand",  gP_grain_demand, "g/m^2","P demand of grain");
}

void fruitGrainPart::get_grain_wt(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, Green.DM());
}

void fruitGrainPart::get_grain_n(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, Green.N());
}

void fruitGrainPart::get_grain_n_demand(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, gN_grain_demand);
}

void fruitGrainPart::get_n_conc_grain(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   float n_conc = nConcPercent();
   system->sendVariable(qd, n_conc);
}

void fruitGrainPart::get_grain_protein(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   float gp = nConcPercent() * 5.71;
   system->sendVariable(qd, gp);
}

void fruitGrainPart::get_n_conc_meal(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   float n_conc = mealPart->Green.NconcPercent();
   system->sendVariable(qd, n_conc);
}

void fruitGrainPart::get_yield(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, Total.DM() * gm2kg / sm2ha);
}

void fruitGrainPart::get_yield_wet(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, Total.DM() * gm2kg / sm2ha / (1.0 - cGrn_water_cont));
}

void fruitGrainPart::get_grain_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
   //===========================================================================
{
   float grain_p = Green.P();
   systemInterface->sendVariable(qd, grain_p);  //()
}

void fruitGrainPart::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
{
   string s;

   if (!scienceAPI.readOptional("min_temp_grnfill", pMinTempGrnFill, 0.0f, 20.0f))
      {
      pMinTempGrnFill = -100.0;
      pDaysDelayGrnFill = 0;
      }
   else
      {
      scienceAPI.read("days_delay_grnfill", pDaysDelayGrnFill, 0, 10);
      }

   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->readCultivarParameters(system, cultivar);
}

void fruitGrainPart::writeCultivarInfo (protocol::Component */* system*/)
   //===========================================================================
{
}

void fruitGrainPart::morphology(void)
{
}

void fruitGrainPart::zeroAllGlobals(void)
{
   plantPart::zeroAllGlobals();

   gHasreadconstants = false;

   gDelayGrnFill  = false;
   gDaysDelayedGrnFill  = 0;
   cSw_fac_max  = 0.0;
   cTemp_fac_min  = 0.0;
   cSfac_slope  = 0.0;
   cTfac_slope  = 0.0;
   cGrn_water_cont  = 0.0;
   cN_conc_crit_grain  = 0.0;
   cN_conc_max_grain  = 0.0;
   cN_conc_min_grain  = 0.0;

   cTwilight = 0.0;

   pMinTempGrnFill = 0.0;
   pDaysDelayGrnFill = 0;

   gDlt_dm_stress_max        = 0.0;


   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->zeroAllGlobals();

}

void fruitGrainPart::zeroDeltas(void)
{
   plantPart::zeroDeltas();

   gDlt_dm_grain_demand = 0.0;
   gDlt_dm = 0.0;

   gN_grain_demand = 0.0;
   gP_grain_demand = 0.0;

   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->zeroDeltas();

}

void fruitGrainPart::onKillStem(void)
   // ====================================================================
{
   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->onKillStem();
}




void fruitGrainPart::readConstants(protocol::Component *system, const string &section)
   //===========================================================================
{
   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->readConstants(system, section);

}

void fruitGrainPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
   {
   scienceAPI.read("sw_fac_max", cSw_fac_max, 0.0f, 100.0f);
   scienceAPI.read("temp_fac_min", cTemp_fac_min, 0.0f, 100.0f);
   scienceAPI.read("sfac_slope", cSfac_slope, -10.0f, 0.0f);
   scienceAPI.read("tfac_slope", cTfac_slope, 0.0f, 100.0f);
   scienceAPI.read("grn_water_cont", cGrn_water_cont, 0.0f, 1.0f);
   scienceAPI.read("n_conc_crit_grain", cN_conc_crit_grain, 0.0f, 100.0f);
   scienceAPI.read("n_conc_max_grain", cN_conc_max_grain, 0.0f, 100.0f);
   scienceAPI.read("n_conc_min_grain", cN_conc_min_grain, 0.0f, 100.0f);

   gHasreadconstants = true;

   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->readSpeciesParameters(system, sections);

}

void fruitGrainPart::update(void)
   //===========================================================================
{

   CompositePart::update();

   if (plant->phenology().inPhase("hi_stress_sensitive")) gDm_stress_max.update();
}

void fruitGrainPart::display(ostream &os)
{
   //   os << "fruitGrainPart:" << endl;
   //   os << "Green meal: " << green.meal << endl;
   //   os << "Senesced meal: " << senesced.meal << endl;
   //   os << "Dead meal: " << dead.meal << endl << endl;
   os << endl;
}


void fruitGrainPart::doProcessBioDemand(void)
   //===========================================================================
{

   doDMDemandStress ();
   doDMDemandGrain ();
}

float fruitGrainPart::nDemandGrain(void)  {return gN_grain_demand;}

float fruitGrainPart::nConcPercent(void)  {return Total.NconcPercent();}   //remove
float fruitGrainPart::dltDmDemand(void) {return gDlt_dm_grain_demand;}                               //remove
float fruitGrainPart::dltDmGrainDemand(void)  {return gDlt_dm_grain_demand;}

float fruitGrainPart::meanT (void) {return plant->environment().meant();}

void fruitGrainPart::doDMDemandGrain(void)
   //===========================================================================
{
}

void fruitGrainPart::doNDemandGrain(float nfact_grain_conc      //   (INPUT)
                                     , float swdef_expansion)    //   grain N demand (g/m^2)
   //===========================================================================
{
   //    Calculate plant n demand

//   float   n_potential;           // maximum grain N demand (g/m^2)
   if (plant->phenology().inPhase("grainfill"))
   {
      doNConcGrainLimits();

      gN_grain_demand = (mealPart->Growth.DM() + mealPart->Retranslocation.DM())
                      * dltNGrainConc(cSfac_slope
                                     , cSw_fac_max
                                     , cTemp_fac_min
                                     , cTfac_slope
                                     , meanT()
                                     , nfact_grain_conc
                                     , swdef_expansion);

      gN_grain_demand = u_bound (gN_grain_demand, mealPart->nCapacity2());
   }
}

float fruitGrainPart::dltNGrainConc(float sfac_slope      //(INPUT)  soil water stress factor slope
                                    , float sw_fac_max      //(INPUT)  soil water stress factor maximum
                                    , float temp_fac_min    //(INPUT)  temperature stress factor minimum optimum temp
                                    , float tfac_slope      //(INPUT)  temperature stress factor slope
                                    , float ave_temp        //(INPUT)  mean air temperature (oC)
                                    , float nfact_grain_conc// (INPUT)
                                    , float swdef_expansion) // (INPUT)
   //==========================================================================

   /*  Purpose
      *     Calculate the nitrogen concentration required to meet the increase
      *     from daily grain growth (0-1) as affected by temperature and water stress.
      *
      *
      *  Notes
      *     First, two factors are calculated and used to estimate the
      *     effects of mean temperature and drought stress on the N
      *     concentration in grain growth for the day.  High temperature
      *     or drought stress can cause the factors to exceed 1.
      *     N deficiency can cause nfac < 1.  The net effect of these
      *     equations is to allow grain nitrogen concentration to range
      *     from less than .01 when N deficiency is severe to about .018
      *     stress limit grain growth.
      *     Here, optimum N concentration = 1.7%
      *
      */
{
   //  Local Variables
   float N_conc_pot;                   // potential grain N concentration (0-1) (g N/g part)
   float N_grain_sw_fac;               // soil water stress factor for N uptake
   float N_grain_temp_fac;             // temperature stress factor for N uptake

   //!!!!!!!!!! return to orig cm
   N_grain_temp_fac = temp_fac_min + tfac_slope * ave_temp;
   N_grain_sw_fac = sw_fac_max - sfac_slope * swdef_expansion ;

   // N stress reduces grain N concentration below critical
   N_conc_pot = mealPart->N_conc_pot(nfact_grain_conc);

   // Temperature and water stresses can decrease/increase grain
   // N concentration

   // when there is no N stress, the following can be a higher N conc than
   // the crit and thus the N conc of the grain can exceed N critical.

   return  (N_conc_pot * max (N_grain_temp_fac, N_grain_sw_fac));
}

float fruitGrainPart::dltDmPotentialGrain (void)
   //===========================================================================
{
   //       Calculate grain dm yield demand (g/m^2)
   //       (OUTPUT) assimilate demand for reproductive part (g/m^2)

   return oilPart->removeEnergy(gDlt_dm_grain_demand);
}

void fruitGrainPart::doDMDemandStress (void)
   //     ===========================================================
{
   //       Simulate crop grain biomass demand stress factor

   cproc_yieldpart_demand_stress1 (min(plant->getNfactPhoto(), plant->getPfactPhoto())
                                   , plant->getSwdefPhoto()
                                   , plant->getTempStressPhoto()
                                   , &gDlt_dm_stress_max);
}


void fruitGrainPart::doNConcGrainLimits (void)
   //============================================================================
{
   //       Calculate the critical N concentration for grain below which plant growth
   //       is affected.  Also minimum and maximum N concentrations below
   //       and above which it is not allowed to fall or rise.
   //       These are analogous to the water concentrations
   //       of sat, dul and ll.

   //+  Local Variables
   float dm_grain;                               // grain mass (g/m2)
   float n_crit_grain;                           // critial mass of grain N (g/m2)
   float n_max_grain;                            // maximum mass of grain N (g/m2)
   float n_min_grain;                            // minimum mass of grain N (g/m2)

   //- Implementation Section ----------------------------------
   if (plant->phenology().inPhase ("grainfill"))
      {
      dm_grain = 0.0;
      vector<plantPart *>::iterator part;
      for (part = myParts.begin(); part != myParts.end(); part++)
         dm_grain += ((*part)->Green.DM() + (*part)->Growth.DM() + (*part)->Retranslocation.DM());

      n_crit_grain = cN_conc_crit_grain * dm_grain;
      n_max_grain = cN_conc_max_grain * dm_grain;
      n_min_grain = cN_conc_min_grain * dm_grain;

      mealPart->doNConcGrainLimits(n_min_grain, n_crit_grain, n_max_grain);
      }
}
void fruitGrainPart::doNRetranslocate( float N_supply, float grain_n_demand)
   //============================================================================
{
   //     Calculate the nitrogen retranslocation from the various plant parts to the grain.

   // available N does not include grain
   // this should not presume grain is 0.

   // get actual grain N uptake by retransolcation
   // limit retranslocation to total available N

   if (grain_n_demand >= N_supply)
      {
      // demand greater than or equal to supply
      // retranslocate all available N

      mealPart->doNRetranslocate(N_supply, grain_n_demand);
      }
   else
      {
      // supply greater than demand.
      // Retranslocate what is needed

      mealPart->doNRetranslocate(grain_n_demand, grain_n_demand);

      }
}

void fruitGrainPart::doNDemand1(float /*dlt_dm*/             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                                , float /*dlt_dm_pot_rue*/)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
}

void fruitGrainPart::doNDemand1Pot(float /*dlt_dm*/             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                                   , float /*dlt_dm_pot_rue*/)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
}

void fruitGrainPart::doNDemand2(float /*dlt_dm*/             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                                , float /*dlt_dm_pot_rue*/)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   mealPart->doNDemand(gN_grain_demand);
}

float fruitGrainPart::dmGrainWetTotal(void)
//=======================================================================================
   {
   return (divide(Total.DM(), (1.0 - cGrn_water_cont), 0.0));
   }

float fruitGrainPart::grainWaterContent(void) {return cGrn_water_cont;}
float fruitGrainPart::dltDmYieldPotential(void) {return 0;}


