#include "StdPlant.h"

#include "FruitCohortFN.h"
#include "GrainPartFN.h"
#include "../Environment.h"
#include "../Stem.h"
#include "../Population.h"
using namespace std;

inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

//  initialise data members.
fruitGrainPartFN::fruitGrainPartFN(XMLNode parameters, ScienceAPI& scienceAPI, plantInterface *p, FruitCohortFN *g, const string &name)
   : fruitGrainPart(parameters, scienceAPI, p, name)
   , myParent(g)
{
}

// destructor
fruitGrainPartFN::~fruitGrainPartFN()
{
}

ostream &operator<<(ostream &output, const fruitGrainPartFN /*&pool*/)
{
   //   output << "fruitGrainPartFN:" << endl;
   //   output << "   Green meal:    " << pool.green.meal << endl;
   //   output << "   Senesced meal: " << pool.senesced.meal << endl;
   //   output << "   Dead meal:     " << pool.dead.meal << endl << endl;
   output << endl;
   return output;
}

// Assigment operator
//  assign data members of object

const fruitGrainPartFN &fruitGrainPartFN::operator=(const fruitGrainPartFN &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for fruitGrainPartFN");
}

void fruitGrainPartFN::onInit1(protocol::Component *system)
   //===========================================================================
{
   fruitGrainPart::onInit1(system);

   system->addGettableVar("GrainNumber",gGrain_no, "/m^2", "Grain number");
   setupGetFunction(system, "grain_size", protocol::DTsingle, false, &fruitGrainPartFN::get_grain_size, "g", "Size of each grain");
}

float fruitGrainPartFN::grainWt(void)
   //===========================================================================
{
   return divide (Total.DM(), gGrain_no, 0.0);
}

void fruitGrainPartFN::get_grain_size(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, grainWt());
}

void fruitGrainPartFN::doGrainNumber (void)
   //===========================================================================
   //       Calculate Grain Number
{
      gGrain_no = grainNumber (plant->stem().Green.DM()
                              , pGrains_per_gram_stem);
}

float fruitGrainPartFN::grainNumber (float stem_dm_green
                                      , float p_grains_per_gram_stem)    // OUTPUT
   //===========================================================================
   //       Perform grain number calculations
{
   float grain_no;
   if (myParent->onDayOf ("emergence"))
      {
      // seedling has just emerged.
      grain_no = 0.0;
      }
   else if (myParent->onDayOf("flowering"))
      {
      // we are at first day of grainfill.
      grain_no = p_grains_per_gram_stem * stem_dm_green;
      }
   else
      {
      grain_no = gGrain_no;   // no changes
      }
   return grain_no;
}

void fruitGrainPartFN::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
   {
   fruitGrainPart::readCultivarParameters (system, cultivar);
   scienceAPI.read("grains_per_gram_stem", pGrains_per_gram_stem, 0.0f, 10000.0f);
   scienceAPI.read("potential_grain_filling_rate", pPotential_grain_filling_rate, 0.0f, 1.0f);
   scienceAPI.read("potential_grain_growth_rate", pPotential_grain_growth_rate, 0.0f, 1.0f);
   scienceAPI.read("max_grain_size", pMaxGrainSize, 0.0f, 1.0f);
   }

void fruitGrainPartFN::writeCultivarInfo (protocol::Component *system)
   //===========================================================================
{

   // report
     ostringstream msg;
     msg.flags ( ios::right | ios::fixed);
     msg.precision(1);
     msg << "   grains_per_gram_stem           = " << setw(10) << pGrains_per_gram_stem << " (/g)" << endl;
     msg.precision(4);
     msg << "   potential_grain_filling_rate   = " << setw(10) << pPotential_grain_filling_rate << " (g/grain/day)" << endl;
     msg << "   potential_grain_growth_rate    = " << setw(10) << pPotential_grain_growth_rate << " (g/grain/day)" << endl;
     msg << "   max_grain_size                 = " << setw(10) << pMaxGrainSize << " (g)" << ends;
     system->writeString (msg.str().c_str());

}

void fruitGrainPartFN::zeroAllGlobals(void)
{
   fruitGrainPart::zeroAllGlobals();

   cPotential_grain_n_filling_rate  = 0.0;
   cMinimum_grain_n_filling_rate  = 0.0;
   cCrit_grainfill_rate  = 0.0;
   pGrains_per_gram_stem = 0.0;
   pPotential_grain_growth_rate = 0.0;
   pMaxGrainSize = 0.0;
   gGrain_no = 0.0;

}

void fruitGrainPartFN::onKillStem(void)
   // ====================================================================
{
   fruitGrainPart::onKillStem();
   gGrain_no = 0.0;
}

void fruitGrainPartFN::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
   {
   fruitGrainPart::readSpeciesParameters(system, sections);

   rel_grainfill.read(scienceAPI, "x_temp_grainfill", "oC", 0.0, 80.0
                                   , "y_rel_grainfill", "-", 0.0, 1.0);
   rel_grain_n_fill.read(scienceAPI, "x_temp_grain_n_fill", "oC", 0.0, 40.0
                                   , "y_rel_grain_n_fill", "-", 0.0, 1.0);
   scienceAPI.read("potential_grain_n_filling_rate", cPotential_grain_n_filling_rate, 0.0f, 1.0f);
   scienceAPI.read("minimum_grain_n_filling_rate", cMinimum_grain_n_filling_rate, 0.0f, 1.0f);
   scienceAPI.read("crit_grainfill_rate", cCrit_grainfill_rate, 0.0f, 1.0f);
   }

void fruitGrainPartFN::update(void)
   //===========================================================================
{
   fruitGrainPart::update();
   // transfer plant grain no.
   float dlt_grain_no_lost  = gGrain_no * plant->population().DyingFractionPlants();
   gGrain_no -= dlt_grain_no_lost;

}

//void fruitGrainPartFN::display(ostream &os)
//{
//   //   os << "fruitGrainPartFN:" << endl;
//   //   os << "Green meal: " << green.meal << endl;
//   //   os << "Senesced meal: " << senesced.meal << endl;
//   //   os << "Dead meal: " << dead.meal << endl << endl;
//   os << endl;
//}
//

void fruitGrainPartFN::doProcessBioDemand(void)
   //===========================================================================
{

   doDMDemandStress ();
   doGrainNumber();
   doDMDemandGrain ();
}

float fruitGrainPartFN::grainNo(void)  {return gGrain_no;}

float fruitGrainPartFN::dltDmYieldPotential(void)
   //===========================================================================
{

   return myParent->potentialCohortGrainFillRate()
                           * rel_grainfill.value(plant->environment().meant())                                            //cohort dm demand - cohort stuff
                           * myParent->getDltTT();
}

void fruitGrainPartFN::doDMDemandGrain(void)
   //===========================================================================
   {
   if (plant->phenology().inPhase("postflowering"))
      {
      //       Perform grain filling calculations
      if (plant->phenology().inPhase("grainfilling"))
          {
          // we are in grain filling stage
          float dlt_dm_yield = dltDmYieldPotential();                        //cohort dm demand - cohort stuff

          dlt_dm_yield = bound (dlt_dm_yield, 0.0, myParent->dltDmGrainMax());

          gDlt_dm_grain_demand  = max(0.0, oilPart->addEnergy(dlt_dm_yield));// adding grain energy to potential new grain wt to get grain demand

          // check that grain growth will not result in daily n conc below minimum conc
          // for daily grain growth
          //      float nfact_grain_conc = plant->getNfactGrainConc();
          //      float nfact_grain_fill = min(1.0, nfact_grain_conc*cPotential_grain_n_filling_rate/cMinimum_grain_n_filling_rate);
          //      gDlt_dm_grain_demand = gDlt_dm_grain_demand * nfact_grain_fill;
          //      gDlt_dm_grain_demand = dlt_dm_grain_demand;

          oilPart->doDMDemandGrain(gDlt_dm_grain_demand);
          mealPart->doDMDemandGrain(max(0.0, gDlt_dm_grain_demand - oilPart->dmGreenDemand()));
          }
     else
          gDlt_dm_grain_demand = 0.0;
    }
}

void fruitGrainPartFN::doNDemandGrain (float nfact_grain_conc      //   (INPUT)
                                     , float /* swdef_expansion*/)    //   grain N demand (g/m^2)
   //===========================================================================
{
   float Tav ;
   float grain_growth;

   // default case
   gN_grain_demand = 0.0;

   if (plant->phenology().inPhase("reproductive"))
      {
      // we are in grain filling stage
      Tav = meanT();

      gN_grain_demand = gGrain_no
                     * cPotential_grain_n_filling_rate * nfact_grain_conc
                     * rel_grain_n_fill.value(Tav);
      }

   if (plant->phenology().inPhase("postflowering"))
      {
      // during grain C filling period so make sure that C filling is still
      // going on otherwise stop putting N in now

      grain_growth = (float)divide((mealPart->Growth.DM() + mealPart->Retranslocation.DM())
                            , gGrain_no
                            , 0.0);
      if (grain_growth < cCrit_grainfill_rate)
         {
         //! grain filling has stopped - stop n flow as well
         gN_grain_demand = 0.0;
         }
      float dailyNconc = (float)divide(gN_grain_demand,(mealPart->Growth.DM() + mealPart->Retranslocation.DM()),1.0);
      if (dailyNconc > 0.03) gN_grain_demand = (mealPart->Growth.DM() + mealPart->Retranslocation.DM())*0.03f;
      }

}



