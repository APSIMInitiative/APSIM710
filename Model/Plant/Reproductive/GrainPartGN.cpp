#include "StdPlant.h"

#include "GrainPartGN.h"
#include "../Phenology/Phenology.h"
#include "../Population.h"
#include "../Stem.h"
#include "../Root/RootBase.h"
using namespace std;


inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

//  initialise data members.
fruitGrainPartGN::fruitGrainPartGN(XMLNode parameters, ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : fruitGrainPart(parameters, scienceAPI, p, name)
{
}

// destructor
fruitGrainPartGN::~fruitGrainPartGN()
{
}

ostream &operator<<(ostream &output, const fruitGrainPartGN /*&pool*/)
{
   //   output << "fruitGrainPartGN:" << endl;
   //   output << "   Green meal:    " << pool.green.meal << endl;
   //   output << "   Senesced meal: " << pool.senesced.meal << endl;
   //   output << "   Dead meal:     " << pool.dead.meal << endl << endl;
   output << "\n";
   return output;
}

// Assigment operator
//  assign data members of object

const fruitGrainPartGN &fruitGrainPartGN::operator=(const fruitGrainPartGN &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for fruitGrainPartGN");
}

void fruitGrainPartGN::onInit1(protocol::Component *system)
   //===========================================================================
{
   fruitGrainPart::onInit1(system);

   system->addGettableVar("grain_no",gGrain_no, "/m^2", "Grain number");
   setupGetFunction(system, "grain_size", protocol::DTsingle, false, &fruitGrainPartGN::get_grain_size, "g", "Size of each grain");
   
   //scienceAPI.expose("GrainKillFraction", "", "Fraction of grains killed", GrainKillFraction);
   scienceAPI.exposeWritable("GrainKillFraction", "", "GrainKillFraction", FloatSetter(&fruitGrainPartGN::onSetGrainKillFraction));
}


   void fruitGrainPartGN::onSetGrainKillFraction(float Factor)
   {
   	GrainKillFraction = Factor;
   }

float fruitGrainPartGN::grainWt(void)
   //===========================================================================
{
   return (float)divide (Total.DM(), gGrain_no, 0.0);
}

void fruitGrainPartGN::get_grain_size(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, grainWt()); //////FIXME!!!!!!!!!
}

void fruitGrainPartGN::doGrainNumber (void)
   //===========================================================================
   //       Calculate Grain Number
{
      gGrain_no = grainNumber (plant->stem().Green.DM(),plant->fruit().Green.DM()
                              , pGrains_per_gram_stem);
      Debug("Grian.GrainNumber=%f", gGrain_no);
}

float fruitGrainPartGN::grainNumber (float stem_dm_green, float ear_dm_green
                                      , float p_grains_per_gram_stem)    // OUTPUT
   //===========================================================================
   //       Perform grain number calculations
{
   float grain_no;
   if (plant->phenology().onDayOf("emergence"))
      {
      // seedling has just emerged.
      grain_no = 0.0;
      }
   else if (plant->phenology().onDayOf("flowering"))
      {
      // we are at first day of grainfill.
	  if(pGrain_No_Determinant=="stem")
         grain_no = p_grains_per_gram_stem * stem_dm_green;
      else if (pGrain_No_Determinant=="ear")
	     grain_no = p_grains_per_gram_stem * ear_dm_green;
	  else
	     throw std::runtime_error ("Unknown Grain Number Determinant Specified");
      }
   else
      {
      grain_no = gGrain_no;   // no changes
      }
   return grain_no;
}

void fruitGrainPartGN::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
   {
   fruitGrainPart::readCultivarParameters (system, cultivar);
   scienceAPI.read("grains_per_gram_stem", pGrains_per_gram_stem, 0.0f, 10000.0f);
   scienceAPI.read("potential_grain_filling_rate", pPotential_grain_filling_rate, 0.0f, 1.0f);
   scienceAPI.read("potential_grain_growth_rate", pPotential_grain_growth_rate, 0.0f, 1.0f);
   scienceAPI.read("max_grain_size", pMaxGrainSize, 0.0f, 1.0f);
   scienceAPI.read("grain_no_determinant",pGrain_No_Determinant);
   }

void fruitGrainPartGN::writeCultivarInfo (protocol::Component *system)
   //===========================================================================
{

   // report
     ostringstream msg;
     msg.flags ( ios::right | ios::fixed);
     msg.precision(1);
     msg << "   grains_per_gram_stem           = " << setw(10) << pGrains_per_gram_stem << " (/g)" << "\n";
     msg.precision(4);
     msg << "   potential_grain_filling_rate   = " << setw(10) << pPotential_grain_filling_rate << " (g/grain/day)" << "\n";
     msg << "   potential_grain_growth_rate    = " << setw(10) << pPotential_grain_growth_rate << " (g/grain/day)" << "\n";
     msg << "   max_grain_size                 = " << setw(10) << pMaxGrainSize << " (g)" << ends;
     system->writeString (msg.str().c_str());

}

void fruitGrainPartGN::zeroAllGlobals(void)
{
   fruitGrainPart::zeroAllGlobals();

   cPotential_grain_n_filling_rate  = 0.0;
   cMinimum_grain_n_filling_rate  = 0.0;
   cCrit_grainfill_rate  = 0.0;
   cGrainMaxDailyNConc = 0.0;
   pGrains_per_gram_stem = 0.0;
   pPotential_grain_growth_rate = 0.0;
   pMaxGrainSize = 0.0;
   gGrain_no = 0.0;
   GrainKillFraction = 0.0;

}

void fruitGrainPartGN::onKillStem(void)
   // ====================================================================
{
   fruitGrainPart::onKillStem();
   gGrain_no = 0.0;
}

void fruitGrainPartGN::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
   {
   fruitGrainPart::readSpeciesParameters(system, sections);

   rel_grainfill.read(scienceAPI, "x_temp_grainfill", "oC", 0.0f, 80.0f
                                   , "y_rel_grainfill", "-", 0.0f, 1.0f);
   rel_grain_n_fill.read(scienceAPI, "x_temp_grain_n_fill", "oC", 0.0f, 40.0f
                                   , "y_rel_grain_n_fill", "-", 0.0f, 1.0f);
   scienceAPI.read("potential_grain_n_filling_rate", cPotential_grain_n_filling_rate, 0.0f, 1.0f);
   scienceAPI.read("minimum_grain_n_filling_rate", cMinimum_grain_n_filling_rate, 0.0f, 1.0f);
   scienceAPI.read("crit_grainfill_rate", cCrit_grainfill_rate, 0.0f, 1.0f);
   scienceAPI.read("GrainMaxDailyNConc", cGrainMaxDailyNConc, 0.0f, 1.0f);
   }

void fruitGrainPartGN::update(void)
   //===========================================================================
{
   fruitGrainPart::update();
   // transfer plant grain no.
   float dlt_grain_no_lost  = gGrain_no * plant->population().DyingFractionPlants();
   gGrain_no -= dlt_grain_no_lost;
   
   gGrain_no = (float)(gGrain_no * (1.0 - GrainKillFraction));
   GrainKillFraction = 0.0;
   Debug("meal.GrainNo=%f", gGrain_no);
   
}

//void fruitGrainPartGN::display(ostream &os)
//{
//   //   os << "fruitGrainPartGN:" << endl;
//   //   os << "Green meal: " << green.meal << endl;
//   //   os << "Senesced meal: " << senesced.meal << endl;
//   //   os << "Dead meal: " << dead.meal << endl << endl;
//   os << endl;
//}
//

void fruitGrainPartGN::doProcessBioDemand(void)
   //===========================================================================
{

   doDMDemandStress ();
   doGrainNumber();
   doDMDemandGrain ();
}

float fruitGrainPartGN::grainNo(void)  {return gGrain_no;}

void fruitGrainPartGN::doDMDemandGrain(void)
   //===========================================================================
   {
   if (plant->phenology().inPhase("postflowering"))
      {
      //       Perform grain filling calculations
      float tav;
      tav = meanT();

      if (plant->phenology().inPhase("grainfill"))
         gDlt_dm_grain_demand = gGrain_no
                              * pPotential_grain_filling_rate
                              * rel_grainfill.value(tav);
      else
         {
         // we are in the flowering to grainfill phase
         gDlt_dm_grain_demand = gGrain_no
                              * pPotential_grain_growth_rate
                              * rel_grainfill.value(tav);
          }
       // check that grain growth will not result in daily n conc below minimum conc
       // for daily grain growth
      float nfact_grain_conc = plant->getNfactGrainConc();
      float nfact_grain_fill = (float)min(1.0, nfact_grain_conc*cPotential_grain_n_filling_rate/cMinimum_grain_n_filling_rate);
      gDlt_dm_grain_demand = gDlt_dm_grain_demand * nfact_grain_fill;



      // Check that growth does not exceed maximum grain size
      float max_grain = gGrain_no * pMaxGrainSize;
      float max_dlt = (float)max (max_grain - mealPart->Green.DM(), 0.0);
      gDlt_dm_grain_demand = (float)min (gDlt_dm_grain_demand, max_dlt);

      mealPart->doDMDemandGrain(gDlt_dm_grain_demand);
      }
   else
      gDlt_dm_grain_demand = 0.0;

   Debug("Grain.Dlt_dm_grain_demand=%f", gDlt_dm_grain_demand);
   }

void fruitGrainPartGN::doNDemandGrain (float nfact_grain_conc      //   (INPUT)
                                     , float /* swdef_expansion*/)    //   grain N demand (g/m^2)
   //===========================================================================
{
   float Tav ;
   float grain_growth;

   // default case
   float gN_grain_demand1 = 0.0;
   float gN_grain_demand2 = 0.0;
   gN_grain_demand = 0.0;
   if (plant->phenology().inPhase("reproductive"))
      {

      // we are in grain filling stage
      Tav = meanT();

      gN_grain_demand1 = gGrain_no
                     * cPotential_grain_n_filling_rate * nfact_grain_conc
                     * rel_grain_n_fill.value(Tav);

      gN_grain_demand2 = min(gGrain_no * cPotential_grain_n_filling_rate * rel_grain_n_fill.value(Tav)
                             ,plant->root().NSupply());
      gN_grain_demand = max(gN_grain_demand1,gN_grain_demand2);
      gN_grain_demand = gN_grain_demand1;

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
      if (dailyNconc > cGrainMaxDailyNConc) gN_grain_demand = (mealPart->Growth.DM() + mealPart->Retranslocation.DM())*cGrainMaxDailyNConc;
      }

    Debug("Grain.N_grain_demand=%f", gN_grain_demand);
}



