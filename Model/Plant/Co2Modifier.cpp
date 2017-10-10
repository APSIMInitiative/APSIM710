#include "StdPlant.h"
#include "Co2Modifier.h"

#include "Environment.h"
using namespace std;

//#####################################################################################
// Co2Modifier implementation
//######################################################################################

Co2Modifier::Co2Modifier(ScienceAPI& scienceAPI, plantInterface& p)
   : scienceAPI(scienceAPI), plant(p)
   {
   }

// destructor
Co2Modifier::~Co2Modifier()
{
}

void Co2Modifier::init(void)
   {
   zero_co2_variables();
   scienceAPI.expose("temp_stress_photo_co2", "", "Temperature Stress in photosynthesis via co2Modifier", tFact.photo);

   c.photosynthetic_pathway = photosynthetic_pathway_UNDEF;
   }

void Co2Modifier::zero_co2_variables (void)
// =======================================
//     Set all variables in this module to some initial value.
{
      co2_modifier_te = 1.0;
      co2_modifier_n_conc = 1.0;
      co2_modifier_rue = 1.0;
      co2_default = 350.0;
 }

//     ===========================================================
void Co2Modifier::read_co2_constants (void)
{
    cTE.readOptional(scienceAPI, "x_co2_te_modifier", "()", 0.0, 1000.0,
                         "y_co2_te_modifier", "()", 0.0, 10.0);

    cNConc.readOptional(scienceAPI, "x_co2_nconc_modifier", "()", 0.0, 1000.0,
                            "y_co2_nconc_modifier", "()", 0.0, 10.0);

    string pathway;
    scienceAPI.readOptional("photosynthetic_pathway", pathway);

    if (Str_i_Eq(pathway.c_str(), "C3")) {
      c.photosynthetic_pathway = photosynthetic_pathway_C3;

    } else if(Str_i_Eq(pathway.c_str(), "C4")) {
      c.photosynthetic_pathway = photosynthetic_pathway_C4;

    } else {
      c.photosynthetic_pathway = photosynthetic_pathway_UNDEF;
	  string msg = "Undefined photosynthetic_pathway \"";
	  msg += pathway;
	  msg += "\"\nShould be \"C3\" or \"C4\".";
      scienceAPI.warning(msg);
    }
}

float Co2Modifier::rue (void) const
   {
      return co2_modifier_rue;
   }

float Co2Modifier::te (void) const
   {
      return co2_modifier_te;
   }

float Co2Modifier::n_conc (void)
   {
      return co2_modifier_n_conc;
   }

void Co2Modifier::doPlant_Co2Modifier()
//     ===========================================================
//         Get current temperature stress factors (0-1)
   {
         float co2 = plant.environment().co2();
		 if (fabs(co2 - co2_default) > 0.1 && 
		     (! cTE.isInitialised() || ! cNConc.isInitialised()))
            throw std::runtime_error("CO2 isn't at default level, and plant has no CO2 x (TE, nConc) parameterisations.");

         co2_modifier_rue = plant_rue_co2_modifier(co2, plant.environment().meant());

		 if ( cTE.isInitialised() )
           co2_modifier_te = cTE.value(co2);
		 
		 if ( cNConc.isInitialised() )
           co2_modifier_n_conc = cNConc.value(co2);
   }

//==========================================================================
float Co2Modifier::plant_rue_co2_modifier(float co2,                 //!CO2 level (ppm)
                                   float temp)                //!daily average temp (C)
//==========================================================================
/*  Purpose
*     Calculation of the CO2 modification on rue
*
*     References
*     Reyenga, Howden, Meinke, Mckeon (1999), Modelling global change impact on wheat cropping in
*              south-east Queensland, Australia. Enivironmental Modelling & Software 14:297-306
*
*
*  Purpose
*     Calculation of the CO2 modification on rue
*
*  Changes
*     20000717   ew programmed
*/
   {
   //  Local Variables
      float TT;    //co2 compensation point (ppm)
      float first;            // Temp vars for passing composite arg to a func
      float second;           // expecting a pointer

   // Implementation Section ----------------------------------
   if (c.photosynthetic_pathway == photosynthetic_pathway_C3)
      {
      TT  = (float)divide(163.0 - temp, 5.0 - 0.1 * temp, 0.0);

      first = (float)((co2 - TT) * (350.0 + 2.0 * TT));
      second = (float)((co2 + 2.0 * TT)*(350.0 - TT));
      return (float)divide( first, second, 1.0);
      }
    else if (c.photosynthetic_pathway == photosynthetic_pathway_C4)
      {
      return (float)(0.000143 * co2 + 0.95); //Mark Howden, personal communication
      }

    return 1.0;
  }






