#include "StdPlant.h"
#include "PlantStress.h"

using namespace std;


#include "Root/RootPart.h"
#include "Environment.h"

//#####################################################################################
// PStress implementation
//######################################################################################

PStress::PStress(ScienceAPI& scienceAPI, protocol::Component *p)
   : scienceAPI(scienceAPI)
   , parent(p)
{}

// destructor
PStress::~PStress()
{
}

void PStress::init(void)
   {
   zero_p_variables ();
   PlantP_set_phosphorus_aware ();

   parent->addGettableVar("pfact_photo",
               pFact.photo,
               "", "P factor in photosynthesis");

   parent->addGettableVar("pfact_pheno",
               pFact.pheno,
               "", "P factor in phenology");

   parent->addGettableVar("pfact_expansion",
               pFact.expansion,
               "", "P factor in leaf expansion");

   parent->addGettableVar("pfact_grain",
               pFact.grain,
               "", "P factor in grain");

   setupGetFunction(parent, "p_stress_photo", protocol::DTsingle, false,
                    &PStress::get_pstress_photo,
                    "", "P stress in photosynthesis");

   setupGetFunction(parent, "p_stress_pheno", protocol::DTsingle, false,
                    &PStress::get_pstress_pheno,
                    "", "P stress in phenology");

   setupGetFunction(parent, "p_stress_expansion", protocol::DTsingle, false,
                    &PStress::get_pstress_expansion,
                    "", "P stress in leaf expansion");

   setupGetFunction(parent, "p_stress_expan", protocol::DTsingle, false,
                    &PStress::get_pstress_expansion,
                    "", "P stress in leaf expansion");

   setupGetFunction(parent, "p_stress_grain", protocol::DTsingle, false,
                    &PStress::get_pstress_grain,
                    "", "P stress in grain");

   }

bool PStress::isPhosphorusAware(void)
{
   return phosphorus_aware;
};

void PStress::PlantP_set_phosphorus_aware ()
// ====================================================================
//      Check that soil phosphorus is in system
{
//+  Local Variables
      vector<float> values;               // Scratch area
      bool soilpPresent;

      soilpPresent = scienceAPI.getOptional("labile_p", "", values, 0.0, 1000000.0);

      if(soilpPresent == true)
      {
           //module is p aware
         phosphorus_aware = true;
         parent->writeString ("   - Module is set phosphorus aware");
         parent->writeString (" ");
      }
      else
      {
         phosphorus_aware = false;
      }
}

void PStress::zero_p_variables ()
// =======================================
//     Set all variables in this module to zero.
{
      pFact = 1.0;
      c.pFactSlope = 0.0;
 }


//     ===========================================================
void PStress::read_p_constants (void)
{
    scienceAPI.read("pfact_photo_slope", c.pFactSlope.photo, 0.0f, 100.0f);
    scienceAPI.read("pfact_expansion_slope", c.pFactSlope.expansion, 0.0f, 100.0f);
    scienceAPI.read("pfact_pheno_slope", c.pFactSlope.pheno, 0.0f, 100.0f);
    scienceAPI.read("pfact_grain_slope", c.pFactSlope.grain, 0.0f, 100.0f);
}
float PStress::PlantP_Pfact (vector<plantPart *> &allParts)
// ====================================================================
//      Provide value of generic P factor
{
      float    max_p;
      float    min_p;
      float    act_p;
      float    max_p_conc;
      float    min_p_conc;
      float    act_p_conc;
      float    determinants_wt;
      float    pfact;
      vector<plantPart*>::iterator part;

   if (isPhosphorusAware())
   {
      act_p = 0.0;
      min_p = 0.0;
      max_p = 0.0;
      determinants_wt = 0.0;


      for (part = allParts.begin(); part != allParts.end(); part++)
         {
            act_p += (*part)->pGreenStressDeterminant();
            max_p += (*part)->pMaxPotStressDeterminant();
            min_p += (*part)->pMinPotStressDeterminant();
            determinants_wt += (*part)->dmGreenStressDeterminant();

         }

      act_p_conc = (float)divide(act_p, determinants_wt, 0.0);
      max_p_conc = (float)divide(max_p, determinants_wt, 0.0);
      min_p_conc = (float)divide(min_p, determinants_wt, 0.0);

      if ((determinants_wt <= 0.0) || (act_p <= 0.0))
      {
         // appears that things are not yet initialised
         pfact = 1.0;
      }
      else
      {
         pfact = (float)divide(act_p_conc - min_p_conc
                       , max_p_conc - min_p_conc
                       , 1.0);
      }

      pfact = bound(pfact, 0.0, 1.0);
   }
   else
   {
      pfact = 1.0;
   }

   return pfact;
}

void PStress::doPlantPStress (vector<plantPart *> &allParts)
// ====================================================================
//      Provide value of  P stress factors
{
      float    pfact = PlantP_Pfact(allParts);

      pFact.photo = pfact * c.pFactSlope.photo;
      pFact.photo = bound(pFact.photo, 0.0, 1.0);

      pFact.expansion = pfact * c.pFactSlope.expansion;
      pFact.expansion = bound(pFact.expansion, 0.0, 1.0);

      pFact.pheno = pfact * c.pFactSlope.pheno;
      pFact.pheno = bound(pFact.pheno, 0.0, 1.0);

      pFact.grain = pfact * c.pFactSlope.grain;
      pFact.grain = bound(pFact.grain, 0.0, 1.0);

}

void PStress::get_pfact_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, pFact.grain);  //()
}

void PStress::get_pstress_photo(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_photo;
    if (pFact.photo > 0.0)
       pstress_photo = 1.0f - pFact.photo;
    else
       pstress_photo = 0.0;
    systemInterface->sendVariable(qd, pstress_photo);  //()
}

void PStress::get_pstress_pheno(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_pheno;
    if (pFact.pheno > 0.0)
       pstress_pheno = 1.0f - pFact.pheno;
    else
       pstress_pheno = 0.0;
    systemInterface->sendVariable(qd, pstress_pheno);  //()
}

void PStress::get_pstress_expansion(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_expansion;
    if (pFact.expansion > 0.0)
       pstress_expansion = 1.0f - pFact.expansion;
    else
       pstress_expansion = 0.0;
    systemInterface->sendVariable(qd, pstress_expansion);  //()
}

void PStress::get_pstress_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_grain;
    if (pFact.grain > 0.0)
       pstress_grain = 1.0f - pFact.grain;
    else
       pstress_grain = 0.0;
    systemInterface->sendVariable(qd, pstress_grain);  //()
}

//#####################################################################################
// NStress implementation
//######################################################################################

NStress::NStress(ScienceAPI& scienceAPI, protocol::Component *p)
   : scienceAPI(scienceAPI)
   , parent(p)
{
}

// destructor
NStress::~NStress()
{
}

void NStress::init(void)
   {
      nFact = 1.0;
      c.nFact = 0.0;
   parent->addGettableVar("nfact_photo",
               nFact.photo, "", "N factor for photosynthesis");

   parent->addGettableVar("nfact_pheno",
               nFact.pheno, "", "N factor for phenology");

   parent->addGettableVar("nfact_expan",
               nFact.expansion, "", "N factor for leaf expansion");

   parent->addGettableVar("nfact_grain",
               nFact.grain, "", "N factor for ??");

   setupGetFunction(parent, "n_stress_photo", protocol::DTsingle, false,
                    &NStress::get_nstress_photo,
                    "","N stress for photosyntesis");

   setupGetFunction(parent, "n_stress_pheno", protocol::DTsingle, false,
                    &NStress::get_nstress_pheno,
                    "","N stress for phenology");

   setupGetFunction(parent, "n_stress_expan", protocol::DTsingle, false,
                    &NStress::get_nstress_expan,
                    "","N stress for leaf expansion");

   setupGetFunction(parent, "n_stress_grain", protocol::DTsingle, false,
                    &NStress::get_nstress_grain,
                    "","N stress for grain filling");


   }

//     ===========================================================
void NStress::read_n_constants (void)
{
    scienceAPI.read("n_stress_option", c.n_stress_option, 1, 2);
    scienceAPI.read("n_fact_photo", c.nFact.photo, 0.0f, 100.0f);
    scienceAPI.read("n_fact_pheno", c.nFact.pheno, 0.0f, 100.0f);
    scienceAPI.read("n_fact_expansion", c.nFact.expansion, 0.0f, 100.0f);
    //c.nFact.grain = 1.0;
    scienceAPI.read("n_fact_grain", c.nFact.grain, 0.0f, 100.0f);
}

void NStress::doPlantNStress(string module, plantPart* leafPart, plantPart* stemPart)
//=======================================================================================
// Calculate Plant Nitrogen Stress Factors
{
    if (c.n_stress_option == 1)
    {        
        vector<plantPart*> parts;

        // Expansion uses leaves only
        parts.push_back(leafPart);
        nFact.expansion = critNFactor(parts, c.nFact.expansion);

        // Rest have leaf & stem
        parts.push_back(stemPart);
        nFact.photo = critNFactor(parts, c.nFact.photo);
        nFact.grain = critNFactor(parts, c.nFact.grain);

        if (module == "wheat")
            nFact.pheno = 1; // No effect of N on phenology (following APSIM-wheat document, 2020/09/02).
        else
            nFact.pheno = critNFactor(parts, c.nFact.pheno);
    }
    else if (c.n_stress_option == 2)
    {
        vector< plantPart*> parts;

        // Expansion & photosynthesis from leaves only
        parts.push_back(leafPart);
        nFact.expansion = critNFactor(parts, c.nFact.expansion);
        nFact.photo = critNFactor(parts, c.nFact.photo);

        // leaf & stem
        parts.push_back(stemPart);
        nFact.grain = critNFactor(parts, c.nFact.grain);

        if (module == "wheat")
            nFact.pheno = 1; // No effect of N on phenology (following APSIM-wheat document, 2020/09/02).
        else
            nFact.pheno = critNFactor(parts, c.nFact.pheno);
    }
    else
    {
        throw std::invalid_argument("invalid template option in doPlantNStress");
    }
    Debug("NStress.Photo=%f", nFact.photo);
    Debug("NStress.Pheno=%f", nFact.pheno);
    Debug("NStress.Grain=%f", nFact.grain);
    Debug("NStress.Expansion=%f", nFact.expansion);
}


float NStress::critNFactor(vector< plantPart *> &parts, float multiplier)
//=======================================================================================
//   Calculate Nitrogen stress factor from a bunch of parts
/*  Purpose
*   The concentration of Nitrogen in plant parts is used to derive a Nitrogen stress index
*   for many processes. This stress index is calculated from today's relative nutitional
*   status between a critical and minimum Nitrogen concentration.
*/
   {
   vector< plantPart *>::iterator part;

   float dm = 0.0, N = 0.0;
   for (part = parts.begin(); part != parts.end(); part++)
      {
      dm += (*part)->Green.DM();
      N += (*part)->Green.N();
      }

   if (dm > 0.0)
      {
      float N_conc = (float)divide (N, dm, 0.0);

      // calculate critical N concentrations
      float N_crit = 0.0;
      for (part = parts.begin(); part != parts.end(); part++)
          N_crit += (*part)->nCrit();

      float N_conc_crit = (float)divide (N_crit, dm, 0.0);

      // calculate minimum N concentrations
      float N_min = 0.0;
      for (part = parts.begin(); part != parts.end(); part++)
         N_min += (*part)->nMin();

      float N_conc_min = (float)divide (N_min, dm, 0.0);

      //calculate shortfall in N concentrations
      float dividend =  N_conc - N_conc_min;
      float divisor =   N_conc_crit - N_conc_min;
      float result = (float)(multiplier * divide (dividend, divisor, 1.0));
      result = bound(result, 0.0, 1.0);
      return (result);
      }
   else
      return (1.0);
   }

void NStress::get_nstress_pheno(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_pheno;
    if (nFact.pheno > 0.0)
       nstress_pheno = 1.0f - nFact.pheno;
    else
       nstress_pheno = 0.0;
    systemInterface->sendVariable(qd, nstress_pheno);  //()
}

void NStress::get_nstress_photo(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_photo;
    if (nFact.photo > 0.0)
       nstress_photo = 1.0f - nFact.photo;
    else
       nstress_photo = 0.0;
    systemInterface->sendVariable(qd, nstress_photo);  //()
}

void NStress::get_nstress_expan(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_expan;
    if (nFact.expansion > 0.0)
       nstress_expan = 1.0f - nFact.expansion;
    else
       nstress_expan = 0.0;
    systemInterface->sendVariable(qd, nstress_expan);  //()
}

void NStress::get_nstress_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_grain;
    if (nFact.grain > 0.0)
       nstress_grain = 1.0f - nFact.grain;
    else
       nstress_grain = 0.0;
    systemInterface->sendVariable(qd, nstress_grain);  //()
}

//#####################################################################################
// TempStress implementation
//######################################################################################

TempStress::TempStress(ScienceAPI& scienceAPI, protocol::Component *p)
   : scienceAPI(scienceAPI)
   , parent(p)
{
}

// destructor
TempStress::~TempStress()
{
}

void TempStress::init(void)
   {
   tFact = 1.0;
   parent->addGettableVar("temp_stress_photo",
               tFact.photo, "", "Temperature Stress in photosynthesis");

//   parent->addGettableVar("temp_fact_photo",
//               tFact.photo, "", "Temperature Stress in photosynthesis");
//
//   setupGetFunction(parent, "temp_stress_photo", protocol::DTsingle, false,
//                    &TempStress::get_tstress_photo,
//                    "","Temperature stress for photosynthesis");

   }

//     ===========================================================
void TempStress::read_t_constants (void)
{
    cTStressPhoto.read(scienceAPI, "x_ave_temp", "()", 0.0f, 100.0f,
                                   "y_stress_photo", "()", 0.0f, 1.0f);
}

void TempStress::doPlantTempStress (Environment& Environment)
//     ===========================================================
//         Get current temperature stress factors (0-1)
   {
   tFact.photo = cTStressPhoto.value (Environment.meant());
   tFact.photo = bound (tFact.photo, 0.0, 1.0);
   }



void TempStress::get_tstress_photo(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float tstress_photo;
    if (tFact.photo > 0.0)
       tstress_photo = 1.0f - tFact.photo;
    else
       tstress_photo = 0.0;
    systemInterface->sendVariable(qd, tstress_photo);  //()
}


//#####################################################################################
// SWStress implementation
//######################################################################################

SWStress::SWStress(ScienceAPI& scienceAPI, protocol::Component *p)
   : scienceAPI(scienceAPI)
   , parent(p)
{
}

// destructor
SWStress::~SWStress()
{
}

void SWStress::init(RootBase *root)
   {
   rootPart = root;
   swDef = 1.0;


   parent->addGettableVar("swdef_pheno",
               swDef.pheno, "", "Soil water deficit in phenological development");

   parent->addGettableVar("swdef_photo",
               swDef.photo, "", "Soil water deficit in photosynthesis");

   parent->addGettableVar("swdef_expan",
               swDef.expansion, "", "Soil water deficit in leaf expansion");

   parent->addGettableVar("swdef_fixation",
               swDef.fixation, "", "Soil water deficit in N fixation");

   parent->addGettableVar("oxdef_photo",
               swDef.oxdef_photo, "", "Oxygen deficit in photosynthesis");

   setupGetFunction(parent, "sw_stress_pheno", protocol::DTsingle, false,
                    &SWStress::get_swstress_pheno,
                          "","Soil water stress for phenological development");

   setupGetFunction(parent, "sw_stress_photo", protocol::DTsingle, false,
                    &SWStress::get_swstress_photo,
                    "","Soil water stress for photosynthesis");

   setupGetFunction(parent, "sw_stress_expan", protocol::DTsingle, false,
                    &SWStress::get_swstress_expan,
                    "","Soil water stress for leaf expansion");

   setupGetFunction(parent, "sw_stress_fixation", protocol::DTsingle, false,
                    &SWStress::get_swstress_fixation,
                    "","Soil water stress for N fixation");

   }

//     ===========================================================
void SWStress::read_sw_constants (void)
{
   cSwDefPheno.read(scienceAPI, "x_sw_avail_ratio", "()", 0.0f, 100.0f,
                                "y_swdef_pheno", "()", 0.0f, 100.0f);

   cSwwDefPhenoFlower.read(scienceAPI, "x_sw_avail_ratio_flowering", "()", 0.0f, 1.0f,
                                       "y_swdef_pheno_flowering", "()", 0.0f, 5.0f);

   cSwwDefPhenoGrainfill.read(scienceAPI, "x_sw_avail_ratio_start_grain_fill", "()", 0.0f, 1.0f,
                                          "y_swdef_pheno_start_grain_fill", "()", 0.0f, 5.0f);

   cSwwDefExpansion.read(scienceAPI, "x_sw_demand_ratio", "()", 0.0f, 100.0f,
                                     "y_swdef_leaf", "()", 0.0f, 100.0f);

   cSwwDefFix.read(scienceAPI, "x_sw_avail_fix", "()", 0.0f, 100.0f,
                               "y_swdef_fix", "()", 0.0f, 100.0f);

   cOxDefPhoto.read(scienceAPI, "oxdef_photo_rtfr", "()", 0.0f, 1.0f,
                                "oxdef_photo", "()", 0.0f, 1.0f);
}

void SWStress::doPlantWaterStress (float sw_demand)
//     ===========================================================
//         Get current water stress factors (0-1)
   {
   swDef.photo = SWDefPhoto(sw_demand);
   swDef.pheno = SWDefPheno(cSwDefPheno);
   swDef.pheno_flower = SWDefPheno(cSwwDefPhenoFlower);
   swDef.pheno_grainfill = SWDefPheno(cSwwDefPhenoGrainfill);
   swDef.expansion = SWDefExpansion(sw_demand);
   swDef.fixation = SWDefFixation();
   swDef.oxdef_photo = SWDefOxygen(rootPart->wet_root_fr());

   Debug("SWStress.Photo=%f", swDef.photo);
   Debug("SWStress.Pheno=%f", swDef.pheno);
   Debug("SWStress.PhenoFlower=%f", swDef.pheno_flower);
   Debug("SWStress.PhenoGrainFilling=%f", swDef.pheno_grainfill);
   Debug("SWStress.Expansion=%f", swDef.expansion);
   Debug("SWStress.Fixation=%f", swDef.fixation);
   Debug("SWStress.OxygenDeficitPhoto=%f", swDef.oxdef_photo);
}

float SWStress::SWDefExpansion(float sw_demand)
//==========================================================================
// Get the soil water to demand ratio and calculate the 0-1 stress factor for leaf expansion.
// 1 is no stress, 0 is full stress.
   {
   if (sw_demand > 0.0)
      {
      // get potential water that can be taken up when profile is full
      float sw_demand_ratio = (float)divide (rootPart->swSupply(), sw_demand, 10.0);
      return cSwwDefExpansion.value(sw_demand_ratio);
      }
   else
      return 1.0;
   }

float SWStress::SWDefPhoto(float sw_demand)
//========================================================================
// Calculate the soil water supply to demand ratio and therefore the 0-1 stress factor
//       for photosysnthesis. 1 is no stress, 0 is full stress.
   {
   if (sw_demand > 0.0)
      {
      //get potential water that can be taken up when profile is full
      float sw_demand_ratio = (float)divide (rootPart->waterUptake(), sw_demand, 1.0);
      return bound (sw_demand_ratio , 0.0, 1.0);
      }
   else
      return 1.0;
   }


float SWStress::SWDefPheno(interpolationFunction& cSwDefPheno)
//=========================================================================
// Get the soil water availability ratio in the root zone
// and calculate the 0 - 1 stress factor for phenology.
// 1 is no stress, 0 is full stress.
   {
   if (rootPart->swAvailablePotential() > 0.0)
      {
      float sw_avail_ratio = (float)divide (rootPart->swAvailable(), rootPart->swAvailablePotential(), 1.0);
      sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0);
      return cSwDefPheno.value(sw_avail_ratio);
      }
   else
      return 1.0;
   }

float SWStress::SWDefFixation(void)
//==========================================================================
// Get the soil water availability ratio in the root zone and
// calculate the 0 - 1 stress factor for fixation.
// 1 is no stress, 0 is full stress.
   {
   if (rootPart->swAvailablePotential() > 0.0)
      {
      float sw_avail_ratio = (float)divide(rootPart->swAvailable(), rootPart->swAvailablePotential(), 1.0);
      sw_avail_ratio = bound(sw_avail_ratio , 0.0, 1.0);
      return cSwwDefFix.value(sw_avail_ratio);
      }
   else
      return 1.0;
   }

float SWStress::SWDefOxygen (float wet_root_fr)
//=======================================================================================
// Calculate today's oxygen deficit (i.e. water logging) stress factor
   {
   if (wet_root_fr > 0.0)
      return cOxDefPhoto.value(wet_root_fr);
   else
      return 1.0;
}

void SWStress::get_swstress_pheno(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float swstress_pheno;
    if (swDef.pheno > 0.0)
       swstress_pheno = 1.0f - swDef.pheno;
    else
       swstress_pheno = 0.0;
    systemInterface->sendVariable(qd, swstress_pheno);  //()
}

void SWStress::get_swstress_photo(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float swstress_photo;
    if (swDef.photo > 0.0)
       swstress_photo = 1.0f - swDef.photo;
    else
       swstress_photo = 0.0;
    systemInterface->sendVariable(qd, swstress_photo);  //()
}

void SWStress::get_swstress_expan(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float swstress_expan;
    if (swDef.expansion > 0.0)
       swstress_expan = 1.0f - swDef.expansion;
    else
       swstress_expan = 0.0;
    systemInterface->sendVariable(qd, swstress_expan);  //()
}

void SWStress::get_swstress_fixation(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float swstress_fixation;
    if (swDef.fixation > 0.0)
       swstress_fixation = 1.0f - swDef.fixation;
    else
       swstress_fixation = 0.0;
    systemInterface->sendVariable(qd, swstress_fixation);  //()
}



