#include "../StdPlant.h"

#include "Leaf.h"
#include "GenericLeaf.h"
#include "../Environment.h"
#include "../Phenology/Phenology.h"
#include "../Population.h"
using namespace std;

const float  tolerance_lai = 1.0e-4 ;


// Read Constants
void GenericLeaf::readConstants (protocol::Component *system, const string &section)
{
    SimplePart::readConstants(system, section);
   // Nothing to do here..
}

// Read species specific parameters
void GenericLeaf::readSpeciesParameters (protocol::Component *system, vector<string> &search_order)
   {
   SimplePart::readSpeciesParameters(system, search_order);
   scienceAPI.read("leaf_no_at_emerg", cLeafNumberAtEmerg, 0.0f, 100.0f);
   scienceAPI.read("initial_tpla", cInitialTPLA, 0.0f, 100000.0f);
   scienceAPI.read("min_tpla", cMinTPLA, 0.0f, 100000.0f);
   scienceAPI.read("sla_min", cSLAMin, 0.0f, 100000.0f);
   scienceAPI.read("fr_lf_sen_rate", cFrLeafSenRate, 0.0f, 1.0f);
   scienceAPI.read("node_sen_rate", cNodeSenRate, 0.0f, 1000.0f);
   scienceAPI.read("n_fact_lf_sen_rate", cNFactLeafSenRate, 0.0f, 5.0f);
   scienceAPI.read("node_no_correction", cNodeNoCorrection, 0.0f, 10.0f);
   cSLAMax.read(scienceAPI
                   , "x_lai",  "(mm2/mm2)", 0.0, 15.0
                   , "y_sla_max", "(mm2/g)", 0.0, 2.e5);

   cLeafNoFrac.read(scienceAPI
                     ,"x_lai_ratio", "()", 0.0, 1.0
                     ,"y_leaf_no_frac", "()", 0.0, 1.0);

   scienceAPI.read("lai_sen_light", cLAISenLight, 3.0f, 20.0f);
   scienceAPI.read("sen_light_slope", cSenLightSlope, 0.0f, 100.0f);
   scienceAPI.read("sen_rate_water", cSenRateWater, 0.0f, 100.0f);

   cSenescenceFac.read(scienceAPI
                     , "x_temp_senescence", "(oc)", -20.0, 20.0
                     , "y_senescence_fac", "()", 0.0, 1.0);

   cLeafSize.read(scienceAPI
                        , "x_node_no",  "()", 0.0, 100.0
                        , "y_leaf_size", "(mm2)", 0.0, 100000.0);

   cNodeAppRate.read(scienceAPI
                        , "x_node_no_app",  "()", 0.0, 200.0
                        , "y_node_app_rate", "()", 0.0, 400.0);

   bool LAIOptionFound = scienceAPI.readOptional("lai_exp_model_option", cLAIExpModelOption); // lai expansion model option (PFR)

   if (LAIOptionFound)
       cLAIRatePhoto.read(scienceAPI                                                 //    LAI expansion rate
                        , "x_lai_exp_rate_photo",  "()", 5.0, 24.0
                        , "y_lai_exp_rate", "()", 0.0, 0.05);
   else
       cLAIExpModelOption = "default";

   cNodeAppRatePhoto.readOptional(scienceAPI                             // photoperiod response for phyllochron
                        , "x_pp_node_app_rate",  "()", 0.0, 200.0
                        , "y_tt_node_app_rate", "()", 0.0, 400.0);

   scienceAPI.readOptional("node_app_rate_pre_photo", cNodeAppRatePrePhoto, 0.0f, 1000.0f);     // pre-photoperiod phyllochron  (PFR)
   scienceAPI.readOptional("node_app_rate_post_photo", cNodeAppRatePostPhoto, 0.0f, 1000.0f);  // post-photoperiod phyllochron  (PFR)

   cLeavesPerNode.read(scienceAPI
                        , "x_node_no_leaf",  "()", 0.0, 200.0
                        , "y_leaves_per_node", "()", 0.0, 50.0);

   scienceAPI.read("x_row_spacing", cXRowSpacing, cNumRowSpacing, 0.0f, 2000.0f);
   scienceAPI.read("y_extinct_coef", cYExtinctCoef, cNumRowSpacing, 0.0f, 1.0f);
   scienceAPI.read("y_extinct_coef_dead", cYExtinctCoefDead, cNumRowSpacing, 0.0f, 1.0f);

   scienceAPI.read("transp_eff_cf", c.transpEffCf, 0.0f, 1.0f);

   Photosynthesis->Read();

   }

// Connect our bits to the system
void GenericLeaf::onInit1(protocol::Component *system)
{
   SimplePart::onInit1(system);
   Leaf::onInit1(system);

   setupGetFunction(system, "leaf_no", protocol::DTsingle, false,
                    &GenericLeaf::get_leaf_no, "leaves/plant", "Number of leaves per plant");

   system->addGettableVar("node_no", gNodeNo, "nodes/plant", "Number of mainstem nodes per plant");

   setupGetFunction(system, "leaf_no_sen", protocol::DTsingle, false,
                     &GenericLeaf::get_leaf_no_sen, "leaves/m2", "Number of senesced leaves per square meter");

   setupGetFunction(system, "leaf_area", protocol::DTsingle, true,
                    &GenericLeaf::get_leaf_area, "mm^2", "Leaf area for each node");

   setupGetFunction(system, "leaf_area_tot", protocol::DTsingle, false,
                    &GenericLeaf::get_leaf_area_tot, "m^2", "Total plant leaf area");

   setupGetFunction(system, "lai_sum", protocol::DTsingle, false,
                    &GenericLeaf::get_lai_sum, "m^2/m^2", "LAI of all leaf components");

   setupGetFunction(system, "tlai", protocol::DTsingle, false,
                    &GenericLeaf::get_tlai, "m^2/m^2", "Total lai");

   system->addGettableVar("slai", gSLAI, "m^2/m^2", "Senesced lai");

   system->addGettableVar("lai", gLAI, "m^2/m^2", "Leaf area index");

    system->addGettableVar("dlt_lai", dltLAI, "m^2/m^2", "Actual change in live plant lai"); 									// (PFR)

   system->addGettableVar("dlt_lai_pot", dltLAI_pot, "m^2/m^2", "Potential change in live plant lai");

   system->addGettableVar("dlt_lai_stressed", dltLAI_stressed, "m^2/m^2", "Potential change in lai allowing for stress");

   system->addGettableVar("dlt_lai_carbon", dltLAI_carbon, "m^2/m^2", "Potential change in lai allowing for growth"); 			// (PFR)

   system->addGettableVar("dlt_leaf_no", dltLeafNo, "leaves/m2", "Change in number of leaves");

   system->addGettableVar("dlt_node_no", dltNodeNo, "nodes/m2", "Change in number of nodes");

   system->addGettableVar("dlt_leaf_no_pot", dltLeafNoPot, "m^2/m^2", "Potential Leaf no");

   system->addGettableVar("dlt_slai_age", dltSLAI_age, "m^2/m^2", "Change in lai via age");

   system->addGettableVar("dlt_slai_light", dltSLAI_light, "m^2/m^2", "Change in lai via light");

   system->addGettableVar("dlt_slai_water", dltSLAI_water, "m^2/m^2", "Change in lai via water stress");

   system->addGettableVar("dlt_slai_frost", dltSLAI_frost, "m^2/m^2", "Change in lai via low temperature");

   system->addGettableVar("leaves_per_node", gLeavesPerNode, "","");

}

void GenericLeaf::get_tlai(protocol::Component *system, protocol::QueryValueData &qd)
{
    float tlai = gLAI + gSLAI;
    system->sendVariable(qd, tlai);
}

void GenericLeaf::get_lai_sum(protocol::Component *system, protocol::QueryValueData &qd)
{
    float lai_sum = gLAI + gSLAI;
    system->sendVariable(qd, lai_sum);
}
void GenericLeaf::get_leaf_no(protocol::Component *system, protocol::QueryValueData &qd)
{
   system->sendVariable(qd, this->getLeafNo());
}
float GenericLeaf::getLeafNo(void)
{
   float sum = 0.0;
   for (int i = 0; i < max_node; i++) sum += gLeafNo[i];
   return sum;
}
float GenericLeaf::getDltNodeNo(void)
{
   return dltNodeNo;
}

float GenericLeaf::getNodeNo(void)
{
   return gNodeNo;
}

void GenericLeaf::get_leaf_area_tot(protocol::Component *system, protocol::QueryValueData &qd)
{
   float sum = 0.0;
   for (int i = 0; i < max_node; i++) sum += gLeafArea[i];
   system->sendVariable(qd, sum);
}

void GenericLeaf::get_leaf_no_sen(protocol::Component *system, protocol::QueryValueData &qd)
{
   float sum = 0.0;
   for (int i = 0; i < max_node; i++) sum += gLeafNoSen[i];
   system->sendVariable(qd, sum);
}

void GenericLeaf::get_leaf_area(protocol::Component *system, protocol::QueryValueData &qd)
{
   system->sendVariable(qd, std::vector<float>(gLeafArea, gLeafArea+20/*max_node*/)); // XX system can't handle big arrays..
}

// Clean out yesterday's rate calculations
void GenericLeaf::zeroDeltas(void)
{
   SimplePart::zeroDeltas();
   dltLAI = 0.0;
   dltSLAI = 0.0;
   dltLAI_pot = 0.0;
   dltLAI_stressed = 0.0;
   dltLAI_carbon = 0.0;  // (PFR)
   dltSLAI_detached = 0.0;
   dltSLAI_age = 0.0;
   dltSLAI_light = 0.0;
   dltSLAI_water = 0.0;
   dltSLAI_frost = 0.0;
   dltLeafNo              = 0.0;
//    g.dlt_node_no              = 0.0; JNGH - need to carry this through for site no next day.
   dltLeafNoPot = 0.0;
   dltNodeNoPot = 0.0;
}

// Initialise all constants & parameters
void GenericLeaf::zeroAllGlobals(void)
{
   SimplePart::zeroAllGlobals();
   cLeafNumberAtEmerg = 0.0;
   cSLAMin = 0.0;
   cInitialTPLA = 0.0;
   cNodeNoCorrection = 0.0;
   cLAISenLight = 0.0;
   cSenLightSlope = 0.0;

   gSLAI = 0.0;
   gLAI = 0.0;
   fill_real_array (gLeafNo , 0.0, max_node);
   fill_real_array (gLeafNoSen , 0.0, max_node);
   fill_real_array (gLeafArea , 0.0, max_node);
   gNodeNo = 0.0;
   gLeavesPerNode = 0.0;
   dltNodeNo = 0.0;

   coverLeaf.green = 0.0;
   coverLeaf.sen   = 0.0;

   fill_real_array (cXRowSpacing, 0.0, max_table);
   fill_real_array (cYExtinctCoef, 0.0, max_table);
   cNumRowSpacing = 0;

   ExternalSWDemand = false;
}

// Leaf, Node number and area initialisation
void GenericLeaf::onEmergence(void)
   {
   SimplePart::onEmergence();
   initialiseAreas();
   }

void GenericLeaf::onTransplanting(void)
   {
   SimplePart::onEmergence();
   initialiseAreas();
   }
void GenericLeaf::onKillStem(void)
   // transfer plant leaf area
   {
   SimplePart::onKillStem();
   //float deadLAI = gLAI;
   onEmergence();
   //gSLAI += deadLAI;
   }

// Initialise leaf areas to a newly emerged state.
void GenericLeaf::initialiseAreas(void)
   {
   gNodeNo = cLeafNumberAtEmerg;

   fill_real_array (gLeafNo, 0.0, max_node);
   fill_real_array (gLeafNoSen, 0.0, max_node);

   int   leaf_no_emerged = (int) cLeafNumberAtEmerg;
   float leaf_emerging_fract = fmod((double)cLeafNumberAtEmerg, (double)1.0);
   for (int leaf = 0; leaf < leaf_no_emerged; leaf++)
      {
      gLeafNo[leaf] = 1.0;
      }
   gLeafNo[leaf_no_emerged] = leaf_emerging_fract;

   fill_real_array (gLeafArea, 0.0, max_node);
   float avg_leaf_area = divide (cInitialTPLA, cLeafNumberAtEmerg, 0.0);
   for (int leaf = 0; leaf < leaf_no_emerged; leaf++)
      {
      gLeafArea[leaf] = avg_leaf_area * plant->population().Density();
      }
   gLeafArea[leaf_no_emerged] = leaf_emerging_fract * avg_leaf_area * plant->population().Density();

   gLAI = cInitialTPLA * smm2sm * plant->population().Density();
   gSLAI = 0.0;
   }

// Harvest event
void GenericLeaf::onHarvest(float /* cutting_height */, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
{
   onHarvest_GenericAboveGroundPart(remove_fr, dm_type, dlt_crop_dm, dlt_dm_n, dlt_dm_p, fraction_to_residue);
   initialiseAreas();
}


// Sanity checks
void GenericLeaf::checkBounds(void)
{
   SimplePart::checkBounds();
   if (gLAI < 0.0) throw std::runtime_error(myName + " LAI is negative! (" + ftoa(gLAI,".6") + ")");
   if (gSLAI < 0.0) throw std::runtime_error(myName + " SLAI is negative! (" + ftoa(gSLAI,".6") + ")");
   if (gNodeNo < 0) throw std::runtime_error(myName + " node number is negative! (" + ftoa(gNodeNo,".6") + ")");
   if (gNodeNo >= max_node) throw std::runtime_error(myName + " node number exceeds array size! (" + ftoa(gNodeNo,".6") + ")");

   //     Check that leaf records agree
   float leaf_area_tot = sum_real_array (gLeafArea, max_node) * smm2sm;

   if (! reals_are_equal (leaf_area_tot, gLAI + gSLAI, tolerance_lai))
     {
     ostringstream msg;
     msg << "Total leaf area doesn't match LAI. LAI = ";
     msg <<  leaf_area_tot << ". Lai total = " <<  (gLAI + gSLAI) << ends;
     throw std::runtime_error (msg.str());
     }

//    leaf_area_tot = 0.0;
//    for (int node = 0; node < max_node; node++)
//      {
//      leaf_area_tot +=
//                  divide (gLeafNoDead[node], gLeafNo[node], 0.0)
//                     * gLeafArea[node]
//                     * plant->population().Density() * smm2sm;
//      }
//
//    if (! reals_are_equal (leaf_area_tot, gLAI + gSLAI + gTLAI_dead, tolerance_lai))
//      {
//      ostrstream msg;
//      msg << "total leaf area doesn't match TPLA. LAI = ";
//      msg <<  leaf_area_tot << ". Lai total = " <<  (gLAI + gSLAI + gTLAI_dead) << ends;
//      throw std::runtime_error (msg.str());
//      }

   if (sum_real_array(gLeafNoSen, max_node) >
       sum_real_array(gLeafNo, max_node))
       {
       throw std::runtime_error ("Too much senesced leaf number - exceeds live leaves");
       }
}

// Calculate deltas from potential and stresses
void GenericLeaf::actual(void)
   {
        if (cLAIExpModelOption == "LAIRatePhoto")          // OPTIONAL: Uses the LAI expansion rate based on photoperiod
           this->leaf_area_actual_pp ();
	else if (cLAIExpModelOption == "default") 
   this->leaf_area_actual ();
	else                                               // Unrecognized Flag value in INI file
           throw runtime_error("Unknown LAI model: lai_exp_model_option = " + cLAIExpModelOption + " in " + plant->Name() + "parameterization." );
   this->leaf_no_actual ();
   }

//Purpose
//   Simulate actual crop leaf area development - checks that leaf area
//   development matches D_m production via a maximum specific leaf area
//   for the daily increase in LAI. SLA_max changes as a function of LAI.
//
void GenericLeaf::leaf_area_actual(void)
{
   float sla_max = cSLAMax.value(gLAI);                    //calculated daily max spec leaf area

   dltLAI_carbon = Growth.DM() * sla_max * smm2sm;         //maximum daily increase in leaf area
                                                           //index from carbon supply
   dltLAI = min(dltLAI_carbon, dltLAI_stressed);
}

void GenericLeaf::leaf_area_actual_pp(void)                 // Purpose: Set actual dlt LAI as "stressed dlt LAI" without SLA constraints (PFR)
	{
	   dltLAI = dltLAI_stressed;
	}

//Purpose
//   Simulate actual leaf number increase as limited by dry matter production.
void GenericLeaf::leaf_no_actual (void)
   {
   //ratio of actual to potential lai
   float lai_ratio = divide (dltLAI, dltLAI_stressed, 0.0);

   //ratio of actual to potential leaf appearance
   float leaf_no_frac= cLeafNoFrac.value(lai_ratio);

   dltLeafNo = dltLeafNoPot * leaf_no_frac;

   if (dltLeafNo < dltNodeNoPot)
      {
      dltNodeNo = dltLeafNo;
      }
   else
      {
      dltNodeNo = dltNodeNoPot;
      }
   }


//+  Purpose
//     Calculate the fractional death of oldest green leaf.
void GenericLeaf::leaf_death (float  g_nfact_expansion, float  g_dlt_tt)
   {
   float leaf_no_now;                            // total number of leaves yesterday
   float leaf_no_sen_now;                       // total number of dead leaves yesterday
   float leaf_death_rate;                        // thermal time for senescence of another leaf (oCd)
   float leaf_per_node;                          // no. of leaves senescing per node
   float tpla_now;                               //
   float max_sleaf_no_now;                       // max number of senesced leaves allowable
   float max_sen_area;                           // max area that can be senesced
   float node_sen_rate;


   leaf_no_now = sum_real_array (gLeafNo, max_node);

   leaf_per_node = leaf_no_now * cFrLeafSenRate;

   node_sen_rate = divide( cNodeSenRate
                          , 1.0 + cNFactLeafSenRate * (1.0 - g_nfact_expansion)
                          , 0.0);

   leaf_death_rate = divide (node_sen_rate, leaf_per_node, 0.0);

   if (plant->phenology().inPhase("harvest_ripe"))
       {
       // Constrain leaf death to remaining leaves
       //cnh do we really want to do this?;  XXXX
       leaf_no_sen_now = sum_real_array (gLeafNoSen,max_node);
       dltLeafNoSen = l_bound (leaf_no_now - leaf_no_sen_now, 0.0);
       }
   else if (plant->phenology().inPhase("leaf_senescence"))
//   else if (plant->phenology().stageNum() > )
       {
       dltLeafNoSen = divide (g_dlt_tt, leaf_death_rate, 0.0);

       // Ensure minimum leaf area remains
       tpla_now = sum_real_array (gLeafArea, max_node) ;
       max_sen_area = l_bound (tpla_now - cMinTPLA, 0.0) * plant->population().Density();
       max_sleaf_no_now = legnew_leaf_no_from_area (gLeafArea
                                                    , gLeafNo
                                                    , max_node
                                                    , max_sen_area);

       // Constrain leaf death to remaining leaves
       leaf_no_sen_now = sum_real_array (gLeafNoSen, max_node);
       dltLeafNoSen = u_bound (dltLeafNoSen, max_sleaf_no_now - leaf_no_sen_now);
       }
   else
       {
       dltLeafNoSen = 0.0;
       }
   }

// Public interface to calculate potentials
void GenericLeaf::CanopyExpansion (int leaf_no_pot_option /* (INPUT) option number*/
                              , float stressFactor    /* (INPUT) stress factor */
                              , float dlt_tt         /* (INPUT) Thermal Time */
                              , float AreaStressFactor)
   {
   this->leaf_no_pot (leaf_no_pot_option, stressFactor, dlt_tt);

   if (cLAIExpModelOption == "LAIRatePhoto")          // Uses the LAI expansion rate based on photoperiod (PFR)
         this->leaf_area_potential_pp ();
   else if (cLAIExpModelOption == "default")          // Used the original LAI model with node appearance leaves/node and leaf size
   this->leaf_area_potential ();
   else                                               // Unrecognized Flag value in INI file
       throw runtime_error("Unknown LAI model: lai_exp_model_option = " + cLAIExpModelOption + " in " + plant->Name() + "parameterization." );
   this->leaf_area_stressed(AreaStressFactor);
   }

//+  Purpose
//     Calculate leaf number development
void GenericLeaf::leaf_no_pot (int option, float stressFactor, float dlt_tt)
    {
    if (option == 1)
        {
        cproc_leaf_no_pot1(cNodeAppRate
                           , cLeavesPerNode
                           , plant->phenology().inPhase("node_formation")
                           , plant->phenology().onDayOf("emergence")
                           , gNodeNo
                           , dlt_tt
                           , &dltLeafNoPot
                           , &dltNodeNoPot);
        }
    else if (option == 2)
        {
        //wheat
        float tiller_no_now =  gNodeNo;
        cproc_leaf_no_pot3  (cNodeAppRate
                             , cLeavesPerNode
                             , plant->phenology().inPhase("tiller_formation")
                             , plant->phenology().onDayOf("emergence")
                             , tiller_no_now
                             , dlt_tt
                             , stressFactor
                             , &gLeavesPerNode
                             , &dltLeafNoPot
                             , &dltNodeNoPot);
        }
    else if (option == 3) 							// This option uses phyllochron as a function
        {                                                                       //  of photoperiod in sensitive phase(PFR)
        cproc_leaf_no_pot4(cNodeAppRatePhoto
                           , cLeavesPerNode
                           , cNodeAppRatePrePhoto
                           , cNodeAppRatePostPhoto
                           , plant->phenology().inPhase("node_formation")
                           , plant->phenology().inPhase("pre_photo_phyllochron")
                           , plant->phenology().inPhase("photo_phyllochron")
                           , plant->phenology().inPhase("post_photo_phyllochron")
                           , plant->phenology().onDayOf("emergence")
                           , plant->environment().dayLength()
                           , gNodeNo
                           , dlt_tt
                           , &gLeavesPerNode
                           , &dltLeafNoPot
                           , &dltNodeNoPot);
        }
    else
        {
        throw std::invalid_argument ("invalid template option in leaf_no_pot");
        }
    }

//+  Purpose
//  Calculate the potential increase in leaf area development (mm^2)
//  on an individual leaf basis, with account taken of the area of
//  currently expanding leaves (node_no_correction).
void GenericLeaf::leaf_area_potential ()
   {
   float node_no_now = gNodeNo + cNodeNoCorrection;

   float leaf_size = cLeafSize.value (node_no_now);

   dltLAI_pot =  dltLeafNoPot * leaf_size * smm2sm * plant->population().Density();
   }

  //+  Purpose
//  Option # Calculate the potential increase in leaf area development (mm^2)
//  on an individual leaf basis using an empirical relationship between photoperiod
// and leaf area expansion rate (Teixeira et 2009, Crop and Pasture 60 (8) 778-789)


void GenericLeaf::leaf_area_potential_pp ()
   {
   float photoperiod = plant->environment().dayLength();
   float LAIRatePhoto = cLAIRatePhoto.value (photoperiod);                      // Leaf area expansion rate (m^2/m^2/oCd, i.e. LAI/oCd)
   float dltTT = plant->phenology().TT();
   dltLAI_pot =  dltTT * LAIRatePhoto;
   }


//+  Purpose
//   Calculate the biomass non-limiting leaf area development from the
//   potential daily increase in lai and stress factors (water &
//   nitrogen)
void GenericLeaf::leaf_area_stressed (float stressFactor)
    {
    dltLAI_stressed = dltLAI_pot * stressFactor;
    }

void GenericLeaf::Detachment (void)
   {
        cproc_lai_detachment1 (c.sen_detach_frac
                               , gSLAI
                               , &dltSLAI_detached);


        plant_leaf_detachment (gLeafArea
                               , dltSLAI_detached
                               , 1.0  //required because gLeafArea is on an area basis not a plant basis
                               , max_node);

    SimplePart::Detachment();
   }

//   Calculate todays leaf area senescence
void GenericLeaf::leaf_area_sen(float swdef_photo)
{
    float plants = plant->population().Density();

    dltSLAI_age = legopt_leaf_area_sen_age1( gLeafNo
                              , gLeafNoSen
                              , dltLeafNoSen
                              , max_node
                              , gLAI
                              , gSLAI
                              , cMinTPLA
                              , gLeafArea
                              , 1.0);  // because gLeafArea is on an area basis and not a plant basis

    dltSLAI_light = crop_leaf_area_sen_light1 (cLAISenLight, cSenLightSlope, gLAI, plants, cMinTPLA);


    dltSLAI_water = crop_leaf_area_sen_water1 (cSenRateWater,
                               gLAI,
                               swdef_photo,
                               plants,
                               cMinTPLA);

    dltSLAI_frost = crop_leaf_area_sen_frost1(cSenescenceFac,
                              gLAI,
                              plant->environment().mint(),
                              plants,
                              cMinTPLA);

    dltSLAI = max(max(max(dltSLAI_age, dltSLAI_light), dltSLAI_water), dltSLAI_frost);
}

// Update state variables
void GenericLeaf::update(void)
{
    SimplePart::update();
    // need to account for truncation of partially developed leaf (add 1)
    float node_no = 1.0 + gNodeNo;

    float dlt_leaf_area = dltLAI * sm2smm;
    accumulate (dlt_leaf_area, gLeafArea, node_no-1.0, dltNodeNo);

    // Area senescence is calculated apart from plant number death
    // so any decrease in plant number will mean an increase in average
    // plant size as far as the leaf size record is concerned.

    // NIH - Don't think this is needed anymore because death goes into SLAI not TLAI_dead now
    //if ((plant->population().Density() /*+ g_dlt_plants*/)<=0.0)   //XXXX FIXME!!
    //    {
    //    fill_real_array(gLeafArea, 0.0, max_node);
    //    }

    accumulate (dltLeafNo, gLeafNo, node_no-1.0, dltNodeNo);

    float leaf_no_sen_tot = sum_real_array(gLeafNoSen, max_node) + dltLeafNoSen;

    for (int node = 0; node < max_node; node++)
        {
        if (leaf_no_sen_tot > gLeafNo[node])
            {
            leaf_no_sen_tot -=  gLeafNo[node];
            gLeafNoSen[node] = gLeafNo[node];
            }
        else
            {
            gLeafNoSen[node] = leaf_no_sen_tot;
            leaf_no_sen_tot = 0.0;
            }
        }
    gNodeNo += dltNodeNo;

    // transfer plant leaf area
    gLAI +=  dltLAI - dltSLAI;
    gSLAI += dltSLAI - dltSLAI_detached;

    // Transfer dead leaf areas
    float dying_fract_plants = plant->population().DyingFractionPlants();

    float dlt_lai_dead  = gLAI  * dying_fract_plants;
    gLAI -=  dlt_lai_dead;
    gSLAI += dlt_lai_dead;

}

// Remove detachment from leaf area record
void GenericLeaf::remove_detachment (float dlt_slai_detached, float dlt_lai_removed )
    {
    // Remove detachment from leaf area record from bottom upwards
    float area_detached = dlt_slai_detached * sm2smm;  // (mm2/plant)

    for (int node = 0; node < max_node; node++)
      {
      if(area_detached > gLeafArea[node])
        {
        area_detached = area_detached - gLeafArea[node];
        gLeafArea[node] = 0.0;
        }
      else
        {
        gLeafArea[node] = gLeafArea[node] - area_detached;
        break;
        }
      }

    // Remove detachment from leaf area record from top downwards
    float area_removed = dlt_lai_removed * sm2smm;  // (mm2/plant)

    for (int node = (int)gNodeNo; node >= 0 ; node--)
    {
      if(area_removed > gLeafArea[node])
      {
        area_removed = area_removed - gLeafArea[node];
        gLeafArea[node] = 0.0;
      }
      else
      {
        gLeafArea[node] = gLeafArea[node] - area_removed;
        break;
      }
   }

   // calc new node number
   for (int node = max_node - 1; node >= 0; node--)
      {
      if (!reals_are_equal(gLeafArea[node], 0.0, 1.0E-4))    // Slop?
         {
         gNodeNo = (float)node;  //FIXME - need adjustment for leafs remaining in for this node
         break;
         }
      }

   // calc new leaf number
   int newNodeNo = (int) (1.0 + gNodeNo);
   for (int node = newNodeNo - 1; node < max_node; node++)
      {
      gLeafNo[node] = 0.0;
      gLeafNoSen[node] = 0.0;
      }
}

void GenericLeaf::removeBiomass(void)
// (Re)-Initialise plant leaf area from deltas
    {
    float chop_fr_green = divide(GreenRemoved.DM(), Green.DM(), 0.0);
    float chop_fr_sen   = divide(SenescedRemoved.DM(), Senesced.DM(), 0.0);

    float dlt_lai = gLAI * chop_fr_green;
    float dlt_slai = gSLAI * chop_fr_sen;

    // keep leaf area above a minimum
    float lai_init = cInitialTPLA * smm2sm * plant->population().Density();
    float dlt_lai_max = gLAI - lai_init;
    dlt_lai = u_bound (dlt_lai, dlt_lai_max);

    gLAI -= dlt_lai;
    gSLAI -= dlt_slai;
    remove_detachment (dlt_slai, dlt_lai);

     SimplePart::removeBiomass();

    //Green = Green * chop_fr_green;
    //Senesced = Senesced * chop_fr_sen;

   // keep dm above a minimum
    float dm_init = c.dm_init * plant->population().Density();
    float n_init = dm_init * c.n_init_conc;
    if (Green.DM() < dm_init)
       {
       	// keep dm above a minimum
       Green.SetStructuralDM(Green.StructuralDM()*dm_init/Green.DM());
       Green.SetNonStructuralDM(Green.NonStructuralDM()*dm_init/Green.DM());
       }
    if (Green.N() < n_init)
       {
       	// keep N above a minimum
       Green.SetN(n_init);
       }


}

