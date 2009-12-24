#include "../StdPlant.h"

#include "../CompositePart.h"
#include "Leaf.h"
#include "CohortingLeaf.h"
#include "../Environment.h"
#include "../Phenology/Phenology.h"
#include "../Population.h"
using namespace std;


void CohortingLeaf::readConstants (protocol::Component *system, const string &section)
//=======================================================================================
// Read Constants
   {
   SimplePart::readConstants(system, section);
   // Nothing to do here..
   }

void CohortingLeaf::readSpeciesParameters (protocol::Component *system, vector<string> &search_order)
//=======================================================================================
// Read species specific parameters
   {
   SimplePart::readSpeciesParameters(system, search_order);
   scienceAPI.read("leaf_no_at_emerg", cLeafNumberAtEmerg, 0.0f, 100.0f);
   scienceAPI.read("initial_tpla", cInitialTPLA, 0.0f, 100000.0f);
   scienceAPI.read("min_tpla", cMinTPLA, 0.0f, 100000.0f);
   scienceAPI.read("sla_min", cSLAMin, 0.0f, 100000.0f);
   scienceAPI.read("fr_lf_sen_rate", cFrLeafSenRate, 0.0f, 1.0f);
   scienceAPI.read("node_sen_rate", cNodeSenRate, 0.0f, 1000.0f);
   scienceAPI.read("n_fact_lf_sen_rate", cNFactLeafSenRate, 0.0f, 5.0f);
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

   cLeavesPerNode.read(scienceAPI
                       , "x_node_no_leaf",  "()", 0.0, 200.0
                       , "y_leaves_per_node", "()", 0.0, 50.0);

   if (!cTilleringCriticalCover.readOptional(scienceAPI
                       , "CropCover",  "()", 0.0, 1.0
                       , "RelativeTillerAppearance", "()", 0.0, 1.0))
       cTilleringCriticalCover.setDefaultValue(1.0);   // If not there assume that cover has no effect on tillering

   cGrowthPeriod.read(scienceAPI,
                        "x_leaf_cohort", "(cohort)", 0, 50.0,
                        "y_leaf_growth_period", "(oC)", 0.0, 2000.0);

   cLagPeriod.read(scienceAPI,
                        "x_leaf_cohort", "(cohort)", 0, 50.0,
                        "y_leaf_lag_period", "(oC)", 0.0, 2000.0);

   cSenescingPeriod.read(scienceAPI,
                        "x_leaf_cohort", "(cohort)", 0, 50.0,
                        "y_leaf_sen_period", "(oC)", 0.0, 2000.0);


   cAreaPot.read(scienceAPI,
                        "x_leaf_cohort", "(cohort)", 0, 50.0,
                        "y_leaf_area_pot", "(mm^2)", 0.0, 2000000.0);

   scienceAPI.read("x_row_spacing", cXRowSpacing, cNumRowSpacing, 0.0f, 2000.0f);
   scienceAPI.read("y_extinct_coef", cYExtinctCoef, cNumRowSpacing, 0.0f, 1.0f);
   scienceAPI.read("y_extinct_coef_dead", cYExtinctCoefDead, cNumRowSpacing, 0.0f, 1.0f);
   scienceAPI.read("transp_eff_cf", c.transpEffCf, 0.0f, 1.0f);

   Photosynthesis->Read();
   }


void CohortingLeaf::onInit1(protocol::Component *system)
//=======================================================================================
// Connect our bits to the system
   {
   SimplePart::onInit1(system);
   Leaf::onInit1(system);
   setupGetFunction(system, "node_no", protocol::DTsingle, false,
                    &CohortingLeaf::get_node_no, "/plant", "Number of main stem nodes");

   setupGetFunction(system, "node_no_sen", protocol::DTsingle, false,
                    &CohortingLeaf::get_node_no_sen, "/plant", "Number of main stem nodes senesced");

   setupGetFunction(system, "node_no_fx", protocol::DTsingle, false,
                    &CohortingLeaf::get_node_no_fx, "/plant", "Number of main stem nodes senesced");

   setupGetFunction(system, "leaf_no", protocol::DTsingle, false,
                    &CohortingLeaf::get_leaf_no, "/plant", "Number of leaves on the plant");

   setupGetFunction(system, "leaf_area", protocol::DTsingle, true,
                    &CohortingLeaf::get_leaf_area, "mm^2/plant", "Leaf area for each leaf cohort");

   setupGetFunction(system, "leaf_area_max", protocol::DTsingle, true,
                    &CohortingLeaf::get_leaf_area_max, "mm^2/plant", "Maximum Leaf area for each leaf cohort");

   setupGetFunction(system, "leaf_area_tot", protocol::DTsingle, false,
                    &CohortingLeaf::get_leaf_area_tot, "mm^2/plant", "Total plant leaf area");

   setupGetFunction(system, "leaf_age", protocol::DTsingle, true,
                    &CohortingLeaf::get_leaf_age, "oCd", "Age of each leaf cohort");

   setupGetFunction(system, "lai_sum", protocol::DTsingle, false,
                    &CohortingLeaf::get_lai_sum, "m^2/m^2", "LAI of all leaf components");

   setupGetFunction(system, "tlai", protocol::DTsingle, false,
                    &CohortingLeaf::get_tlai, "m^2/m^2", "Total lai");

   setupGetFunction(system, "slai", protocol::DTsingle, false,
                    &CohortingLeaf::get_sen_leaf_area_index, "m^2/m^2", "Senesced leaf area index");

   setupGetFunction(system, "lai", protocol::DTsingle, false,
                    &CohortingLeaf::get_leaf_area_index, "m^2/m^2", "Leaf area index");

   system->addGettableVar("dlt_lai", dltLAI, "m^2/m^2", "Actual change in live plant lai");

   system->addGettableVar("dlt_lai_pot", dltLAI_pot, "m^2/m^2", "Potential change in live plant lai");

   system->addGettableVar("dlt_lai_stressed", dltLAI_stressed, "m^2/m^2", "Potential change in lai allowing for stress");

   system->addGettableVar("dlt_lai_carbon", dltLAI_carbon, "m^2/m^2", "Potential change in lai allowing for growth");

   system->addGettableVar("dlt_leaf_no", dltLeafNo, "leaves/m2", "Change in number of leaves");

   system->addGettableVar("dlt_leaf_no_pot", dltLeafNoPot, "m^2/m^2", "Potential Leaf no");

   system->addGettableVar("dlt_tiller_no", dltNodeNo, "tillers/m2", "Change in number of tillers");

   system->addGettableVar("dlt_node_no", dltNodeNo, "/m2", "Change in number of main stem nodes");

   setupGetFunction(system, "dlt_slai", protocol::DTsingle, false,
                    &CohortingLeaf::get_dlt_slai, "m^2/m^2", "Change in lai");

   setupGetFunction(system, "dlt_slai_age", protocol::DTsingle, false,
                    &CohortingLeaf::get_dlt_slai_age, "m^2/m^2", "Change in lai via age");

   system->addGettableVar("dlt_slai_light", dltSLAI_light, "m^2/m^2", "Change in lai via light");

   system->addGettableVar("dlt_slai_water", dltSLAI_water, "m^2/m^2", "Change in lai via water stress");

   system->addGettableVar("dlt_slai_frost", dltSLAI_frost, "m^2/m^2", "Change in lai via low temperature");


   system->addGettableVar("leaves_per_node", gLeavesPerNode, "","");
   }

void CohortingLeaf::get_tlai(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
    float tlai = getLAI() + getSLAI();
    system->sendVariable(qd, tlai);
}

void CohortingLeaf::get_lai_sum(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
    float lai_sum = getLAI() + getSLAI();
    system->sendVariable(qd, lai_sum);
}

void CohortingLeaf::get_leaf_no(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, sum(gLeafNo));
}

void CohortingLeaf::get_node_no(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   if (gNodeNo > 0)
      system->sendVariable(qd, (float)((int)gNodeNo));
   else
      system->sendVariable(qd, (float)(0.0));
}

void CohortingLeaf::get_node_no_sen(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   float node_no_sen = 0.0;

   if (gNodeNo == 0)
      node_no_sen = 0.0;

   else
      for (unsigned int cohort = 1; cohort != gLeafArea.size(); cohort++)
         {
         if (reals_are_equal(gLeafArea[cohort-1], 0.0, 1.0E-4)&&gLeafArea[cohort]>0.0)
            {
            // This is the senescing node
            node_no_sen = cohort-1+1+divide(gLeafAreaSen[cohort],gLeafAreaMax[cohort],0.0);
            break;
            }
         }

   system->sendVariable(qd, node_no_sen);
}

void CohortingLeaf::get_node_no_fx(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   float node_no_fx = 1.0; //gNodeNo;

   if (gNodeNo == 0)
      node_no_fx = 0.0;

   else
      for (unsigned int cohort = 0; cohort < gLeafArea.size(); cohort++)
         {

         if ((cohort == (gLeafArea.size()-1)) && gLeafAge[cohort]>cGrowthPeriod[cohort+1])
            {
            // We are dealing with the top node
            node_no_fx = cohort + 1;
            break;
            }
         else if (gLeafAge[cohort]>cGrowthPeriod[cohort+1] && gLeafAge[cohort+1]<=cGrowthPeriod[cohort+1+1])
            {
            // This is the expanded node
            node_no_fx = cohort+1; //+divide(gLeafAge[cohort+1],cGrowthPeriod[cohort+1+1],0.0);
            break;
            }
         }

   system->sendVariable(qd, node_no_fx);
}
void CohortingLeaf::get_dlt_slai_age(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, sum(dltSLA_age) * plant->population().Density() * smm2sm);
}

float CohortingLeaf::getLeafNo(void)
//=======================================================================================
{
   return (sum(gLeafNo));
}

void CohortingLeaf::get_leaf_area_tot(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, sum(gLeafArea));
}

void CohortingLeaf::get_leaf_area(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, gLeafArea);
}

void CohortingLeaf::get_leaf_area_max(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, gLeafAreaMax);
}

void CohortingLeaf::get_leaf_age(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, gLeafAge);
}

void CohortingLeaf::get_leaf_area_index(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, getLAI());
}

void CohortingLeaf::get_sen_leaf_area_index(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, getSLAI());
}

void CohortingLeaf::get_dlt_slai(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   float dltSLAI = max(max(max(sum(dltSLA_age) * plant->population().Density() * smm2sm,
                               dltSLAI_light),
                               dltSLAI_water),
                               dltSLAI_frost);
   system->sendVariable(qd, dltSLAI);
}

void CohortingLeaf::zeroDeltas(void)
//=======================================================================================
// Clean out yesterday's rate calculations
{
   SimplePart::zeroDeltas();
   dltLAI = 0.0;
   dltLAI_pot = 0.0;
   dltLAI_stressed = 0.0;
   dltLAI_carbon = 0.0;
   dltSLAI_detached = 0.0;
   dltSLAI_light = 0.0;
   dltSLAI_water = 0.0;
   dltSLAI_frost = 0.0;
   dltLeafNo              = 0.0;
   gDltLeavesPerNode = 0.0;
//    g.dlt_node_no              = 0.0; JNGH - need to carry this through for site no next day.
   dltNodeNo = 0.0;
   setTo(dltSLA_age, (float) 0.0);
}

void CohortingLeaf::zeroAllGlobals(void)
//=======================================================================================
// Initialise all constants & parameters
{
   SimplePart::zeroAllGlobals();
   cLeafNumberAtEmerg = 0.0;
   cSLAMin = 0.0;
   cInitialTPLA = 0.0;
   cLAISenLight = 0.0;
   cSenLightSlope = 0.0;
   //cGrowthPeriod.clear();
   //cAreaPot.clear();

   gLeafAge.clear();
   gLeafArea.clear();
   gLeafAreaMax.clear();
   gLeafAreaSen.clear();
   dltSLA_age.clear();
   gDltLeafAreaPot.clear();
   gLeafNo.clear();
   gLeavesPerNode = 0.0;
   gDltLeavesPerNode = 0.0;

   gNodeNo = 0.0;
   dltNodeNo = 0.0;
   coverLeaf.green = 0.0;
   coverLeaf.sen = 0.0;

   ExternalSWDemand = false;

}

void CohortingLeaf::onEmergence(void)
//=======================================================================================
// Leaf, Node number and area initialisation
   {
   SimplePart::onEmergence();
   initialiseAreas();
   }

void CohortingLeaf::onTransplanting(void)
   {
   SimplePart::onEmergence();
   initialiseAreas();
   }

void CohortingLeaf::onKillStem(void)
//=======================================================================================
// transfer plant leaf area to dead pool
   {
   SimplePart::onKillStem();
   vector <float> dying = gLeafArea;
   onEmergence();

   for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++)
      gLeafAreaSen[cohort] =gLeafAreaSen[cohort]+ dying[cohort];
   }

void CohortingLeaf::initialiseAreas(void)
//=======================================================================================
// Initialise leaf areas to a newly emerged state.
   {
   gNodeNo = 0.0;

   gLeafArea.clear();
   gDltLeafAreaPot.clear();
   gLeafAreaMax.clear();
   gLeafAreaSen.clear();
   gLeafAge.clear();
   gLeafNo.clear();
   dltSLA_age.clear();

   // Fill cohorts until no more area available
   for (int cohort = 0; cohort < cLeafNumberAtEmerg; cohort++)
      {
      gLeafArea.push_back(cAreaPot[cohort+1]);
      gLeafAreaMax.push_back(cAreaPot[cohort+1]);
      gLeafAreaSen.push_back(0.0);
      gLeafAge.push_back(0.0);
      gDltLeafAreaPot.push_back(0.0);
      gLeafNo.push_back(1.0);
      dltSLA_age.push_back(0.0);
      gNodeNo += 1.0;
      }

   }

void CohortingLeaf::onHarvest(float /* cutting_height */, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
//=======================================================================================
// Harvest event
{
   onHarvest_GenericAboveGroundPart(remove_fr, dm_type, dlt_crop_dm, dlt_dm_n, dlt_dm_p, fraction_to_residue);
   initialiseAreas();
}

void CohortingLeaf::checkBounds(void)
//=======================================================================================
// Sanity checks
   {
   SimplePart::checkBounds();
   if (gNodeNo < 0) throw std::runtime_error(myName + " node number is negative! (" + ftoa(gNodeNo,".6") + ")");

   for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++)
      {
      if (gLeafArea[cohort] < 0.0)
         throw std::runtime_error(myName + " LA cohort is negative! (" + ftoa(gLeafArea[cohort],".6") + ")");
      if (gLeafAreaSen[cohort] < 0.0)
         throw std::runtime_error(myName + " LA Senesced cohort is negative! (" + ftoa(gLeafAreaSen[cohort],".6") + ")");
      if (gLeafNo[cohort] < 0)
         throw std::runtime_error(myName + " leaf number is negative! (" + ftoa(gLeafNo[cohort],".6") + ")");
      }
   }

void CohortingLeaf::actual(void)
//=======================================================================================
// Plant is telling us to calculate deltas from potential and stresses
   {
   this->leaf_area_actual ();
   this->leaf_no_actual ();
   }

void CohortingLeaf::leaf_area_actual(void)
//=======================================================================================
//   Simulate actual crop leaf area development - checks that leaf area
//   development matches D_m production via a maximum specific leaf area
//   for the daily increase in LAI. SLA_max changes as a function of LAI.
{
   float sla_max = cSLAMax[getLAI()];                      //calculated daily max spec leaf area

   dltLAI_carbon = Growth.DM() * sla_max * smm2sm;  //maximum daily increase in leaf area
                                                           //index from carbon supply
   //cout <<  dltLAI_carbon, dltLAI_stressed;
//    char  msg[200];                               // message
//       sprintf (msg, "%10.6f%10.6f%10.6f",dltLAI_carbon, dltLAI_stressed,dltLAI_pot);
//    plant->writeString (msg);

   dltLAI = min(dltLAI_carbon, dltLAI_stressed);
}

void CohortingLeaf::leaf_no_actual (void)
//=======================================================================================
//   Simulate actual leaf & tiller number increase as limited by dry matter production.
   {
   //ratio of actual to potential lai
   float lai_ratio = divide (dltLAI, dltLAI_stressed, 0.0);

   //ratio of actual to potential leaf appearance
   float leaf_no_frac= cLeafNoFrac[lai_ratio];


   dltLeafNo = dltLeafNoPot * leaf_no_frac;
   if (dltNodeNo > dltLeafNo) dltNodeNo = dltLeafNo;
   gDltLeavesPerNode = gDltLeavesPerNode * leaf_no_frac;

   }


void CohortingLeaf::leaf_death (float  /* g_nfact_expansion*/, float  /* g_dlt_tt*/)
//=======================================================================================
//     Calculate the fractional death of oldest green leaf.
   {
//XXX Fix me
   }

void CohortingLeaf::CanopyExpansion (int /*leaf_no_pot_option*/, /* (INPUT) option number*/
                                   float stressFactor,     /* (INPUT) stress factor */
                                   float dlt_tt,           /* (INPUT) Thermal Time */
                                   float AreaStressFactor)
//=======================================================================================
// Plant is telling us to calculate potentials
   {
   dltTT = dlt_tt; //Yuck..  XXXXXXXXXXX
   // NIH Going to try using this approach for indeterminants.  Might cause memory problems for long crops.
   //if (leaf_no_pot_option != 2) throw std::invalid_argument("cohorting not implemented for indeterminates");

   this->leaf_no_pot (stressFactor, dlt_tt);
   this->leaf_area_potential (dlt_tt);
   this->leaf_area_stressed (AreaStressFactor);
   }

void CohortingLeaf::leaf_no_pot (float stressFactor, float dlt_tt)
//=======================================================================================
//     Calculate leaf number development
    {
    bool tillering = plant->phenology().inPhase("tiller_formation");
    if (tillering)
       {
       float node_app_rate = cNodeAppRate[gNodeNo];
       dltNodeNo = divide (dlt_tt, node_app_rate, 0.0);
       }
    else
       dltNodeNo = 0.0;

    dltLeafNoPot = 0.0;
    if (tillering)
        {
        float leaves_per_node_now = cLeavesPerNode[gNodeNo];

        gLeavesPerNode = min(gLeavesPerNode, leaves_per_node_now);

        gDltLeavesPerNode = cLeavesPerNode[gNodeNo + dltNodeNo] - leaves_per_node_now;
        gDltLeavesPerNode = gDltLeavesPerNode * cTilleringCriticalCover[coverTotal()];

        if (gDltLeavesPerNode > 0.0)
           gDltLeavesPerNode =  gDltLeavesPerNode * stressFactor;

        gDltLeavesPerNode = max(1.0 - gLeavesPerNode, gDltLeavesPerNode);  //cannot reduce LPN to below 1

        dltLeafNoPot = dltNodeNo * (gLeavesPerNode + gDltLeavesPerNode);
        }
    }

void CohortingLeaf::leaf_area_potential (float tt)
//=======================================================================================
//  Calculate the potential increase in leaf area development (mm^2)
   {
   for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++)
      {
      if (cGrowthPeriod[cohort+1] - gLeafAge[cohort] > 0.0)
         gDltLeafAreaPot[cohort] = cAreaPot[cohort+1] * (gLeavesPerNode+gDltLeavesPerNode) * u_bound(divide(tt, cGrowthPeriod[cohort+1], 0.0), 1.0);
      else
         gDltLeafAreaPot[cohort] = 0.0;
      }

   dltLAI_pot =  sum(gDltLeafAreaPot) * smm2sm * plant->population().Density();
   }

void CohortingLeaf::leaf_area_stressed (float stressFactor)
//=======================================================================================
//   Calculate the biomass non-limiting leaf area development from the
//   potential daily increase in lai and stress factors (water & nitrogen)
   {
   dltLAI_stressed = dltLAI_pot * stressFactor;
   }

void CohortingLeaf::Detachment (void)
//=======================================================================================
   {
   cproc_lai_detachment1 (c.sen_detach_frac
                               , getSLAI()
                               , &dltSLAI_detached);

   float area_detached = divide(dltSLAI_detached,  plant->population().Density(), 0.0) * sm2smm;

   for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++)
      {
      if(area_detached > gLeafAreaSen[cohort])
        {
        area_detached -= gLeafAreaSen[cohort];
        gLeafAreaSen[cohort] = 0.0;
        }
      else
        {
        gLeafAreaSen[cohort] -= area_detached;
        break;
        }
      }

    SimplePart::Detachment();
  }

void CohortingLeaf::leaf_area_sen(float swdef_photo)
//=======================================================================================
//   Calculate todays leaf area senescence
{
    float plants = plant->population().Density();

    // Age senescence for each cohort
    for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++)
       if (gLeafAge[cohort] > (cGrowthPeriod[cohort+1] + cLagPeriod[cohort+1]) &&
           gLeafAge[cohort] < (cGrowthPeriod[cohort+1] + cLagPeriod[cohort+1] + cSenescingPeriod[cohort+1]))
          {
//          float qq = (gLeafAreaMax[cohort]*dltTT)/cSenescingPeriod[cohort];
//          if (qq > gLeafAreaMax[cohort])
//             dltSLA_age[cohort] = gLeafAreaMax[cohort];
//          else
//             dltSLA_age[cohort] = qq;
          float tt_remaining = max(0.0,cGrowthPeriod[cohort+1] + cLagPeriod[cohort+1] + cSenescingPeriod[cohort+1] - gLeafAge[cohort]);
          float senfr = min(1.0,divide(dltTT,tt_remaining,0.0));
          dltSLA_age[cohort] = gLeafArea[cohort]*senfr;
          }

    dltSLAI_light = crop_leaf_area_sen_light1 (cLAISenLight, cSenLightSlope, getLAI(), plants, cMinTPLA);


    dltSLAI_water = crop_leaf_area_sen_water1 (cSenRateWater,
                               getLAI(),
                               swdef_photo,
                               plants,
                               cMinTPLA);

    dltSLAI_frost = crop_leaf_area_sen_frost1(cSenescenceFac,
                              getLAI(),
                              plant->environment().mint(),
                              plants,
                              cMinTPLA);
}

// Update state variables
void CohortingLeaf::update(void)
//=======================================================================================
{
   unsigned int cohort;
   SimplePart::update();

   if (((int)gNodeNo) != ((int)(gNodeNo + dltNodeNo))) {
      // Initiate a new cohort
      gLeafArea.push_back(0.0);
      gDltLeafAreaPot.push_back(0.0);
      gLeafAreaMax.push_back(0.0);
      gLeafAreaSen.push_back(0.0);
      gLeafAge.push_back(0.0);
      gLeafNo.push_back(0.0);
      dltSLA_age.push_back(0.0);
   }
   if (gLeafArea.size() > 0)
     gLeafNo[gLeafArea.size()-1] += dltLeafNo;  // Add leaves to currently expanding cohort.

   gNodeNo += dltNodeNo;
   gLeavesPerNode = gLeavesPerNode + gDltLeavesPerNode;

   for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++)
      gLeafAge[cohort] += dltTT;

   float dltLeafArea = divide (dltLAI, plant->population().Density(), 0.0) * sm2smm;

    // Partition new LAI to cohorts

    if (dltLeafArea > 0.0)
       {
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          float fract = divide(gDltLeafAreaPot[cohort], sum(gDltLeafAreaPot), 0.0);
          gLeafArea[cohort] += fract * dltLeafArea;
          }
       }

    // Transfer SLAI processes within cohorts
    // XX We need to re-think all of these
    if (sum(dltSLA_age) > 0.0)
       {
       // Age senescence per cohort
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          gLeafArea[cohort] = l_bound(gLeafArea[cohort] - dltSLA_age[cohort], 0.0);
          gLeafAreaSen[cohort] += dltSLA_age[cohort];
          }
       }
    if (dltSLAI_light > 0.0)
       {
       // bottom up (shading)
       float dltLeafArea = divide (dltSLAI_light, plant->population().Density(), 0.0) * sm2smm;
       for (cohort = 0; cohort != gLeafArea.size() && dltLeafArea > 0.0; cohort++)
          {
          float dlt = (dltLeafArea > gLeafArea[cohort]) ? gLeafArea[cohort] : dltLeafArea;
          gLeafArea[cohort] -= dlt;
          gLeafAreaSen[cohort] += dlt;
          dltLeafArea -= dlt;
          }
       }
    if (dltSLAI_water > 0.0)
       {
       // bottom up
       float dltLeafArea = divide (dltSLAI_water, plant->population().Density(), 0.0) * sm2smm;
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          float dlt = (dltLeafArea > gLeafArea[cohort]) ? gLeafArea[cohort] : dltLeafArea;
          gLeafArea[cohort] -= dlt;
          gLeafAreaSen[cohort] += dlt;
          dltLeafArea -= dlt;
          }
       }
    if (dltSLAI_frost > 0.0)
       {
       // top down
       float dltLeafArea = divide (dltSLAI_frost, plant->population().Density(), 0.0) * sm2smm;
       for (int cohort = (int)gLeafArea.size()-1; cohort >= 0 && dltLeafArea > 0.0; cohort--)
          {
          float dlt = (dltLeafArea > gLeafArea[cohort]) ? gLeafArea[cohort] : dltLeafArea;
          gLeafArea[cohort] -= dlt;
          gLeafAreaSen[cohort] += dlt;
          dltLeafArea -= dlt;
          }
       }

    // Plant death
    float dying_fract_plants = plant->population().DyingFractionPlants();
    //XX I'm not sure any of this is needed???????
    if (dying_fract_plants > 0.0)
       {
       // uniform effect
       float dltLeafArea = sum(gLeafArea) * dying_fract_plants;
       float areaTot = sum(gLeafArea);
       float dltLeafAreaSen = sum(gLeafAreaSen) * dying_fract_plants;
       float areaTotSen = sum(gLeafAreaSen);
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          float area = dltLeafArea * divide(gLeafArea[cohort], areaTot, 0.0);
          gLeafArea[cohort] = l_bound(gLeafArea[cohort] - area, 0.0);

          area = dltLeafAreaSen * divide(gLeafAreaSen[cohort], areaTotSen, 0.0);
          gLeafAreaSen[cohort] = l_bound(gLeafAreaSen[cohort] - area, 0.0);
          }
       }

       // Keep track of maximum size of each cohort
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          if(gLeafArea[cohort] > gLeafAreaMax[cohort])
             {
             gLeafAreaMax[cohort] = gLeafArea[cohort];
             }
          }
}

// Uniform removal from each cohort
void CohortingLeaf::removeBiomass(void)
   {
   unsigned int cohort;
   float chop_fr_green = bound(divide(GreenRemoved.DM(), Green.DM(), 0.0), 0.0, 1.0);
   if (chop_fr_green > 0.0)
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          gLeafArea[cohort] *= chop_fr_green;

   float chop_fr_sen   = divide(SenescedRemoved.DM(), Senesced.DM(), 0.0);
   if (chop_fr_sen > 0.0)
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          gLeafAreaSen[cohort] *= chop_fr_sen;

   SimplePart::removeBiomass();

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

float CohortingLeaf::senFract (void)
//=======================================================================================
   {
   float dltSLAI = max(max(max(sum(dltSLA_age) * plant->population().Density() * smm2sm,
                               dltSLAI_light),
                               dltSLAI_water),
                               dltSLAI_frost);

   return(divide (dltSLAI, getLAI() + dltLAI, 0.0));             // fraction of canopy senescing
   }

