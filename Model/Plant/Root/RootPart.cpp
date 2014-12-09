#include "StdPlant.h"

#include "RootPart.h"
#include "RootGrowthOption1.h"
#include "RootGrowthOption2.h"
#include "../Environment.h"
#include "Soil.h"
#include "../Phenology/Phenology.h"
#include "../Population.h"
#include <numeric>
#include <math.h>
using namespace std;

string IncorpFOMType = protocol::DDML(protocol::IncorpFomType());
string floatArrayType = protocol::DDML(vector<float>());


RootPart::RootPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : RootBase(scienceAPI, p, name)
//=======================================================================================
// Constructor
   {
   Soil *s = new Soil(scienceAPI, *p);
   soil.push_back(s);

   zeroAllGlobals();
   }
RootPart::~RootPart()
   {
   for (vector<Soil*>::iterator s = soil.begin(); s != soil.end(); s++)
      delete *s;

   }


void RootPart::zeroAllGlobals(void)
//=======================================================================================
// Zero all global values
   {
   SimplePart::zeroAllGlobals();
   root_depth            = 0.0;
   initialRootDepth = 0.0;
   rootDieBackFraction = 0.0;
   specificRootLength = 0.0;

   fill_real_array (root_length , 0.0, max_layer);
   fill_real_array (root_length_senesced, 0.0, max_layer);

   uptake_source = "";
   root_depth_max = 0.0;
   root_layer_max = 0;
   }

void RootPart::zeroDeltas(void)
//=======================================================================================
// Zero all daily deltas
   {
   SimplePart::zeroDeltas();

   for (vector<Soil*>::iterator s = soil.begin(); s != soil.end(); s++)
      (*s)->ZeroDeltas();

   dltRootDepth = 0.0;
   setTo(dltRootLength, (float)0.0);
   setTo(dltRootLengthSenesced, (float)0.0);
   setTo(dltRootLengthDead, (float)0.0);

   }

void RootPart::onInit1(protocol::Component *system)
//=======================================================================================
// Perform all component initialisation.
   {
   SimplePart::onInit1(system);
   (*soil[0]).onInit1 (system);

   system->addGettableVar("root_depth",
               root_depth, "mm", "depth of roots");

   system->addGettableVar("root_depth_max",
               root_depth_max, "mm", "Maximum depth of roots");
               
   system->addGettableVar("root_layer_max",
               root_layer_max, "", "Maximum layer number of roots");               

   setupGetFunction(system, "root_length", protocol::DTsingle, true,
                    &RootPart::get_root_length,
                    "mm/mm^2", "Root length");

   setupGetFunction(system, "root_length_senesced", protocol::DTsingle, true,
                    &RootPart::get_root_length_senesced, "mm/mm^2", "Senesced root length");

   setupGetFunction(system, "rlv", protocol::DTsingle, true,
                    &RootPart::get_rlv, "mm/mm^3", "Root length density");

   setupGetFunction(system, "rld", protocol::DTsingle, true,
                    &RootPart::get_rlv, "mm/mm^3", "Root length density");

   setupGetFunction(system, "no3gsm_uptake_pot", protocol::DTsingle, true,
                    &RootPart::get_no3gsm_uptake_pot,
                    "g/m2", "Pot NO3 uptake");

   setupGetFunction(system, "nh4gsm_uptake_pot", protocol::DTsingle, true,
                    &RootPart::get_nh4gsm_uptake_pot,
                    "g/m2", "Pot NH4 uptake");

   // Respond to Get
   setupGetFunction(system, "sw_uptake", protocol::DTsingle, true,
                    &RootPart::get_sw_uptake, "mm", "Plant water uptake per layer");
   setupGetFunction(system, "sw_supply", protocol::DTsingle, false,
                    &RootPart::get_sw_supply, "mm", "Soil water supply");
   setupGetFunction(system, "sw_deficit", protocol::DTsingle, false,
                    &RootPart::get_sw_deficit, "mm", "Soil water deficit");
   setupGetFunction(system, "sw_supply_layr", protocol::DTsingle, true,
                    &RootPart::get_sw_supply_layr, "mm", "Soil water supply");
   setupGetFunction(system, "ep", protocol::DTsingle, false,
                    &RootPart::get_ep, "mm", "Plant water uptake");
   setupGetFunction(system, "esw_layr", protocol::DTsingle, true,
                    &RootPart::get_esw_layr, "mm", "Extractable soil water");
   setupGetFunction(system, "no3_uptake", protocol::DTsingle, true,
                    &RootPart::get_no3_uptake,"kg/ha","NO3 uptake");
   setupGetFunction(system, "nh4_uptake", protocol::DTsingle, true,
                    &RootPart::get_nh4_uptake,"kg/ha","NH4 uptake");
   setupGetFunction(system, "no3_tot", protocol::DTsingle, false,
                    &RootPart::get_no3_tot,"g/m^2", "NO3 available to plants");
   setupGetFunction(system, "ll", protocol::DTsingle, true,
                    &RootPart::get_ll,"%vol","Crop lower limit");
   setupGetFunction(system, "n_supply_soil", protocol::DTsingle, false,
                    &RootPart::get_n_supply_soil,
                    "g/m^2", "N supply");

   }

void RootPart::read()
//=======================================================================================
// Read all parameters
   {
   //SimplePart::readConstants(NULL, "");
   //SimplePart::readSpeciesParameters(NULL, vector<string>());
   //SimplePart::readCultivarParameters(NULL, "");
   
   string st;
   scienceAPI.readOptional("MaxRootDepth", st);
   if (st != "" && Is_numerical(st.c_str()))
      MaxRootDepth = atof(st.c_str());
   else
      MaxRootDepth = 0;
   scienceAPI.readOptional("crop_type", crop_type);

   // Read species-specific parameters

   scienceAPI.read("specific_root_length", specificRootLength,0.0f, 1000000.0f);

   float n_conc = 0.0;
   scienceAPI.read("n_conc_crit_root", n_conc, 0.0f, 100.0f);
   SimplePart::c.n_conc_crit.setDefaultValue(n_conc);

   scienceAPI.read("n_conc_max_root", n_conc, 0.0f, 100.0f);
   SimplePart::c.n_conc_max.setDefaultValue(n_conc);

   scienceAPI.read("n_conc_min_root", n_conc, 0.0f, 100.0f);
   SimplePart::c.n_conc_min.setDefaultValue(n_conc);

   scienceAPI.read("initial_root_depth", initialRootDepth, 0.0f, 1000.0f);
   scienceAPI.read("specific_root_length", specificRootLength, 0.0f, 1000000.0f);
   scienceAPI.read("root_die_back_fr", rootDieBackFraction, 0.0f, 0.99f);

   rel_root_rate.read(scienceAPI,
                         "x_plant_rld", "()", 0.0f, 0.1f,
                         "y_rel_root_rate", "()", 0.001f, 1.0f);

   sw_fac_root.read(scienceAPI,
                         "x_sw_ratio", "()", 0.0f, 100.0f,
                         "y_sw_fac_root", "()", 0.0f, 100.0f);

   rel_root_advance.read(scienceAPI,
                         "x_temp_root_advance", "(oc)", -10.0f, 60.0f,
                         "y_rel_root_advance", "()", 0.0, 1.0f);

   root_depth_rate.read(scienceAPI,
                         "stage_code_list", "()", 0.0, 100.0f,
                         "root_depth_rate", "(mm/day)", 0.0, 1000.0f);

   ws_root_fac.read(scienceAPI,
                         "x_ws_root", "()", 0.0, 1.0f,
                         "y_ws_root_fac", "()", 0.0, 1.0f);

// NIH This really should be inherited from simple part.
    c.MaintenanceCoefficient.readOptional(scienceAPI
                        , "MaintenanceCoefficientStage", "()", 0.0, 100.0
                        , (myName+"MaintenanceCoefficient").c_str(), "()", 0.0, 1.0);
   
   scienceAPI.readOptional("uptake_source", uptake_source);
   if (uptake_source == "")uptake_source = "calc";
   float swim3=0.0;
   scienceAPI.getOptional("swim3", "-", swim3,0.0,1.0);
   if(swim3>0.0)
   	 {
   	 uptake_source = "swim3";
     ostringstream msg;
     msg << "Using SWIM3 for Soil Water Uptake."<<endl;
   	 scienceAPI.write(msg.str());
   	 }

   (*soil[0]).Read();
   }

void RootPart::write()
//=======================================================================================
// Write all parameters as a summary to stdout.
   {
   (*soil[0]).write();
   
   if (MaxRootDepth > 0)
      {
      ostringstream msg;
      msg << "    **** A maximum rooting depth has been specified: " << MaxRootDepth << "mm" << endl;
   	  scienceAPI.write(msg.str());
   	  }
   }

void RootPart::onTransplanting(void)
   {
   onSowing();
   onGermination();
   onEmergence();
   }
void RootPart::onSowing(void)
//=======================================================================================
// Sowing Event Handler
   {
   int n = (*soil[0]).num_layers;
   dltRootLength.clear(); dltRootLength.resize(n);
   dltRootLengthDead.clear(); dltRootLengthDead.resize(n);
   dltRootLengthSenesced.clear(); dltRootLengthSenesced.resize(n);
   }

void RootPart::onGermination(void)
//=======================================================================================
// Germination Event Handler
   {
   SimplePart::onGermination();
   root_depth = initialRootDepth;
   }

void RootPart::onEmergence(void)
//=======================================================================================
//     Initialise crop root length at emergence based on root weight
//     at emergence and specific root length.
   {
   SimplePart::onEmergence();


   // initial root length (mm/mm^2)
   float initial_root_length = Green.DM() / sm2smm * specificRootLength;

   // initial root length density (mm/mm^3)
   float rld = (float)divide (initial_root_length, root_depth, 0.0);

   int deepest_layer = (*soil[0]).find_layer_no (root_depth);

   for (int layer = 0; layer <= deepest_layer; layer++)
      root_length[layer] = rld *(*soil[0]).dlayer[layer] * (*soil[0]).root_proportion (layer, root_depth);
   Debug("Root.InitRootLength=%f", sum_real_array(root_length, max_layer));
   }

void RootPart::onFlowering(void)
//=======================================================================================
// Flowering Event Handler
   {
   }

void RootPart::onStartGrainFill(void)
//=======================================================================================
// Start of Grain Filling Event Handler
   {
   }

void RootPart::onHarvest(float /*cutting_height*/, float /* remove_fr*/,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
//=======================================================================================
// Harvesting Event Handler
   {
   // Push dead fraction into senesced pool

   Biomass Dead;
   Dead = Green*rootDieBackFraction;
   // however dead roots have a given N concentration
   Dead.SetN(Dead.DM()*c.n_sen_conc);

   Green = Green - Dead;
   Senesced = Senesced + Dead;

   // Unlike above ground parts, no roots go to surface residue module.
   dm_type.push_back(myName);
   fraction_to_residue.push_back(0.0);
   dlt_crop_dm.push_back(0.0);
   dlt_dm_n.push_back(0.0);
   dlt_dm_p.push_back(0.0);
   }


void RootPart::onKillStem(void)
//=======================================================================================
// Kill Stem Event Handler
   {
   // Calculate Root Die Back

   Biomass Dead;
   Dead = Green*rootDieBackFraction;
   // however dead roots have a given N concentration
   Dead.SetN(Dead.DM()*c.n_sen_conc);

   Green = Green - Dead;
   Senesced = Senesced + Dead;

   SimplePart::onKillStem();
   }

void RootPart::plant_root_depth (void)
//=======================================================================================
//  Calculate change in plant rooting depth
   {
   const Environment *e = &plant->environment();
   //Temperature factor
   float avg_temp = (e->mint() + e->maxt())/2.0f;
   float temp_factor = rel_root_advance.value(avg_temp);

   //Water stress factor
   float ws_factor = ws_root_fac.value (plant->getSwdefPhoto());

   //Soil water availability factor
   int deepest_layer = (*soil[0]).num_layers-1;

   //  the layer with root front
   int layer = (*soil[0]).find_layer_no(root_depth);

   float cum_depth = sum_real_array((*soil[0]).dlayer, layer+1);
   float rootdepth_in_layer = (*soil[0]).dlayer[layer] - (cum_depth - root_depth);

   rootdepth_in_layer = bound (rootdepth_in_layer, 0.0, (*soil[0]).dlayer[layer]);

   float weighting_factor = (float)divide (rootdepth_in_layer, (*soil[0]).dlayer[layer], 0.0);

   int next_layer = min(layer+1, deepest_layer);

   float fasw1 = (*soil[0]).layer_fasw(layer);

   float fasw2 = (*soil[0]).layer_fasw(next_layer);

   fasw1 = (float)min(1.0,max(0.0, fasw1));
   fasw2 = (float)min(1.0,max(0.0, fasw2));

   float fasw = weighting_factor * fasw2 + (1.0f - weighting_factor) * fasw1;

   float sw_avail_factor = sw_fac_root.value(fasw);

   float RelRFV=-1.0;
   scienceAPI.getOptional("RelRFV", "-", RelRFV,0.0,1.0);
   if(RelRFV>0.0)
   	 {
       // There is an output out there for relative root front velocity so use this
       // instead of soil water factor here
       sw_avail_factor = RelRFV;
       }

   
   
   // this equation allows soil water in the deepest
   // layer in which roots are growing
   // to affect the daily increase in rooting depth.
   dltRootDepth  = plant->phenology().doInterpolation(root_depth_rate) *
                     temp_factor *
                       min(ws_factor, sw_avail_factor) *
                         (*soil[0]).xf[layer];

   // prevent roots partially entering layers where xf == 0
   for (deepest_layer = (*soil[0]).xf.size()-1;
        deepest_layer >= 0 && 
        ((*soil[0]).xf[deepest_layer] <= 0.0 || (*soil[0]).getModifiedKL(deepest_layer) <= 0.0);
        deepest_layer--)
      ; /* nothing */
   root_layer_max = deepest_layer + 1;
   root_depth_max = sum_real_array ((*soil[0]).dlayer, deepest_layer+1);
   
   // Optionally apply maximum root depth.
   if (MaxRootDepth > 0)
      {
      root_depth_max = min(root_depth_max, MaxRootDepth);
      if (root_depth > root_depth_max)
         root_depth = root_depth_max;
      }
   
   dltRootDepth = u_bound ( dltRootDepth, root_depth_max - root_depth);

   if (dltRootDepth < 0.0) throw std::runtime_error("negative root growth??") ;

   Debug("Root.dltRootDepth=%f", dltRootDepth);
   Debug("Root.root_layer_max=%i", root_layer_max);
   Debug("Root.root_depth_max=%f", root_depth_max);
   }

void RootPart::update(void)
//=======================================================================================
// Update Daily State
   {
   SimplePart::update();
   root_depth += dltRootDepth;

   for (int layer = 0; layer < (*soil[0]).num_layers; layer++)
      root_length[layer] += dltRootLength[layer];

   for (int layer = 0; layer < (*soil[0]).num_layers; layer++)
      {
      root_length[layer] -= dltRootLengthSenesced[layer];
      root_length_senesced[layer] += dltRootLengthSenesced[layer];
      }
    // Note that movement and detachment of C is already done, just
    // need to maintain relationship between length and mass
    // Note that this is not entirely accurate.  It links live root
    // weight with root length and so thereafter dead(and detaching)
    // root is assumed to have the same distribution as live roots.
    float dying_fract_plants = plant->population().DyingFractionPlants();
    for (int layer = 0; layer < (*soil[0]).num_layers; layer++)
        {
        dltRootLengthDead[layer] = root_length[layer] * dying_fract_plants;
        root_length[layer] -= dltRootLengthDead[layer];
        root_length_senesced[layer] += dltRootLengthDead[layer];
        }

   bound_check_real_var(scienceAPI, root_depth, 0.0
                        , sum_real_array ((*soil[0]).dlayer, max_layer)
                        , "root_depth");
   Debug("root.RootDepth=%f", root_depth);
   Debug("root.RootLength=%f", sum_real_array(root_length, max_layer));
   Debug("root.RootLengthSenesced=%f", sum_real_array(root_length_senesced, max_layer));
   }

void RootPart::sen_length(void)
//=======================================================================================
//     Calculate root length senescence based upon changes in senesced root
//     biomass and the specific root length.
   {
   setTo (dltRootLengthSenesced, (float) 0.0);
   float senesced_length = Senescing.DM() / sm2smm * specificRootLength;
   root_dist(senesced_length, dltRootLengthSenesced);
   Debug("Root.dltRootLengthSenesced=%f", sum(dltRootLengthSenesced));
   }

void RootPart::root_dist(float root_sum, vector<float> &root_array)           //(INPUT) Material to be distributed
//=========================================================================
//       Distribute root material over profile based upon root
//       length distribution.
   {
   // distribute roots over profile to root_depth
   int deepest_layer = (*soil[0]).find_layer_no (root_depth);
   float root_length_sum = sum_real_array (root_length, deepest_layer+1);
   for (int layer = 0; layer <= deepest_layer; layer++)
      root_array[layer] = (float)(root_sum *
                           divide (root_length[layer], root_length_sum, 0.0));
   }

void RootPart::root_dist_dead(float root_sum, vector<float> &root_array)      //(INPUT) Material to be distributed
//=========================================================================
//       Distribute root material over profile based upon dead root
//       length distribution.
   {
   // distribute roots over profile to root_depth
   int deepest_layer = (*soil[0]).find_layer_no (root_depth);
   float root_length_sum = sum_real_array (root_length_senesced, deepest_layer+1);
   for (int layer = 0; layer <= deepest_layer; layer++)
      root_array[layer] = (float)(root_sum *
                           divide (root_length_senesced[layer], root_length_sum, 0.0));
   }

void RootPart::collectDetachedForResidue(vector<string> &//part_name
                              , vector<float> &//dm_residue
                              , vector<float> &//dm_n
                              , vector<float> &//dm_p
                              , vector<float> &//fract_to_residue
                              )
//=======================================================================================
// Unlike above ground parts, no roots go to surface residue module.
   {
   }

void RootPart::onEndCrop(vector<string> &/*dm_type*/,
                          vector<float> &/*dlt_crop_dm*/,
                          vector<float> &/*dlt_dm_n*/,
                          vector<float> &/*dlt_dm_p*/,
                          vector<float> &/*fraction_to_residue*/)
//=======================================================================================
// Unlike above ground parts, no roots go to surface residue module. Send our DM to FOM pool.
   {
   root_incorp (Green.DM() , Green.N(), Green.P());
   root_incorp_dead (Senesced.DM(), Senesced.N(), Senesced.P());
   //root_incorp_dead (dmDead(), nDead(), pDead());

   Senesced.Clear();
   Green.Clear();
   zeroAllGlobals();
   }

void RootPart::updateOthers(void)
//=======================================================================================
// dispose of detached material from dead & senesced roots into FOM pool
   {
   root_incorp (Detaching.DM(),
                Detaching.N(),
                Detaching.P());
   }


void RootPart::root_incorp (float  dlt_dm_root,                  // (INPUT) root residue dm (g/m^2)
                                 float  dlt_N_root,                   // (INPUT) root residue N (g/m^2)
                                 float  dlt_P_root)                   // (INPUT) root residue P (g/m^2)
   //=======================================================================================
   //       Add root DM, N & P to FOM pool
   {
   if (dlt_dm_root>0.0)
      {
      Soil &mySoil = (*soil[0]);
      vector<float> dlt_dm_incorp(mySoil.num_layers); // root residue (kg/ha)
      vector<float> dlt_N_incorp(mySoil.num_layers);  // root residue N (kg/ha)
      vector<float> dlt_P_incorp(mySoil.num_layers);  // root residue P (kg/ha)

      // DM
      root_dist(dlt_dm_root * gm2kg /sm2ha, dlt_dm_incorp);

      // Nitrogen
      root_dist(dlt_N_root * gm2kg /sm2ha, dlt_N_incorp);

      // Phosporous
      root_dist(dlt_P_root * gm2kg /sm2ha, dlt_P_incorp);

      protocol::FOMLayerType IncorpFOM;
      IncorpFOM.Type = plant->getCropType();
      Debug("Root.IncorpFOM.Type=%s", IncorpFOM.Type.c_str());
      for (unsigned i = 0; i != dlt_dm_incorp.size(); i++)
         {
         protocol::FOMLayerLayerType Layer;
         Layer.FOM.amount = dlt_dm_incorp[i];
         Layer.FOM.N = dlt_N_incorp[i];
         Layer.FOM.P = dlt_P_incorp[i];
         Layer.FOM.C = 0.0;
         Layer.FOM.AshAlk = 0.0;
         Layer.CNR = 0;
         Layer.LabileP = 0;
         IncorpFOM.Layer.push_back(Layer);
         Debug("Root.IncorpFOM.FOM.amount=%f2", Layer.FOM.amount);
         Debug("Root.IncorpFOM.FOM.N=%f", Layer.FOM.N);

         }
      scienceAPI.publish("IncorpFOM", IncorpFOM);
      }
   else
      {
      // no roots to incorporate
      }
   }

void RootPart::root_incorp_dead (float  dlt_dm_root,                  // (INPUT) root residue dm (g/m^2)
                                      float  dlt_N_root,                   // (INPUT) root residue N (g/m^2)
                                      float  dlt_P_root)                   // (INPUT) root residue P (g/m^2)
   //=======================================================================================
   //       Add root DM, N & P to FOM pool
   {
   if (dlt_dm_root>0.0)
      {
      Soil &mySoil = *soil[0];
      vector<float> dlt_dm_incorp(mySoil.num_layers); // root residue (kg/ha)
      vector<float> dlt_N_incorp(mySoil.num_layers);  // root residue N (kg/ha)
      vector<float> dlt_P_incorp(mySoil.num_layers);  // root residue P (kg/ha)

      // DM
      root_dist_dead(dlt_dm_root * gm2kg /sm2ha, dlt_dm_incorp);

      // Nitrogen
      root_dist_dead(dlt_N_root * gm2kg /sm2ha, dlt_N_incorp);

      // Phosporous
      root_dist_dead(dlt_P_root * gm2kg /sm2ha, dlt_P_incorp);

      protocol::FOMLayerType IncorpFOM;
      IncorpFOM.Type = plant->getCropType();
      for (unsigned i = 0; i != dlt_dm_incorp.size(); i++)
         {
         protocol::FOMLayerLayerType Layer;
         Layer.FOM.amount = dlt_dm_incorp[i];
         Layer.FOM.N = dlt_N_incorp[i];
         Layer.FOM.P = dlt_P_incorp[i];
         Layer.CNR = 0;
         Layer.LabileP = 0;
         IncorpFOM.Layer.push_back(Layer);
         }
      scienceAPI.publish("IncorpFOM", IncorpFOM);
      }
   else
      {
      // no roots to incorporate
      }
   }

void RootPart::redistribute(const vector<float> &dlayer_old,        //  old soil profile layers (mm)
                                 const vector<float> &dlayer_new,        //  new soil profile layers (mm)
                                 float root_depth_new)
//==========================================================================
/*  Purpose
*      Map root length density distribution into a new layer structure
*      after reduction is profile depth due to erosion.
*
*  Assumptions
*      That the new profile is shallower and the roots are at the
*      bottom of the old profile.
*
*  Notes
*      Remapping is achieved by first constructing a map of
*      cumulative root length vs depth that is 'squashed'
*      to the new profile depth.
*      The new values of root length per layer can be linearly
*      interpolated back from this shape taking into account
*      the rescaling of the profile.
*
*/
   {
   //  Local Variables
   int layer;                   // simple layer counter
   int nlayr_rt_old;            // No. of layers in old root profile
   int nlayr_rt_new;            // No. of layers in old root profile
   float pro_red_fr;                 // profile reduction fraction
   float cum_root_bottom;            // cum root at bottom of layer (mm/mm2)
   float cum_root_top;               // cum root at top of layer (mm/mm2)
   float layer_top;                  // depth to top of layer (mm)
   float layer_bottom;               // depth to bottom of layer (mm)

   // Implementation Section ----------------------------------
   int maxLayer = max(dlayer_old.size(),dlayer_new.size());
   if (maxLayer <= 0) {throw std::invalid_argument("max_layer <= 0 in crop_root_length_growth1");}
   float *cum_root_length = new float[maxLayer];  //Cum Root length with depth (mm/mm2)
   float *cum_root_depth = new float[maxLayer];   //Cum Root depth (mm)

   // Identify key layers
   // -------------------
   nlayr_rt_old = find_layer_no(root_depth,dlayer_old);
   nlayr_rt_new = find_layer_no(root_depth_new,dlayer_new);

      // calculate the fractional reduction in total profile depth
      // ---------------------------------------------------------
   pro_red_fr = (float)divide (root_depth_new, root_depth, 0.0);

      // build interpolation pairs based on 'squashed' original root profile
      // -------------------------------------------------------------------
   cum_root_depth[0] = 0.0;
   cum_root_length[0] = 0.0;

   for(layer = 0; layer <= nlayr_rt_old; layer++)
      {
      if (layer == nlayr_rt_old)
         {
         cum_root_depth[layer + 1] = root_depth * pro_red_fr;
         }
      else
         {
         cum_root_depth[layer + 1] = cum_root_depth[layer] + dlayer_old[layer] * pro_red_fr;
         }
      cum_root_length[layer + 1] = cum_root_length[layer] + root_length[layer];
      }

   fill_real_array (root_length, 0.0, nlayr_rt_old);

   // look up new root length from interpolation pairs
   // ------------------------------------------------
   for(layer = 0; layer <= nlayr_rt_new; layer++)
      {
      layer_bottom = sum(dlayer_new, layer+1);
      layer_top = layer_bottom - dlayer_new[layer];
      cum_root_top = linear_interp_real (layer_top, cum_root_depth,
                                         cum_root_length, nlayr_rt_old + 1);
      cum_root_bottom = linear_interp_real (layer_bottom, cum_root_depth,
                                         cum_root_length, nlayr_rt_old + 1);
      root_length[layer] = cum_root_bottom - cum_root_top;
      }
   root_depth = root_depth_new;

   //delete dynamic memoy
   delete []cum_root_depth;
   delete []cum_root_length;
   }

void RootPart::get_root_length(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for root length
{
    Soil &mySoil = (*soil[0]);
    system->sendVariable(qd, std::vector<float>(root_length,root_length+mySoil.num_layers));
}

void RootPart::get_rlv(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for Root Length Volume
{
    float rlv[max_layer];
    for (int layer = 0; layer < (*soil[0]).num_layers; layer++)
       {
       rlv[layer] = (float)divide (root_length[layer], (*soil[0]).dlayer[layer], 0.0);
       }
    system->sendVariable(qd, std::vector<float>(rlv,rlv+(*soil[0]).num_layers));
}

void RootPart::get_root_length_senesced(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for dead plant root length
{
    system->sendVariable(qd, std::vector<float>(root_length_senesced, root_length_senesced+(*soil[0]).num_layers));
}

void RootPart::get_no3gsm_uptake_pot(protocol::Component *system, protocol::QueryValueData &qd)                  //FIXME - belongs in rootPart
   {
   system->sendVariable(qd, std::vector<float>((*soil[0]).no3gsm_uptake_pot, (*soil[0]).no3gsm_uptake_pot+(*soil[0]).num_layers));
   }

void RootPart::get_nh4gsm_uptake_pot(protocol::Component *system, protocol::QueryValueData &qd)                  //FIXME - belongs in rootPart
   {
   system->sendVariable(qd, std::vector<float>((*soil[0]).nh4gsm_uptake_pot, (*soil[0]).nh4gsm_uptake_pot+(*soil[0]).num_layers));
   }

void RootPart::checkBounds(void)
//=======================================================================================
// Check for data outside of realistic bounds
   {
   if (root_depth < 0)
     throw std::runtime_error(myName + " depth is negative! (" + ftoa(root_depth,".4") +")");
   for (int layer = 0; layer < (*soil[0]).num_layers; layer++)
      {
      if (root_length[layer] < 0)
         throw std::runtime_error(myName + " length in layer " + itoa(layer+1) + " is negative! (" + ftoa(root_length[layer],".4") +")");
      if (root_length_senesced[layer] < 0)
         throw std::runtime_error(myName + " length dead in layer " + itoa(layer+1) + " is negative! (" + ftoa(root_length[layer],".4") +")");
      }
   }

void RootPart::getOtherVariables()
//=======================================================================================
// Get data from other modules as required
   {
   for (vector<Soil*>::iterator s = soil.begin(); s != soil.end(); s++)
      (*s)->getOtherVariables();
   }


float RootPart::waterUptake (void)
//=======================================================================================
// Return the total daily water uptake from this root system
   {
   return (*soil[0]).waterUptake();
   }


void RootPart::doPlantWaterStress (float sw_demand, SWStress *swStress)
//     ===========================================================
//         Get current water stress factors (0-1)
   {
    swStress->doPlantWaterStress (sw_demand);
   }



float RootPart::wet_root_fr (void)
//=======================================================================================
// Calculate today's oxygen deficit (i.e. water logging) stress factor
   {
   if (root_depth > 0.0)
      {
      vector<float> root_fr;
      rootDist(1.0, root_fr);

      float wet_root_fr = 0.0;
      for (unsigned layer = 0; layer != root_fr.size(); layer++)
         {
         wet_root_fr = wet_root_fr + (*soil[0]).WFPS(layer) * root_fr[layer];
         }
      return wet_root_fr;
      }
   else
      return 0.0;
   }

void RootPart::removeBiomass2(float chop_fr)
//=======================================================================================
// Remove biomass from the root system due to senescence or plant death
   {

   Biomass Dead;
   Dead = Green*rootDieBackFraction * chop_fr;
   // however dead roots have a given N concentration
   Dead.SetN(Dead.DM()*c.n_sen_conc);

   Green = Green - Dead;
   Senesced = Senesced + Dead;

       // do root_length
   vector<float> dltRootLengthDie;
   dltRootLengthDie.clear(); dltRootLengthDie.resize((*soil[0]).num_layers);
   setTo (dltRootLengthDie, (float) 0.0);
   float Die_length = Dead.DM() / sm2smm * specificRootLength;
   root_dist(Die_length, dltRootLengthDie);
   for (int layer = 0; layer < (*soil[0]).num_layers; layer++)
      root_length[layer] -= dltRootLengthDie[layer];

   }



void RootPart::UpdateOtherVariables()
//=======================================================================================
// Update data owned by other modules that has changed due to calculations by this root system
   {
   (*soil[0]).UpdateOtherVariables(uptake_source);
   }

void RootPart::get_sw_uptake(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for Soil Water uptake
{
    float rwu[max_layer];
    for (int layer = 0; layer < (*soil[0]).num_layers; layer++)
        {
        rwu[layer] = fabs((*soil[0]).dlt_sw_dep[layer]);
        }
    system->sendVariable(qd, std::vector<float>(rwu, rwu+(*soil[0]).num_layers));
}


void RootPart::get_sw_supply(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for Total Profile Soil Water Supply
{
    int deepest_layer = (*soil[0]).find_layer_no (root_depth);
    float sw_supply_sum = sum_real_array ((*soil[0]).sw_supply, deepest_layer+1);
    system->sendVariable(qd, sw_supply_sum);
}
void RootPart::get_sw_deficit(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for extractable soil water content of each layer
{
    float sw_deficit[max_layer];
    for (int layer = 0; layer < (*soil[0]).num_layers; layer++)
       {
       sw_deficit[layer] = (*soil[0]).sw_dep[layer] - (*soil[0]).ll_dep[layer];
       }
    system->sendVariable(qd, std::vector<float>(sw_deficit,sw_deficit+(*soil[0]).num_layers));
}
void RootPart::get_sw_supply_layr(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for soil water supply from each layer
{
    system->sendVariable(qd, std::vector<float>((*soil[0]).sw_supply, (*soil[0]).sw_supply+(*soil[0]).num_layers));
}

void RootPart::get_ep(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for plant EP
{
    float sum = 0.0;
    for (int layer = 0; layer < (*soil[0]).num_layers; layer++)
        {
        sum = sum + fabs((*soil[0]).dlt_sw_dep[layer]);
        }
    system->sendVariable(qd, sum);
}

void RootPart::get_esw_layr(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for extractable soil water content of each layer
{
    float esw_layr[max_layer];
    for (int layer = 0; layer < (*soil[0]).num_layers; layer++)
       {
       esw_layr[layer] = l_bound ((*soil[0]).sw_dep[layer] - (*soil[0]).ll_dep[layer], 0.0);
       }
    system->sendVariable(qd, std::vector<float>(esw_layr,esw_layr+(*soil[0]).num_layers));
}
void RootPart::get_no3_uptake(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for no3 uptake from each soil layer
{
    float no3_uptake[max_layer];
    fill_real_array(no3_uptake,0.0, max_layer);
    for (int layer = 0; layer < (*soil[0]).num_layers; layer++) {
       no3_uptake[layer] =  (*soil[0]).dlt_no3gsm[layer] * gm2kg/sm2ha;
    }
    system->sendVariable(qd, std::vector<float>(no3_uptake, no3_uptake+(*soil[0]).num_layers));
}

void RootPart::get_nh4_uptake(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for nh4 uptake from each soil layer
{
    float nh4_uptake[max_layer];
    fill_real_array(nh4_uptake,0.0, max_layer);
    for (int layer = 0; layer <= (*soil[0]).num_layers; layer++) {
       nh4_uptake[layer] =  (*soil[0]).dlt_nh4gsm[layer] * gm2kg/sm2ha;
    }
    system->sendVariable(qd, std::vector<float>(nh4_uptake, nh4_uptake+(*soil[0]).num_layers));
}

void RootPart::get_no3_tot(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for total profile no3 uptake
{
    int deepest_layer = (*soil[0]).find_layer_no (root_depth);
    float no3gsm_tot = sum_real_array ((*soil[0]).no3gsm, deepest_layer+1);
    system->sendVariable(qd, no3gsm_tot);
}

void RootPart::get_ll(protocol::Component *systemInterface, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for crop lower limit (volumetric)
   {
   vector<float> ll;
   for(int layer = 0; layer < (*soil[0]).num_layers; layer++)
      ll.push_back((*soil[0]).ll_dep[layer] / (*soil[0]).dlayer[layer]);
   systemInterface->sendVariable(qd, ll);
   }

void RootPart::get_n_supply_soil(protocol::Component *systemInterface, protocol::QueryValueData &qd)    //FIXME - belongs in rootPart
//=======================================================================================
// Getter function for soil n supply (g/m^2)
   {
   int deepest_layer = (*soil[0]).find_layer_no(root_depth);
   float n_uptake_sum = sum_real_array((*soil[0]).dlt_no3gsm, deepest_layer+1)
                     +  sum_real_array((*soil[0]).dlt_nh4gsm, deepest_layer+1);
   if (n_uptake_sum > 0)
      n_uptake_sum = - n_uptake_sum;
   else if (n_uptake_sum < 0)
      n_uptake_sum = - n_uptake_sum ;
   else
      n_uptake_sum = 0.0;
   systemInterface->sendVariable(qd, n_uptake_sum);
   }

void RootPart::plant_nit_supply()
//=======================================================================================
// Calculate Plant Nitrogen Supply
   {
   (*soil[0]).plant_nit_supply(root_depth, root_length);
   }

void RootPart::doNUptake(float sumNMax, float sumSoilNDemand, float nDemand, float n_fix_pot)
//=======================================================================================
//       Find nitrogen uptake.
    {
    (*soil[0]).doNUptake(uptake_source, crop_type, root_depth, sumNMax, sumSoilNDemand, nDemand, n_fix_pot);
    }

//+  Purpose
//       Plant transpiration and soil water extraction
void RootPart::doWaterUptake (int,  float SWDemand)
    {
    (*soil[0]).doWaterUptake(uptake_source, crop_type, SWDemand, root_depth);
    }

// SWIM


float RootPart::peswTotal()
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   return (*soil[0]).peswTotal(root_depth);
   }

float RootPart::swSupply()
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   return (*soil[0]).swSupply(root_depth);
   }

float RootPart::NSupply()
//=======================================================================================
// Calculate total plant extractable N.
   {
   return (*soil[0]).NSupply(root_depth);
   }
float RootPart::swAvailablePotential()
//=======================================================================================
// Calculate total plant potential extractable soil water.
   {
   int deepest_layer = find_layer_no (root_depth, (*soil[0]).dlayer, max_layer);
   return sum_real_array ((*soil[0]).sw_avail_pot, deepest_layer+1);
   }


float RootPart::nUptake()
//=======================================================================================
//
   {
   float nUptakeSum = 0.0;
   if (soil.size() > 0)
   {
     int deepest_layer = find_layer_no (root_depth, (*soil[0]).dlayer, max_layer);
     nUptakeSum = - sum_real_array ((*soil[0]).dlt_no3gsm, deepest_layer+1)
                        - sum_real_array ((*soil[0]).dlt_nh4gsm, deepest_layer+1);
   }
   return nUptakeSum;
   }



void RootPart::rootDist(float root_sum, vector<float>& rootArray)
//=========================================================================
// Distribute root material over profile based upon root
// length distribution.
   {
   int deepest_layer = find_layer_no (root_depth, (*soil[0]).dlayer, max_layer);

   float root_length_sum = sum_real_array (root_length, deepest_layer+1);

   for (int layer = 0; layer <= deepest_layer; layer++)
      rootArray.push_back((float)(root_sum * divide (root_length[layer], root_length_sum, 0.0)));
   }


float RootPart::pesw(int depth)
//=======================================================================================
// Calculate plant extractable soil water at the given depth.
   {
   return (*soil[0]).pesw(depth);
   }

float RootPart::fasw(int depth)
   {
   return (*soil[0]).fasw(depth);
   }
float RootPart::swAvailable()
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   return (*soil[0]).swAvailable(root_depth);
   }
float RootPart::sw_avail_ratio(int layer)  //(INPUT) soil profile layer number
//===========================================================================
//     Get the soil water availability factor in a layer.  For a layer,
//     it is 1.0 unless the plant-extractable soil water declines
//     below a fraction of plant-extractable soil water capacity for
//     that layer.
   {
   //  Local Variables
   float pesw;                // plant extractable soil-water (mm/mm)
   float pesw_capacity;       // plant extractable soil-water capacity (mm/mm)
   float sw_avail_ratio;      // soil water availability ratio (0-1)

   pesw = (*soil[0]).sw_dep[layer] - (*soil[0]).ll_dep[layer];
   pesw_capacity = (*soil[0]).dul_dep[layer] - (*soil[0]).ll_dep[layer];
   sw_avail_ratio = (float)divide (pesw, pesw_capacity, 10.0);
   return sw_avail_ratio;
   }
