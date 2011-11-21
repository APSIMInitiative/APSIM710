#include "StdPlant.h"

#include "Plant.h"
#include "CompositePart.h"
#include "Phenology/Phenology.h"
#include "Leaf/Leaf.h"
#include "Reproductive/FloretPart.h"
#include "Reproductive/PlantFruitCohorting.h"
#include "Stem.h"
#include "Leaf/Leaf.h"
#include "Reproductive/PodPart.h"
#include "Reproductive/MealPart.h"
#include "Reproductive/OilPart.h"
#include "Root/RootPart.h"
#include "Storage/StoragePart.h"
#include "Utility/Observers.h"
#include "Arbitrators/Arbitrator.h"
#include "Utility/PlantUtility.h"
#include "Fixation.h"
#include "Parts.h"

using namespace std;

Plant *currentInstance = NULL;

Plant::Plant(protocol::Component *P, ScienceAPI& api, XMLNode parameters)
   : parent(P),
     scienceAPI(api),
     plant(findNode(parameters, "Options"), scienceAPI, this, ""),
     tops(NULL),
     grain(NULL),
     plantSpatial(scienceAPI)
   {
   // --------------------------------------------------------------------------
   // Constructor
   // --------------------------------------------------------------------------

   _environment  = plant.findByType<Environment*>("environment");
   _population   = plant.findByType<Population*>("population");
   _phenology    = plant.findByType<Phenology*>("phenology");
   _root         = plant.findByType<RootBase*>("root");
   _fixation     = plant.findByTypeOptional<Fixation*>();
   _arbitrator   = plant.findByType<Arbitrator*>("arbitrator");
   _leaf         = plant.findByType<Leaf*>("leaf");
   _stem         = plant.findByType<Stem*>("stem");

   // DPH: Need to sort this fruit stuff out.
   // We need a common base class for all fruit objects.
   _fruit     = plant.findByTypeOptional<FloretPart*>();
   if (_fruit == NULL)
      _fruit     = plant.findByTypeOptional<PlantFruitCohorting*>();
   if (_fruit == NULL)
      _fruit     = plant.findByType<CompositePart*>("head");

    tops = &(dynamic_cast<CompositePart&>(plant.findThing("tops")));
    grain = &(dynamic_cast<CompositePart&>(plant.findThing("grain1")));

    nStress = new NStress(scienceAPI, parent);
    pStress = new PStress(scienceAPI, parent);
    swStress = new SWStress(scienceAPI, parent);
    tempStress = new TempStress(scienceAPI, parent);
    co2Modifier = new Co2Modifier(scienceAPI, *this);

    g.cswd_pheno.setup(&swStress->swDef.pheno);
    g.cswd_photo.setup(&swStress->swDef.photo);
    g.cnd_grain_conc.setup(&nStress->nFact.grain);
    g.cnd_photo.setup(&nStress->nFact.photo);
    g.cswd_expansion.setup(&swStress->swDef.expansion);

    stageObservers.addObserver(&g.cswd_photo);
    stageObservers.addObserver(&g.cswd_expansion);
    stageObservers.addObserver(&g.cnd_grain_conc);
    stageObservers.addObserver(&g.cnd_photo);
    otherObservers.addObserver(&g.cswd_pheno);

    plantSpatial.init(this);
    }

Plant::~Plant(void)
   {
     delete nStress;
     delete pStress;
     delete swStress;
     delete tempStress;
     delete co2Modifier;
   }

void Plant::onInit1(void)
   {
   // --------------------------------------------------------------------------
   // Init1. Set up plant structure
   // --------------------------------------------------------------------------
   plant_zero_variables ();    // Zero global states
   plant_zero_all_globals();

    sowingEventObserver = new eventObserver(scienceAPI, "sowing", this);
    plant.addThing(sowingEventObserver);

    emergenceEventObserver = new eventObserver(scienceAPI, "emergence", this);
    plant.addThing(emergenceEventObserver);

    FIEventObserver = new eventObserver(scienceAPI, "floral_initiation", this);
    plant.addThing(FIEventObserver);

    floweringEventObserver = new eventObserver(scienceAPI, "flowering", this);
    plant.addThing(floweringEventObserver);

    maturityEventObserver = new eventObserver(scienceAPI, "maturity", this);
    plant.addThing(maturityEventObserver);

   scienceAPI.subscribe("prepare",    NullFunction(&Plant::onPrepare));
   scienceAPI.subscribe("process",    NullFunction(&Plant::onProcess));
   scienceAPI.subscribe("sow",        SowFunction(&Plant::onSow));
   scienceAPI.subscribe("harvest",    HarvestFunction(&Plant::onHarvest));
   scienceAPI.subscribe("end_crop",   NullFunction(&Plant::onEndCrop));
   scienceAPI.subscribe("end_run",    NullFunction(&Plant::onEndRun));
   scienceAPI.subscribe("kill_stem",  KillStemFunction(&Plant::onKillStem));
   scienceAPI.subscribe("remove_crop_biomass",    RemoveCropBiomassFunction(&Plant::onRemoveCropBiomass));
   scienceAPI.subscribe("detach_crop_biomass_rate", FloatFunction(&Plant::onDetachCropBiomass));

   // Send My Variable

   setupGetFunction(parent, "plant_status", protocol::DTstring, false,
                     &Plant::get_plant_status, "", "Plant Status");

   scienceAPI.expose("crop_type", "", "Crop Type", c.crop_type);

   parent->addGettableVar("crop_class",
                          g.crop_class, "", "Plant crop class");

   setupGetFunction(parent, "height", protocol::DTsingle, false,
                     &Plant::get_height, "mm", "Height of crop");

   setupGetFunction(parent, "width", protocol::DTsingle, false,
                     &Plant::get_width, "mm", "canopy row width");

   setupGetFunction(parent, "cover_tot", protocol::DTsingle, false,
                     &Plant::get_cover_tot, "", "Total cover");


   setupGetFunction(parent, "effective_rue", protocol::DTsingle, false,
                    &Plant::get_effective_rue, "g/m2/MJ", "EffectiveRUE");
   setupGetFunction(parent, "respiration", protocol::DTsingle, false,
                    &Plant::get_respiration, "g/m2", "Whole Plant Respiration");

   setupGetFunction(parent, "biomass", protocol::DTsingle, false,
                    &Plant::get_biomass, "kg/ha", "Biomass");

   setupGetFunction(parent, "biomass_wt", protocol::DTsingle, false,
                    &Plant::get_biomass_wt, "g/m^2", "Biomass weight");

   setupGetFunction(parent, "green_biomass", protocol::DTsingle, false,
                    &Plant::get_green_biomass, "kg/ha", "Green Biomass weight");

   setupGetFunction(parent, "green_biomass_wt", protocol::DTsingle, false,
                    &Plant::get_green_biomass_wt, "g/m^2", "Green Biomass weight");

   setupGetFunction(parent, "stover_wt", protocol::DTsingle, false,
                    &Plant::get_stover_biomass_wt, "g/m^2", "Stover Biomass weight");

   setupGetFunction(parent, "dm_plant_min", protocol::DTsingle, true,
                    &Plant::get_dm_plant_min, "g/m^2", "Minimum weights");

   setupGetFunction(parent, "dlt_dm_green_retrans", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_green_retrans, "g/m^2", "change in green pool from retranslocation");

   setupGetFunction(parent, "biomass_n", protocol::DTsingle, false,
                    &Plant::get_biomass_n,  "g/m^2", "N in total biomass");

   setupGetFunction(parent, "n_uptake", protocol::DTsingle, false,
                    &Plant::get_n_uptake, "g/m^2", "N uptake");

   setupGetFunction(parent, "green_biomass_n", protocol::DTsingle, false,
                    &Plant::get_green_biomass_n, "g/m^2", "N in green biomass");

   setupGetFunction(parent, "transp_eff", protocol::DTsingle, false,
                    &Plant::get_transp_eff,
                    "g/m2/mm", "Transpiration Efficiency");

   setupGetFunction(parent, "n_conc_stover", protocol::DTsingle, false,
                    &Plant::get_n_conc_stover, "%", "N concentration in stover");

   setupGetFunction(parent, "n_conc_crit", protocol::DTsingle, false,
                    &Plant::get_n_conc_crit, "%", "critical N content");

   setupGetFunction(parent, "n_conc_min", protocol::DTsingle, false,
                    &Plant::get_n_conc_min, "%", "minimum N content");

   setupGetFunction(parent, "n_uptake_stover", protocol::DTsingle, false,
                    &Plant::get_n_uptake_stover,
                    "g/m^2", "N taken up by agp");


   setupGetFunction(parent, "n_demanded", protocol::DTsingle, true,
                    &Plant::get_n_demanded,
                    "g/m^2", "N demanded");

   parent->addGettableVar("dlt_n_fixed_pot",
               g.n_fix_pot, "g/m^2", "potential N fixation");

   parent->addGettableVar("dlt_n_fixed",
               g.n_fix_uptake, "g/m^2", "N fixation");

   parent->addGettableVar("n_fixed_tops",
               g.n_fixed_tops, "g/m^2", "N fixation");

   parent->addGettableVar("remove_biom_pheno",
               g.remove_biom_pheno, "", "biomass removal factor for phenology");

   setupGetFunction(parent, "nfact_grain_tot", protocol::DTsingle, false,
                    &Plant::get_nfact_grain_tot,
                    "", "Summed grain N factor for current stage");

   setupGetFunction(parent, "no3_demand", protocol::DTsingle, false,
                    &Plant::get_no3_demand,
                    "kg/ha", "Demand for NO3");

   setupGetFunction(parent, "parasite_dm_supply", protocol::DTsingle, false,
                     &Plant::get_parasite_c_gain,
                     "g/m^2", "Assimilate to parasite");


   setupGetFunction(parent, "p_demand", protocol::DTsingle, false,
                    &Plant::get_p_demand,
                    "g/m^2","");

   setupGetFunction(parent, "p_demand_parts", protocol::DTsingle, true,
                    &Plant::get_p_demand_parts,
                    "g/m^2","");

   setupGetFunction(parent, "biomass_p", protocol::DTsingle, false,
                    &Plant::get_biomass_p,
                    "g/m^2","P in biomass");

   setupGetFunction(parent, "p_uptake", protocol::DTsingle, false,
                    &Plant::get_biomass_p,
                    "g/m^2","P  uptake");

   setupGetFunction(parent, "green_biomass_p", protocol::DTsingle, false,
                    &Plant::get_green_biomass_p,
                    "g/m^2","P in green biomass");


   setupGetFunction(parent, "dlt_p_retrans", protocol::DTsingle, true,
                    &Plant::get_dlt_p_retrans,
                    "g/m^2","dlt P parts");


   setupGetFunction(parent, "p_conc_stover", protocol::DTsingle, false,
                    &Plant::get_p_conc_stover,
                    "%","P in stover");

   setupGetFunction(parent, "p_uptake_stover", protocol::DTsingle, false,
                    &Plant::get_p_uptake_stover,
                    "%","P in stover");

#undef setupGetVar
#undef setupGetFunction

   scienceAPI.exposeWritable("crop_class", "", "Crop class", StringSetter(&Plant::onSetCropClass));

   plant.tempFlagToShortCircuitInit1 = true;
   plant.onInit1(parent);

   nStress->init();
   swStress->init(_root);
   tempStress->init();
   co2Modifier->init();
   }

void Plant::onInit2(void)
   {
   // --------------------------------------------------------------------------
   // Init2. The rest of the system is here now..
   // --------------------------------------------------------------------------

   pStress->init();
   const char*  section_name = "constants" ;
   scienceAPI.readOptional("crop_type", c.crop_type);
   plant.readConstants(parent, section_name);

   read();
   plant_zero_variables (); // Zero global states

   g.plant_status = out;
   g.module_name = parent->getName();
   doPInit();

   plant_get_other_variables (); // sw etc..

   protocol::NewCropType NewCrop;
   NewCrop.crop_type = c.crop_type;
   NewCrop.sender = Name();
   scienceAPI.publish ("newcrop",NewCrop);
   }

void Plant::doPlantEvent(const string& oldStageName,
                         const string& newStageName,
                         bool phenologyRewound)
   {
   // --------------------------------------------------------------------------
   // Called by Phenology when the phase changes.
   // --------------------------------------------------------------------------
   plant.onPlantEvent(newStageName);
   if (phenologyRewound)
      plant_event(newStageName);
   else
      {
      phenologyEventYesterday = oldStageName;
      phenologyEventToday = newStageName;
      phenologyRewoundToday = phenologyRewound;
      }
   }

void Plant::onPrepare()
   {
   // --------------------------------------------------------------------------
   // Event Handler for Prepare Event
   // --------------------------------------------------------------------------
   plant_zero_daily_variables ();
   plant_get_other_variables ();        // request and receive variables from owner-modules
   if (g.plant_status == out)
      {
      plant_zero_variables ();
      UpdateCanopy();
      }
   else
      {
      plant.prepare();

      nStress->doPlantNStress (&leaf(), &stem());
      tempStress->doPlantTempStress (environment());
      plant.doRadnPartition();

      // Calculate Potential Photosynthesis
      plant.doDmPotRUE();

      // Calculate Plant Water Demand
      float SWDemandMaxFactor = p.eo_crop_factor * g.eo ;
      plant.doSWDemand(SWDemandMaxFactor);
      doNDemandEstimate(1);

      protocol::NewPotentialGrowthType NewPotentialGrowth;
      NewPotentialGrowth.frgr = min(min(getTempStressPhoto(),getNfactPhoto()),min(getOxdefPhoto(),getPfactPhoto()));
      NewPotentialGrowth.sender = Name();
      scienceAPI.publish ("newpotentialgrowth",NewPotentialGrowth);


      // Note actually should send total plant
      // potential growth rather than just tops - NIH
      prepare_p();
      }
   }

void Plant::onProcess()
   {
   // --------------------------------------------------------------------------
   // Event Handler for Process Event
   // --------------------------------------------------------------------------
   if (g.plant_status != out)
      {
      plant_get_other_variables ();   // request and receive variables from owner-modules

      root().plant_root_depth ();
      if (g.plant_status == alive)
         {
         environment().process();
         root().doWaterUptake(1, Tops().SWDemand());
         root().doPlantWaterStress (Tops().SWDemand(), swStress);
         root().plant_nit_supply();

         phenology().process();
         fruit().process();

         plant.morphology();

         leaf().CanopyExpansion(c.leaf_no_pot_option,
                            min(pow(min(nStress->nFact.expansion, pStress->pFact.expansion),2),swStress->swDef.expansion),
                            phenology().TT(),min(swStress->swDef.expansion, min(nStress->nFact.expansion, pStress->pFact.expansion)));

         // Calculate Potential Photosynthesis
         plant.doDmPotRUE();               // NIH - WHY IS THIS HERE!!!!?????  Not needed I hope.

         arbitrator().partitionDM();

         arbitrator().doDmRetranslocate(&stem(), &leaf(), &fruit());

         //Combine these 3 calls into a AreaOrLengthGrowth call
         //Note this will fix a bug - the floret area dlt is not currently calculated!!!
         leaf().actual ();
         fruit().calcDlt_pod_area ();
         root().root_length_growth();

         leaf().leaf_death( min(nStress->nFact.expansion, pStress->pFact.expansion), phenology().TT());
         leaf().leaf_area_sen( swStress->swDef.photo);

         plant.doSenescence(leaf().senFract());
         plant.doMaintenanceRespirationPFR ();    // Plant and Food implementation FIXME-EIT
         root().sen_length();

         plant.doNDemandGrain(nStress->nFact.grain, swStress->swDef.expansion);

         float biomass = Tops().Green.DM() + plant.DMSupply();
//         root().plant_nit_supply();
         if (_fixation!=NULL)
           g.n_fix_pot = _fixation->Potential(biomass, swStress->swDef.fixation);
         else
           g.n_fix_pot = 0.0;

         if (c.n_retrans_option==1)
            {
            // this option requires retrans to happen before working out n demand from soil
            // NOTE: two processes are linked.
            arbitrator().doNRetranslocate(&fruit());
            doNDemand (c.n_retrans_option);
            }
         else
            doNDemand (c.n_retrans_option);

         doNSenescence (c.n_senescence_option);
         plant.doSoilNDemand ();
         float PotNFix;
         if (_fixation != NULL)
            PotNFix = _fixation->NFixPot();
         else
            PotNFix = 0.0;
         root().doNUptake(plant.nMax(), plant.soilNDemand(), plant.nDemand(), PotNFix);     // allows preference of N source
         arbitrator().doNPartition(g.n_fix_pot, plant.nDemand(), g.n_fix_uptake);
         doPPartition();

         if (c.n_retrans_option==2)  // this option requires soil uptake to satisfy grain n before retranslocation
            arbitrator().doNRetranslocate(&fruit());

         doPRetranslocate();
         population().PlantDeath();
         }
      plant.Detachment();
      plant_cleanup();

      swStress->doPlantWaterStress (Tops().SWDemand());
      nStress->doPlantNStress (&leaf(), &stem());

      // See if we need to output a phenological stage report because
      // we've entered a new phase today.
      if (phenologyEventToday != "")
         {
         plant_event(phenologyEventToday);
         if (phenologyRewoundToday)
            {
            stageObservers.reset();
            otherObservers.reset();
            }
         phenologyEventToday = "";
         phenologyRewoundToday = false;
         }
      plant_update_other_variables ();
      root().UpdateOtherVariables();
      }
   }

void Plant::onSow(protocol::SowType& Sow)
   {
   //=======================================================================================
   // Event Handler for Sowing Event
   plant_get_other_variables (); // request and receive variables from owner-modules
   plant_start_crop(Sow);    // start crop and do  more initialisations
   }

void Plant::onHarvest(protocol::HarvestType &Harvest)
   {
   // =======================================================================================
   // Event Handler for a Harvest Event
   string  report_flag;

   if (g.plant_status != out)
      {
      // crop harvested. Report status
      report_flag = Harvest.Report;
      if (report_flag == "" || report_flag == "yes")
         plant_harvest_report();
      plant_auto_class_change("harvest");
      plant_harvest_update(Harvest);
      }
    else
      scienceAPI.warning(g.module_name + " is not in the ground - unable to harvest.");
   }

void Plant::onEndCrop()
//=======================================================================================
// Event Handler for End of Crop Event
  {
  plant_end_crop ();            //end crop - turn into residue
  }


void Plant::onKillStem(protocol::KillStemType &KillStem)
//=======================================================================================
// Event Handler for a Kill Stem Event
   {
   if (g.plant_status != out)
      {
      plant_auto_class_change("kill_stem");

      // determine the new stem density
      // ==============================
      //float temp;
      if (KillStem.Plants != 0)
        population().SetPlants((float)KillStem.Plants);

      // Update biomass and N pools.
      plant.onKillStem();

      // now update new canopy covers
      plantSpatial.setPlants(population().Density());
      plantSpatial.setCanopyWidth(leaf().width());
      plant.doCover(plantSpatial);
      UpdateCanopy();

      plant.doNConccentrationLimits( co2Modifier->n_conc() )  ;                  // plant N concentr
      }
   else
      {
      char msg[500];
      sprintf(msg, "%s%s%s"
       ,g.module_name.c_str()
       , " is not in the ground -"
       , " unable to kill stem.");
      scienceAPI.warning(msg);
      }
   }

void Plant::onEndRun()
//=======================================================================================
// Event Handler for the end of run event
   {
   plant_zero_variables ();
   }

void Plant::onRemoveCropBiomass(protocol::RemoveCropBiomassType& dmRemoved)
//=======================================================================================
// Event Handler for a RemoveCropBiomass Event
   {
   if (c.remove_biomass_report)
      {
      ostringstream msg;
      msg << "Remove Crop Biomass:-" << endl;
      float dmTotal = 0.0;

      for (unsigned int pool=0; pool < dmRemoved.dm.size(); pool++)
         {
         for (unsigned int part = 0; part < dmRemoved.dm[pool].part.size(); part++)
            {
            msg << "   dm " << dmRemoved.dm[pool].pool << " " << dmRemoved.dm[pool].part[part] << " = " << dmRemoved.dm[pool].dlt[part] << " (g/m2)" << endl;
            dmTotal +=  (float)dmRemoved.dm[pool].dlt[part];
            }
         }
      msg << endl << "   dm total = " << dmTotal << " (g/m2)" << endl << ends;
      scienceAPI.write(msg.str());
      }

   plant_remove_biomass_update(dmRemoved);
   }

void Plant::onDetachCropBiomass(float detachRate)
   {
   //=======================================================================================
   // Event Handler for a DetachCropBiomass Event
   protocol::RemoveCropBiomassType dmRemoved;
   protocol::RemoveCropBiomassdmType dm;

   dm.pool = "green";

   vector<float>  dmParts;
   Tops().get_name(dm.part);
   Tops().get_dm_senesced(dmParts);

   for (unsigned int pool=0; pool < dmParts.size(); pool++)
      dm.dlt.push_back(double(dmParts[pool] * 0.0));

   dmRemoved.dm.push_back(dm);

   dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
   dm.part.erase(dm.part.begin(), dm.part.end());
   dmParts.clear();

   dm.pool = "senesced";

   Tops().get_name(dm.part);
   Tops().get_dm_senesced(dmParts);

   for (unsigned int pool=0; pool < dmParts.size(); pool++)
      dm.dlt.push_back(double(dmParts[pool] * detachRate));

   dmRemoved.dm.push_back(dm);

   dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
   dm.part.erase(dm.part.begin(), dm.part.end());
   dmParts.clear();

   if (c.remove_biomass_report)
      {
      ostringstream msg;
      msg << "Detach Crop Biomass:-" << endl;
      float dmTotal = 0.0;

      for (unsigned int pool=0; pool < dmRemoved.dm.size(); pool++)
         {
         for (unsigned int part = 0; part < dmRemoved.dm[pool].part.size(); part++)
            {
            msg << "   dm " << dmRemoved.dm[pool].pool << " " << dmRemoved.dm[pool].part[part] << " = " << dmRemoved.dm[pool].dlt[part] << " (g/m2)" << endl;
            dmTotal += (float)dmRemoved.dm[pool].dlt[part];
            }
          }
      msg << endl << "   dm total = " << dmTotal << " (g/m2)" << endl << ends;

      parent->writeString (msg.str().c_str());
      }
   plant_remove_biomass_update(dmRemoved);
   }

void Plant::onAction(const string& eventName)
   {
   //=======================================================================================
   // Change crop class due to given event - usually from manager.
   plant_auto_class_change(eventName.c_str());
   }

void Plant::onSetCropClass(const string& crop_class)
   {
   //=======================================================================================
   // Someone is changing our class - usually via manager.
   g.crop_class = crop_class;
   scienceAPI.setClass2(g.crop_class);
   read();
   }

void Plant::doNDemand (int option /* (INPUT) option number*/)
//=======================================================================================
//       Find nitrogen demand.
    {
    if (option == 1)
        plant.doNDemand1(Tops().DMSupply(), Tops().dltDmPotRue());        //FIXME Should be able to do away with the arguments someday

    else if (option == 2)
         plant.doNDemand2(Tops().DMSupply(), Tops().dltDmPotRue());      //FIXME Should be able to do away with the arguments someday

    else
        throw std::invalid_argument ("invalid n demand option");
    }

void Plant::doNDemandEstimate (int option)
//=======================================================================================
//      Calculate an approximate nitrogen demand for today's growth.
//      The estimate basically = n to fill the plant up to maximum
//      nitrogen concentration.
    {
    if (option == 1)
        {
        // Option 1 is to assume that the distribution of plant
        // C will be similar after today and so N demand is that
        // required to raise all plant parts to max N conc.

        float dlt_dm_pot_rue = plant.dltDmPotRue();
        plant.doNDemand1Pot(dlt_dm_pot_rue, dlt_dm_pot_rue);

        g.ext_n_demand = plant.nDemand();

        //nh  use zero growth value here so that estimated n fix is always <= actual;
        float n_fix_pot = phenology().doLookup(c.n_fix_rate) * Tops().Green.DM() * swStress->swDef.fixation;

        if (Str_i_Eq(c.n_supply_preference,"active"))
            {
            // Nothing extra to do here
            }
        else if (Str_i_Eq(c.n_supply_preference,"fixation"))
            {
            // Remove potential fixation from demand term
            g.ext_n_demand = g.ext_n_demand - n_fix_pot;
            g.ext_n_demand = l_bound(g.ext_n_demand, 0.0);
            }
        else
            {
            throw std::invalid_argument ("bad n supply preference");
            }
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }
    }

void Plant::doNSenescence (int   option/*(INPUT) option number*/)
//=======================================================================================
//       Simulate plant nitrogen senescence.
    {
    if (option == 1)
        {
        plant.doNSenescence();
        }
    else if (option == 2)
        {
        plant.doNSenescence();

        //! now get N to retranslocate out of senescing leaves
        arbitrator().doNSenescedRetrans(&leaf());
        }
    else
        {
        throw std::invalid_argument ("invalid sen nit option");
        }

    if (pStress->isPhosphorusAware())
       plant.doPSenescence();
    }

void Plant::plant_cleanup (void)
    {
   // --------------------------------------------------------------------------
   // cleanup after crop processes
   // --------------------------------------------------------------------------

    g.remove_biom_pheno = 1.0;

    plant_update();

    bound_check_real_var(scienceAPI, plant.coverGreen(), 0.0, 1.0, "cover_green");
    bound_check_real_var(scienceAPI, plant.coverSen(), 0.0, 1.0, "cover_sen");

    plant.checkBounds();

    if (phenology().onDayOf("sowing"))
        g.n_fixed_tops = (float)(Tops().Growth.N() * divide (g.n_fix_uptake,plant.Growth.N(),0.0));

    else
        g.n_fixed_tops = (float)(g.n_fixed_tops + Tops().Growth.N() * divide (g.n_fix_uptake, plant.Growth.N() ,0.0));
    g.lai_max = max(g.lai_max, leaf().getLAI());             //FIXME - should be returned from leafPart method

    if (g.plant_status == alive && phenologyEventToday != "")
        {
        if (phenology().inPhase("stress_reporting"))
           {
            char msg[1024];
            sprintf (msg,"%4s%-20s%s%-23s%6.3f%13.3f%13.3f%13.3f\n", " ",
                      phenologyEventYesterday.c_str(),
                      " to ",
                      phenologyEventToday.c_str(),
                      g.cswd_photo.getAverage(),
                      g.cswd_expansion.getAverage(),
                      g.cnd_photo.getAverage(),
                      g.cnd_grain_conc.getAverage());
            g.averageStressMessage += msg;
           }
        }
    }

void Plant::plant_update(void)
   {
   // --------------------------------------------------------------------------
   // Update all plant states
   // --------------------------------------------------------------------------

   // Let me register my surprise at how this is done on the next few lines
   // - why intrinsically limit processes to leaf etc right here!!! - NIH
   float n_senesced_trans = leaf().dltNSenescedTrans();
   leaf().giveNGreen(-1.0f*n_senesced_trans);
   stem().giveNGreen(n_senesced_trans);

   leaf().giveNGreen(-1.0f*plant.dltNSenescedRetrans());
   root().updateOthers();    // send off detached roots before root structure is updated by plant death

   plant.update();

   // now update new canopy covers
   plantSpatial.setPlants(population().Density());
   plantSpatial.setCanopyWidth(stem().width());

   plant.doCover(plantSpatial);

   // plant stress observers
   stageObservers.update();
   if (phenology().inPhase("preflowering")) g.cswd_pheno.update();

   population().Update();
   UpdateCanopy();
   plant.doNConccentrationLimits(co2Modifier->n_conc());
   }

void Plant::plant_event(const std::string& newStageName)
   {
   // --------------------------------------------------------------------------
   // Report occurence of event and the current status of specific variables.
   // Called when a new phase has begun.
   // --------------------------------------------------------------------------

   // Tell the system (ie everything outside of plant) about this event.
   // NB. Don't send an "end_crop" to the system - otherwise all the other crops will stop too!
   if (newStageName != "end_crop")
      scienceAPI.publish(newStageName.c_str());

   if (phenology().inPhase("above_ground"))
      {
      char msg[256];
      sprintf(msg,
         "                biomass =       %8.2f (g/m^2)   lai          = %7.3f (m^2/m^2)\n"
         "                stover N conc = %8.2f (%%)    extractable sw = %7.2f (mm)",
         Tops().Total.DM(),
         leaf().getLAI(),
         divide(Tops().Vegetative.N(), Tops().Vegetative.DM(), 0.0) * fract2pcnt,
         root().peswTotal());
      scienceAPI.write(msg);
      }
   }


void Plant::plant_harvest_update(protocol::HarvestType &Harvest)
   {
   // Update states after a harvest
   float dm_chopped;                             // dry matter added to chopped pool (kg/ha)
   float n_chopped;                              // nitrogen added to chopped pool(kg/ha)
   float p_chopped;                              // phosphorus added to chopped pool(g/m^2)
   float dm_root_chopped;                             // dry matter added to chopped pool(kg/ha)
   float n_root_chopped;                              // nitrogen added to chopped pool(kg/ha)
   float p_root_chopped;                              // phosp added to chopped pool(kg/ha)
   float dm_tops_chopped;                             // dry matter added to chopped pool(kg/ha)
   float n_tops_chopped;                              // nitrogen added to chopped pool(kg/ha)
   float p_tops_chopped;                              // phosp added to chopped pool(kg/ha)

   float dm_residue;                             // dry matter added to residue (kg/ha)
   float n_residue;                              // nitrogen added to residue (kg/ha)
   float p_residue;                              // phosphorus added to residue (g/m^2)
   float dm_root_residue;                             // dry matter added to residue (kg/ha)
   float n_root_residue;                              // nitrogen added to residue (kg/ha)
   float p_root_residue;                              // phosp added to residue (kg/ha)
   float dm_tops_residue;                             // dry matter added to residue (kg/ha)
   float n_tops_residue;                              // nitrogen added to residue (kg/ha)
   float p_tops_residue;                              // phosp added to residue (kg/ha)

   float remove_fr;
   float height;                                 // cutting height
   float temp;

   // Tell the rest of the system we are about to harvest
   scienceAPI.publish("harvesting");

   // determine the new stem density
   // ==============================
   temp = (float)Harvest.Plants;
   if (temp != 0)
       population().SetPlants(temp);

   remove_fr = (float)Harvest.Remove;
   bound_check_real_var(scienceAPI,remove_fr, 0.0, 1.0, "remove");

   // determine the cutting height
   height = (float)Harvest.Height;
   bound_check_real_var(scienceAPI,height, 0.0, 1000.0, "height");

   vector<string> dm_type;
   vector<float>  fraction_to_residue;
   vector<float>  dlt_crop_dm;
   vector<float>  dlt_dm_n;
   vector<float>  dlt_dm_p;

   // Update biomass and N pools.
   // Calculate return of biomass to surface residues
   plant.onHarvest(height, remove_fr,
                        dm_type,
                        dlt_crop_dm,
                        dlt_dm_n,
                        dlt_dm_p,
                        fraction_to_residue);

   if (sum(dlt_crop_dm) > 0.0)
       plant_send_crop_chopped_event (c.crop_type
                                     , dm_type
                                     , dlt_crop_dm
                                     , dlt_dm_n
                                     , dlt_dm_p
                                     , fraction_to_residue);

   dm_residue = 0.0; dm_root_residue = 0.0;
   n_residue = 0.0; n_root_residue = 0.0;
   p_residue = 0.0; p_root_residue = 0.0;
   dm_chopped = 0.0; dm_root_chopped = 0.0;
   n_chopped = 0.0; n_root_chopped = 0.0;
   p_chopped = 0.0; p_root_chopped = 0.0;

   for (unsigned int part=0; part < dm_type.size(); part++)
      {
      dm_chopped += dlt_crop_dm[part];
      n_chopped += dlt_dm_n[part];
      p_chopped += dlt_dm_p[part];
      dm_residue += dlt_crop_dm[part] * fraction_to_residue[part];
      n_residue += dlt_dm_n[part] * fraction_to_residue[part];
      p_residue += dlt_dm_p[part] * fraction_to_residue[part];
      if (dm_type[part] == "root")
         {
         dm_root_residue += dlt_crop_dm[part] * fraction_to_residue[part];
         n_root_residue += dlt_dm_n[part] * fraction_to_residue[part];
         p_root_residue += dlt_dm_p[part] * fraction_to_residue[part];
         dm_root_chopped += dlt_crop_dm[part];
         n_root_chopped += dlt_dm_n[part];
         p_root_chopped += dlt_dm_p[part];
         }
      }

   dm_tops_chopped = dm_chopped - dm_root_chopped;
   n_tops_chopped = n_chopped - n_root_chopped;
   p_tops_chopped = p_chopped - p_root_chopped;

   dm_tops_residue = dm_residue - dm_root_residue;
   n_tops_residue = n_residue - n_root_residue;
   p_tops_residue = p_residue - p_root_residue;

   parent->writeString ("\nCrop harvested.");
   char  msg[400];

   parent->writeString ("    Organic matter from crop:-      Tops to surface residue      Roots to soil FOM");

   sprintf (msg, "%48s%7.1f%24.1f", "DM (kg/ha) =               ", dm_tops_residue, dm_root_residue);
   parent->writeString (msg);

   sprintf (msg, "%48s%8.2f%24.2f", "N  (kg/ha) =               ", n_tops_residue, n_root_residue);
   parent->writeString (msg);
   if (pStress->isPhosphorusAware())
      {
      sprintf (msg, "%48s%7.1f%24.2f",
                                    "P  (kg/ha) =               ", p_tops_residue, p_root_residue);
      parent->writeString (msg);
      }
   parent->writeString (" ");

   float dm_removed_tops = dm_tops_chopped - dm_tops_residue;
   float dm_removed_root = dm_root_chopped - dm_root_residue;
   float n_removed_tops = n_tops_chopped - n_tops_residue;
   float n_removed_root = n_root_chopped - n_root_residue;
   float p_removed_tops = p_tops_chopped - p_tops_residue;
   float p_removed_root = p_root_chopped - p_root_residue;

   parent->writeString ("    Organic matter removed from system:-      From Tops               From Roots");

   sprintf (msg, "%48s%7.1f%24.1f", "DM (kg/ha) =               ", dm_removed_tops, dm_removed_root);
   parent->writeString (msg);

   sprintf (msg, "%48s%8.2f%24.2f", "N  (kg/ha) =               ", n_removed_tops, n_removed_root);
   parent->writeString (msg);
   if (pStress->isPhosphorusAware())
      {
      sprintf (msg, "%48s%7.2f%24.2f",
                                    "P  (kg/ha) =               ", p_removed_tops, p_removed_root);
      parent->writeString (msg);
      }
   parent->writeString (" ");

   // now update new canopy covers

   plantSpatial.setPlants(population().Density());
   plantSpatial.setCanopyWidth(leaf().width());

   plant.doCover(plantSpatial);
   UpdateCanopy();

   // other plant states
   plant.doNConccentrationLimits(co2Modifier->n_conc());
   }

void Plant::plant_remove_biomass_update (protocol::RemoveCropBiomassType dmRemoved)
    {
   // Unpack the DmRemoved structure
   Tops().doRemoveBiomass(dmRemoved, c.remove_biomass_report);

   // Update biomass and N pools.  Different types of plant pools are affected in different ways.
   // Calculate Root Die Back
   float chop_fr_green_leaf = (float)divide(leaf().GreenRemoved.DM(), leaf().Green.DM(), 0.0);

   root().removeBiomass2(chop_fr_green_leaf);
   float biomassGreenTops    = Tops().Green.DM();
   float dmRemovedGreenTops  = Tops().GreenRemoved.DM();
   float dmRemovedTops       = (Tops().GreenRemoved.DM() + Tops().SenescedRemoved.DM()) * gm2kg/sm2ha;
   float nRemovedTops        = (Tops().GreenRemoved.N() + Tops().SenescedRemoved.N()) * gm2kg/sm2ha;

   Tops().removeBiomass();

   if (c.remove_biomass_affects_phenology)
      g.remove_biom_pheno = (float)divide (dmRemovedGreenTops, biomassGreenTops, 0.0);

   if (c.remove_biomass_report)
      {
      parent->writeString ("\nCrop biomass removed.");
      char  msgrmv[400];

      parent->writeString ("    Organic matter removed from system:-      From Tops               From Roots");

      sprintf (msgrmv, "%48s%7.2f%24.2f", "DM (kg/ha) =               ", dmRemovedTops, 0.0);
      parent->writeString (msgrmv);

      sprintf (msgrmv, "%48s%7.2f%24.2f", "N  (kg/ha) =               ", nRemovedTops, 0.0);
      parent->writeString (msgrmv);

      sprintf (msgrmv, "%30s%7.2f", "Remove biomass phenology factor = ", g.remove_biom_pheno);
      parent->writeString (msgrmv);

      parent->writeString (" ");
      }

    stem().removeBiomass2(-1.0); // the values calculated here are overwritten in plantPart::morphology(void)

    // now update new canopy covers
    plantSpatial.setPlants(population().Density());
    plantSpatial.setCanopyWidth(leaf().width());

    plant.doCover(plantSpatial);
    UpdateCanopy();

    if (c.remove_biomass_affects_phenology)
      phenology().onRemoveBiomass(g.remove_biom_pheno);

    plant.doNConccentrationLimits(co2Modifier->n_conc() );
    }

void Plant::plant_zero_all_globals (void)
    {
      g.plant_status_out_today = false;
      g.module_name = "";
      g.crop_class = "";
      g.plant_status = out;
      g.cultivar = "";
      g.pre_dormancy_crop_class = "";
      swStress->swDef = 1.0;
      tempStress->tFact = 1.0;
      g.remove_biom_pheno = 1.0;

      g.eo = 0.0;
      g.dlt_dm_parasite  =  0.0;
      g.dlt_dm_parasite_demand = 0.0;
      g.dlt_sw_parasite_demand = 0.0;

      g.n_fix_pot = 0.0;
      g.n_fix_uptake = 0.0;
      g.n_fixed_tops = 0.0;

      g.lai_max = 0.0;
      g.ext_n_demand = 0.0;

      p.eo_crop_factor = 0.0;

      //       plant Constants
      c.leaf_no_pot_option = 0;


      c.crop_type = "";
      c.remove_biomass_report = false;
      c.remove_biomass_affects_phenology = true;

      c.n_supply_preference = "";

      c.class_action.clear();
      c.class_change.clear();
      c.eo_crop_factor_default = 0.0;

      // parasite
      g.dlt_dm_parasite_demand =  0.0;
      g.dlt_sw_parasite_demand = 0.0;
      g.dm_parasite_retranslocate = 0.0;
      g.dlt_dm_parasite = 0.0;
    }

void Plant::plant_zero_variables (void)
    {
    phenologyEventToday = "";
    phenologyRewoundToday = false;

    plant_zero_daily_variables ();

    g.plant_status_out_today = false;

    plant.zeroAllGlobals();

    plantSpatial.zeroAllGlobals();

    g.lai_max               = 0.0;


    swStress->swDef = 1.0;
    nStress->nFact = 1.0;
    pStress->pFact = 1.0;

//    g.remove_biom_pheno = 1.0;

    g.n_fix_pot = 0.0;
    g.n_fix_uptake = 0.0;
    g.n_fixed_tops = 0.0;

    g.dm_parasite_retranslocate   = 0.0;
    }

void Plant::plant_zero_daily_variables (void)
   {
   plant.zeroDeltas();
   g.ext_n_demand = 0.0;
   }

//+  Purpose
//       Start crop using parameters specified in passed record
void Plant::plant_start_crop(protocol::SowType& Sow)
    {

//+  Local Variables
    char  msg[200];                               // output string
    string  dummy;                               // dummy variable

//- Implementation Section ----------------------------------

    if (g.plant_status == out)
    {
         if (g.plant_status_out_today == false)
         {
           parent->writeString ( "Crop Sow");

           // get species parameters
           if (Sow.crop_class == "")
               {
               // crop class was not specified
               scienceAPI.read("default_crop_class", c.default_crop_class);
               g.crop_class = c.default_crop_class;
               scienceAPI.setClass2(c.default_crop_class);
               }
           else
               {
               g.crop_class = Sow.crop_class;
               scienceAPI.setClass2(Sow.crop_class);
               }

           // get cultivar parameters
           if (Sow.Cultivar == "")
               {
               throw std::invalid_argument("Cultivar not specified");
               }
           else
               {
               g.cultivar = Sow.Cultivar;
               scienceAPI.setClass1(Sow.Cultivar);
               }

           read();

           // get other sowing criteria
//           float temp;
           if (Sow.plants == 0)
               {
               throw std::invalid_argument("plant density ('plants') not specified");
               }
           population().SetPlants((float)Sow.plants);

           parent->writeString ("   ------------------------------------------------");
           sprintf (msg, "   %s%s",  "cultivar                   = ", g.cultivar.c_str());
           parent->writeString (msg);
           phenology().writeSummary();
           plant.writeCultivarInfo(parent);
           parent->writeString ("   ------------------------------------------------\n\n");

           root().write();

           sprintf (msg, "%s%5.1f%s"
              ,"    Crop factor for bounding water use is set to "
              , p.eo_crop_factor
                , " times eo.");
           parent->writeString (msg);

           plantSpatial.startCrop(Sow);
           phenology().onSow(Sow);
           UpdateCanopy();

           // Bang.
           g.plant_status = alive;
           plant.onPlantEvent("sowing");

           parent->writeString ("");
           parent->writeString ("                 Crop Sowing Data");
           parent->writeString ("    ------------------------------------------------");
           parent->writeString ("    Sowing  Depth Plants Spacing Skip  Skip  Cultivar");
           parent->writeString ("    Day no   mm     m^2     mm   row   plant name");
           parent->writeString ("    ------------------------------------------------");

           sprintf(msg, "   %7d%7.1f%7.1f%7.1f%6.1f%6.1f %s"
                  , environment().dayOfYear(), plantSpatial.sowing_depth
                  , population().Density(), plantSpatial.row_spacing
                  , plantSpatial.skip_row, plantSpatial.skip_plant, g.cultivar.c_str());
           parent->writeString (msg);

           parent->writeString ("    ------------------------------------------------\n");

         }

         else
         {
            ostringstream msg;
            msg << g.module_name << " was taken out today by \"end_crop\" action -" << endl;
            msg << " Unable to accept \"sow\" action until the next day." << endl << ends;
            throw std::runtime_error (msg.str());
         }
    }
    else
    {
         ostringstream msg;
         msg << g.module_name << " is still in the ground -" << endl;
         msg << " Unable to sow until it is taken out by \"end_crop\" action." << endl << ends;
         throw std::runtime_error (msg.str().c_str());
    }

    }

void Plant::read(void)
   {
   plant_read_species_const ();
   plant.readCultivarParameters(parent, g.cultivar);

   if (!scienceAPI.readOptional("eo_crop_factor", p.eo_crop_factor, 0.0f, 100.0f))
      p.eo_crop_factor = c.eo_crop_factor_default;

   string s;   
   c.remove_biomass_report = false;
   scienceAPI.readOptional("remove_biomass_report", s);
   if (s == "on")
      c.remove_biomass_report = true;

   c.remove_biomass_affects_phenology = true;
   scienceAPI.readOptional("remove_biomass_affects_phenology", s);
   if (s == "off")
      c.remove_biomass_affects_phenology = false;

   phenology().read();
   root().read();
   }

void Plant::plant_end_crop (void)
    {
    float dm_residue;                             // dry matter added to residue (g/m^2)
    float n_residue;                              // nitrogen added to residue (g/m^2)
    float p_residue;                              // phosphorus added to residue (g/m^2)
    float dm_root;                                // dry matter added to soil (g/m^2)
    float n_root;                                 // nitrogen added to soil (g/m^2)
    float p_root;                                 // phosphorus added to soil (g/m^2)
    char  msg[400];
    float yield;                                  // grain wt (kg/ha)

    if (g.plant_status != out)
        {
        g.plant_status_out_today = true;
        g.plant_status = out;

        stageObservers.reset();
        otherObservers.reset();

        // report
        yield = plant.GrainTotal.DM() * gm2kg / sm2ha;

        sprintf (msg, "Crop ended. Yield (dw) = %7.1f  (kg/ha)", yield);
        parent->writeString (msg);

        // now do post harvest processes
        // put stover and any remaining grain into surface residue,
        //     any roots to soil FOM pool
        dm_residue =Tops().Total.DM();
        n_residue =Tops().Total.N();
        p_residue =Tops().Total.P();

        dm_root = root().Total.DM();     //FIXME - should be returned from a root method
        n_root  = root().Total.N();       //FIXME - should be returned from a root method
        p_root  = root().Total.P();       //FIXME - should be returned from a root method

       if (dm_residue + dm_root > 0.0)
          {
          // Build surface residues by part
          vector<string> part_name;
          vector<float> fraction_to_residue;           // fraction sent to residue (0-1)
          vector<float> dlt_dm_crop;                   // change in dry matter of crop (kg/ha)
          vector<float> dlt_dm_n;                      // N content of changeed dry matter (kg/ha)
          vector<float> dlt_dm_p;                      // P content of changeed dry matter (kg/ha)

          plant.onEndCrop(part_name,
                             dlt_dm_crop,
                             dlt_dm_n,
                             dlt_dm_p,
                             fraction_to_residue);

          if (sum(dlt_dm_crop) > 0.0)
              plant_send_crop_chopped_event ( c.crop_type
                                         , part_name
                                         , dlt_dm_crop
                                         , dlt_dm_n
                                         , dlt_dm_p
                                         , fraction_to_residue);
          }

        parent->writeString ("    Organic matter from crop:-      Tops to surface residue      Roots to soil FOM");

        sprintf (msg, "%48s%7.1f%24.1f"
                           , "DM (kg/ha) =               ", dm_residue * gm2kg /sm2ha, dm_root * gm2kg /sm2ha);
        parent->writeString (msg);

        sprintf (msg, "%48s%8.2f%24.2f"
                           , "N  (kg/ha) =               ", n_residue * gm2kg /sm2ha, n_root * gm2kg /sm2ha);
        parent->writeString (msg);

        if (pStress->isPhosphorusAware())
           {
           sprintf (msg, "%48s%7.2f%24.2f"
                           , "P  (kg/ha) =               ", p_residue * gm2kg /sm2ha, p_root * gm2kg /sm2ha);
           parent->writeString (msg);
           }

        parent->writeString (" ");

        plant.onPlantEvent("end_crop");

        UpdateCanopy();

        }
    else
        {
        sprintf(msg, "%s%s%s", g.module_name.c_str(), " is not in the ground -", " unable to end crop.");

        scienceAPI.warning(msg);
        }

    }
//+  Purpose
//      Get the values of variables/arrays from other modules.
void Plant::plant_get_other_variables (void)
    {
    std::vector<float> values;               // Scratch area

    scienceAPI.getOptional("parasite_dm_demand", "g/m2", g.dlt_dm_parasite_demand, 0.0, 10000.0);
    scienceAPI.getOptional("parasite_sw_demand", "mm", g.dlt_sw_parasite_demand, 0.0, 10000.0);

    scienceAPI.get("eo", "mm", g.eo, 0.0, 20.0);
    root().getOtherVariables();
    co2Modifier->doPlant_Co2Modifier ();
    }
void Plant::plant_update_other_variables (void)
//=======================================================================================
//  Update other modules states
    {
    vector<string> part_name;
    vector<float> dm_residue;                   // change in dry matter of crop (kg/ha)
    vector<float> dm_n;                      // N content of changeed dry matter (kg/ha)
    vector<float> dm_p;                      // P content of changeed dry matter (kg/ha)
    vector<float> fraction_to_residue;       // fraction of DM sent to surface residues

    // dispose of detached material from senesced parts in the live population
    plant.collectDetachedForResidue(part_name,
                                       dm_residue,
                                       dm_n,
                                       dm_p,
                                       fraction_to_residue);

    if (sum(dm_residue) > 0.0)
       plant_send_crop_chopped_event (c.crop_type,
                                      part_name,
                                      dm_residue,
                                      dm_n,
                                      dm_p,
                                      fraction_to_residue);

    UpdateCanopy();

    }

void Plant::UpdateCanopy()
//=======================================================================================
// Tell APSIM system that our canopy has changed to a new state.
   {

    float cover_tot = (float)(1.0
        - (1.0 - plant.coverGreen())
        * (1.0 - plant.coverSen()));

   protocol::NewCanopyType NewCanopy;
   NewCanopy.height = stem().height();
   NewCanopy.depth = stem().height();
   NewCanopy.lai = leaf().getLAI();
   NewCanopy.lai_tot = leaf().getLAI() + leaf().getSLAI();
   NewCanopy.cover = plant.coverGreen();
   NewCanopy.cover_tot = cover_tot;
   NewCanopy.sender = Name();
   scienceAPI.publish ("new_canopy",NewCanopy);

   }

//+  Purpose
//       Species initialisation - reads constants from constants file
void Plant::plant_read_species_const (void)
    {

//+  Local Variables
    vector<string> search_order;                  // sections to search
//- Implementation Section ----------------------------------

    string scratch = parent->readParameter (c.crop_type.c_str(), g.crop_class.c_str());

    Split_string(scratch, " ", search_order);

    scienceAPI.read("class_action", scratch);
    Split_string(scratch, " ", c.class_action);

    // Add the new class actions we're interested in
    for (unsigned i = 0; i != c.class_action.size(); i++)
      {
      if (find(actionsAlreadySubscribed.begin(),
               actionsAlreadySubscribed.end(),
               c.class_action[i]) == actionsAlreadySubscribed.end())
         {
         scienceAPI.subscribe(c.class_action[i], NullFunctionWithName(&Plant::onAction));
         actionsAlreadySubscribed.push_back(c.class_action[i]);
         }
      }

    scienceAPI.read("class_change", scratch);
    Split_string(scratch, " ", c.class_change);

    plant.readSpeciesParameters(parent, search_order);
    plantSpatial.read(scienceAPI);

    scienceAPI.read("n_fix_rate", c.n_fix_rate, 0.0, 1.0);
    scienceAPI.read("eo_crop_factor_default", c.eo_crop_factor_default, 0.0f, 100.0f);

    scienceAPI.read("n_supply_preference", c.n_supply_preference);

    //    plant_phenology_init                           //FIXME - should be in leafPart
    scienceAPI.read("leaf_no_pot_option", c.leaf_no_pot_option, 1, 3);          // switch to define the method of calculating node-appearance rate (PFR)
    scienceAPI.read("n_retrans_option", c.n_retrans_option, 1, 2);

    //    plant_n_senescence
    scienceAPI.read("n_senescence_option", c.n_senescence_option, 1, 2);

    //    plant_nfact
    nStress->read_n_constants ();

    //    plant_rue_reduction
   tempStress->read_t_constants ();
   swStress->read_sw_constants ();
   co2Modifier->read_co2_constants ();
   }

void Plant::plant_harvest_report (void)
//=======================================================================================
// Report the state of the crop at harvest time
    {
    //+  Constant Values
    const float  plant_c_frac = 0.4f;    // fraction of c in resiudes


    //+  Local Variables
    float grain_wt;                               // grain dry weight (g/kernel)
    float plant_grain_no;                          // final grains /head
    float n_grain;                                // total grain N uptake (kg/ha)
    float n_green;                                // above ground green plant N (kg/ha)
    float n_stover;                               // nitrogen content of stover (kg\ha)
    float n_total;                                // total gross nitrogen content (kg/ha)
    float n_grain_conc_percent;                   // grain nitrogen %
    char  msg[200];                               // message
    float yield;                                  // grain yield dry wt (kg/ha)
    float yield_wet;                              // grain yield including moisture (kg/ha)

    //- Implementation Section ----------------------------------


    // crop harvested. Report status
       yield = plant.GrainTotal.DM() * gm2kg / sm2ha;
       yield_wet = plant.dmGrainWetTotal() * gm2kg / sm2ha;
       grain_wt = plant.grainWt();
       plant_grain_no = (float)divide (plant.grainNo(), population().Density(), 0.0);
       n_grain = plant.GrainTotal.N() * gm2kg/sm2ha;


    float dmRoot = root().Total.DM() * gm2kg / sm2ha;
    float nRoot = root().Total.N() * gm2kg / sm2ha;

    n_grain_conc_percent = fruit().GrainTotal.NconcPercent();

    n_stover = Tops().VegetativeTotal.N() * gm2kg / sm2ha;
    n_green = Tops().Vegetative.N() * gm2kg / sm2ha;
    n_total = n_grain + n_stover;

    float stoverTot = Tops().VegetativeTotal.DM();
    float DMRrootShootRatio = (float)divide(dmRoot, Tops().Total.DM() * gm2kg / sm2ha, 0.0);
    float HarvestIndex      = (float)divide(yield, Tops().Total.DM() * gm2kg / sm2ha, 0.0);
    float StoverCNRatio     = (float)divide(stoverTot* gm2kg / sm2ha*plant_c_frac, n_stover, 0.0);
    float RootCNRatio       = (float)divide(dmRoot*plant_c_frac, nRoot, 0.0);

    parent->writeString ("");

    sprintf (msg, "%s%4d%26s%s%10.1f"
             , " flowering day          = ",floweringEventObserver->getDoy(), " "
             , " stover (kg/ha)         = ",stoverTot* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%4d%26s%s%10.1f"
             , " maturity day           = ", maturityEventObserver->getDoy(), " "
             , " grain yield (kg/ha)    = ", yield);
    parent->writeString (msg);

    sprintf (msg, "%s%6.1f%24s%s%10.1f"
             , " grain % water content  = ", fruit().grainWaterContent() * fract2pcnt, " "
             , " grain yield wet (kg/ha)= ", yield_wet);
    parent->writeString (msg);

    sprintf (msg, "%s%8.3f%22s%s%10.1f"
             , " grain wt (g)           = ", grain_wt, " "
             , " grains/m^2             = ", fruit().grainNo());
    parent->writeString (msg);

    sprintf (msg, "%s%6.1f%24s%s%10.3f"
             , " grains/plant           = ", plant_grain_no, " "
             , " maximum lai            = ", g.lai_max);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " total above ground biomass (kg/ha)    = ", Tops().Total.DM() * gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f",
               " live above ground biomass (kg/ha)     = "
              , (Tops().Total.DM())* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " green above ground biomass (kg/ha)    = ", Tops().Green.DM()* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " senesced above ground biomass (kg/ha) = ", Tops().Senesced.DM()* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%8.1f"
             , " number of leaves       = ", leaf().getLeafNo());
    parent->writeString (msg);

    sprintf (msg, "%s%8.2f%22s%s%10.2f"
             , " DM Root:Shoot ratio    = ", DMRrootShootRatio, " "
             , " Harvest Index          = ", HarvestIndex);
    parent->writeString (msg);

    sprintf (msg, "%s%8.2f%22s%s%10.2f"
             , " Stover C:N ratio       = ", StoverCNRatio, " "
             , " Root C:N ratio         = ", RootCNRatio);
    parent->writeString (msg);

    sprintf (msg, "%s%8.2f%22s%s%10.2f"
             , " grain N percent        = ", n_grain_conc_percent, " "
             , " total N content (kg/ha)= ", n_total);
    parent->writeString (msg);

    sprintf (msg, "%s%8.2f%22s%s%8.2f"
             , " grain N uptake (kg/ha) = ", n_grain, " "
             , " senesced N content (kg/ha)=", (Tops().VegetativeTotal.N() - Tops().Vegetative.N())* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%8.2f"
             , " green N content (kg/ha)= ", n_green);
    parent->writeString (msg);

    summary_p ();

    parent->writeString ("");

    sprintf (msg,"%s", " Average Stress Indices:                          Water Photo  Water Expan  N Photo      N grain conc");
    parent->writeString (msg);

    parent->writeString (g.averageStressMessage.c_str());
    g.averageStressMessage = "";

    }

bool  Plant::plant_auto_class_change (const char *action)
//=======================================================================================
// Change the Crop Class in response to a given action
   {
    string newclass = "unchanged";
    vector<string>::iterator i = find(c.class_action.begin(), c.class_action.end(),
                                      action);
    if (i != c.class_action.end())
       {
       newclass = c.class_change[i-c.class_action.begin()];
       }
    if (Str_i_Eq(newclass, "unchanged"))
        {
        // then do nothing
        return false;
        }
    else
        {
        g.crop_class = newclass;
        scienceAPI.setClass2(g.crop_class);
        read();
        return true;
        }
    }

void Plant::plant_send_crop_chopped_event (const string&  crop_type             // (INPUT) crop type
                                           ,vector<string> &dm_type             // (INPUT) residue type
                                           ,vector<float>  &dlt_crop_dm         // (INPUT) residue weight (kg/ha)
                                           ,vector<float>  &dlt_dm_n            // (INPUT) residue N weight (kg/ha)
                                           ,vector<float>  &dlt_dm_p            // (INPUT) residue P weight (kg/ha)
                                           ,vector<float>  &fraction_to_residue) // (INPUT) fraction going to residue
//=======================================================================================
// Send a CropChoppedEvent via the communications system
   {
    if (dm_type.size() != dlt_crop_dm.size() ||
        dm_type.size() != dlt_dm_n.size() ||
        dm_type.size() != dlt_dm_p.size() ||
        dm_type.size() != fraction_to_residue.size())
        throw std::runtime_error("Vector size mismatch in plant_send_crop_chopped_event");

    protocol::BiomassRemovedType chopped;
    chopped.crop_type = crop_type.c_str();
    for (unsigned i = 0; i < dm_type.size(); i++)
       {
       chopped.dm_type.push_back(dm_type[i].c_str());
       chopped.dlt_crop_dm.push_back(dlt_crop_dm[i]);
       chopped.dlt_dm_n.push_back(dlt_dm_n[i]);
       chopped.dlt_dm_p.push_back(dlt_dm_p[i]);
       chopped.fraction_to_residue.push_back(fraction_to_residue[i]);
       }
    scienceAPI.publish("BiomassRemoved", chopped);
    }

void Plant::get_plant_status(protocol::Component *system, protocol::QueryValueData &qd)
{
    switch (g.plant_status) {
        case out: system->sendVariable(qd, string("out")); break;
        case dead: system->sendVariable(qd, string("dead")); break;
        case alive: system->sendVariable(qd, string("alive")); break;
    }
}

float Plant::getPeswSeed(void)
{
   return root().pesw((int)plantSpatial.sowing_depth);
}

float Plant::getFaswSeed(void)
{
   return root().fasw((int)plantSpatial.sowing_depth);
}

float Plant::getLeafNo(void)
{
   return leaf().getLeafNo();
}

void Plant::get_height(protocol::Component *system, protocol::QueryValueData &qd)
{
   system->sendVariable(qd, plantSpatial.densityHeightFactor() * stem().height());
}

void Plant::get_width(protocol::Component *system, protocol::QueryValueData &qd)
{
   system->sendVariable(qd, leaf().width());
}

void Plant::get_cover_tot(protocol::Component *system, protocol::QueryValueData &qd)
{
    float cover_tot = (float)(1.0
        - (1.0 - plant.coverGreen())
        * (1.0 - plant.coverSen()));

    system->sendVariable(qd, cover_tot);
}

void Plant::get_biomass(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, Tops().Total.DM() * gm2kg / sm2ha);
}
void Plant::get_respiration(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, plant.Respiration());
}

void Plant::get_effective_rue(protocol::Component *system, protocol::QueryValueData &qd)
{
   float erue;
   if (plant.Respiration()>0.)   // SPASS model used
      erue = (float)divide(plant.Growth.DM()-plant.Respiration()-root().Growth.DM(),environment().radn()*Tops().coverGreen(),0.0);
   else
      erue = (float)divide(plant.Growth.DM()-plant.Respiration(),environment().radn()*Tops().coverGreen(),0.0);
    //erue = divide(Tops().Growth.DM(),environment().radn()*Tops().coverGreen(),0.0);
    system->sendVariable(qd, erue);
}


void Plant::get_green_biomass(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, Tops().Green.DM() * gm2kg / sm2ha);
}

void Plant::get_biomass_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, Tops().Total.DM());
}


void Plant::get_green_biomass_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, Tops().Green.DM());
}

void Plant::get_stover_biomass_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    float stoverTot = Tops().VegetativeTotal.DM();
    system->sendVariable(qd, stoverTot);
}

void Plant::get_dm_plant_min(protocol::Component *system, protocol::QueryValueData &qd)
{
   vector<float>  dm_min;

   plant.get_dm_plant_min(dm_min);

   system->sendVariable(qd, dm_min);
}

void Plant::get_biomass_n(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, Tops().Total.N());
}

void Plant::get_n_uptake(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, Tops().Total.N());
}

void Plant::get_green_biomass_n(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, Tops().Green.N());
}

// plant nitrogen
void Plant::get_n_conc_stover(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = (float)divide (Tops().Vegetative.N(), Tops().Vegetative.DM(), 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}

void Plant::get_n_conc_crit(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = (float)divide ((leaf().n_conc_crit()*leaf().Green.DM()
                           + stem().n_conc_crit()*stem().Green.DM())
                          , (leaf().Green.DM() + stem().Green.DM())
                          , 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}

void Plant::get_n_conc_min(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = (float)divide ((leaf().n_conc_min() * leaf().Green.DM()
                            + stem().n_conc_min() * stem().Green.DM())
                          , (leaf().Green.DM() + stem().Green.DM())
                          , 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}

void Plant::get_n_uptake_stover(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, Tops().Vegetative.N());
}

void Plant::get_n_demanded(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<float>  n_demanded;

   plant.get_n_demanded(n_demanded);

   systemInterface->sendVariable(qd, n_demanded);
}

void Plant::get_nfact_grain_tot(protocol::Component *system, protocol::QueryValueData &qd)
{
    float sum = g.cnd_grain_conc.getSum();
   system->sendVariable(qd, sum);
}

void Plant::get_no3_demand(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, (float)g.ext_n_demand*gm2kg/sm2ha);
}

void Plant::get_transp_eff(protocol::Component *system, protocol::QueryValueData &qd)
{
    float transp_eff = leaf().transpirationEfficiency(); //FIXME - ?? how to handle fruit().transpirationEfficiency();
    system->sendVariable(qd, transp_eff);
}

void Plant::get_parasite_c_gain(protocol::Component *system, protocol::QueryValueData &qd)
{
  float dlt_wt = g.dlt_dm_parasite + g.dm_parasite_retranslocate;
  system->sendVariable(qd, dlt_wt);
}

void Plant::get_dm_parasite_retranslocate(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, g.dm_parasite_retranslocate);
}

void Plant::get_biomass_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, Tops().Total.P());  //()
}

void Plant::get_green_biomass_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, Tops().Green.P());  //()
}
//NIH up to here
void Plant::get_p_conc_stover(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float p_conc_stover = (float)divide (Tops().Vegetative.P(), Tops().Vegetative.DM(), 0.0) * fract2pcnt ;
    systemInterface->sendVariable(qd, p_conc_stover);  //()
}

void Plant::get_p_uptake_stover(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, Tops().Vegetative.P());  //()
}

void Plant::get_dlt_dm_green_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<float>  dlt_dm_green_retrans;

   plant.get_dlt_dm_green_retrans(dlt_dm_green_retrans);

   systemInterface->sendVariable(qd, dlt_dm_green_retrans);
}

void Plant::get_p_demand(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   systemInterface->sendVariable(qd, plant.pDemand());   //(g/m^2
}

void Plant::get_p_demand_parts(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<float>  p_demand;

   plant.get_p_demand(p_demand);

   systemInterface->sendVariable(qd, p_demand);   //(g/m^2
}

void Plant::get_dlt_p_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<float>  dlt_p_retrans;

   plant.get_dlt_p_retrans(dlt_p_retrans);

   systemInterface->sendVariable(qd, dlt_p_retrans);
}

float Plant::getCumSwdefPheno() {return g.cswd_pheno.getSum();}
float Plant::getCumSwdefPhoto() {return g.cswd_photo.getSum();}
float Plant::getTempStressPhoto(void)  {return tempStress->tFact.photo;}
float Plant::getNfactPhoto(void)  {return nStress->nFact.photo;}
float Plant::getNfactGrainConc(void)  {return nStress->nFact.grain;}
float Plant::getOxdefPhoto(void)  {return swStress->swDef.oxdef_photo;}
float Plant::getPfactPhoto(void)  {return pStress->pFact.photo;}
float Plant::getSwdefPhoto(void)  {return swStress->swDef.photo;}
float Plant::getSwDefPheno() {return swStress->swDef.pheno;}
float Plant::getNFactPheno() {return nStress->nFact.pheno;}
float Plant::getPFactPheno() {return pStress->pFact.pheno;}
const std::string & Plant::getCropType(void) {return c.crop_type;};
protocol::Component *Plant::getComponent(void) {return parent;};


