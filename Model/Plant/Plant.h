#ifndef PlantH
#define PlantH

class ApsimVariant;
class Phenology;
class plantPart;
class SimplePart;
class Stem;
class Leaf;
class RootBase;
class plantThing;
class eventObserver;
class Plant;
class Arbitrator;
class pheno_stress_t;
class Co2Modifier;
class Fixation;
class Parts;

#include "PlantInterface.h"
#include "Environment.h"
#include "PlantSpatial.h"
#include "CompositePart.h"
#include "Population.h"
#include "PlantStress.h"
#include "Co2Modifier.h"
typedef bool (Plant::*ptr2setFn) (protocol::QuerySetValueData&);

typedef std::map<unsigned, ptr2setFn>   UInt2SetFnMap;
typedef std::map<unsigned, string>      UInt2StringMap;


//      Process names used for stress
// photosynthesis flag
const int  photo = 0 ;
// cell expansion flag
const int  expansion = 1 ;
// phenological flag
const int  pheno = 2 ;
// grain concentration flag
const int  grain_conc = 3 ;
// N fixation flag
const int  fixation = 4 ;

//   This class performs crop crop growth
//     simulates root, leaf, head, stem and grain development. Water and
//     nitrogen uptake, photosynhesis, and leaf and root senescense.

class Plant : public plantInterface, public IPlant {
private:
   protocol::Component *parent;              // for interface calls to system
   ScienceAPI& scienceAPI;
   stageSubject   stageObservers;            // A collection of state variable observers, reset at each new stage
   stageSubject   otherObservers;            // Another collection of state variable observers

   PlantSpatial plantSpatial;

   CompositePart plant;
   CompositePart* tops;
   CompositePart* grain;

   Stem* _stem;
   Leaf* _leaf;
   RootBase*  _root;
   Phenology* _phenology;
   Fixation* _fixation;
   plantPart*  _fruit;
   Population* _population;
   Environment* _environment;
   Arbitrator* _arbitrator;

   eventObserver *sowingEventObserver;     // Bookkeeper for Sowing events
   eventObserver *emergenceEventObserver;  // Bookkeeper for Emergence events
   eventObserver *FIEventObserver;         // Bookkeeper for Floral Initiation events
   eventObserver *floweringEventObserver;  // Bookkeeper for flowering events
   eventObserver *maturityEventObserver;   // Bookkeeper for maturity events

   StressDeficit swDef;
   StressDeficit nFact;
   StressDeficit pFact;
   PStress *pStress;
   NStress *nStress;
   SWStress *swStress;
   TempStress *tempStress;
   Co2Modifier *co2Modifier;
   vector<string> actionsAlreadySubscribed;


   void read(void);

   std::string phenologyEventToday;
   std::string phenologyEventYesterday;
   bool phenologyRewoundToday;


public:
   Plant(protocol::Component *P, ScienceAPI& api, XMLNode parameters);
   ~Plant(void);

   Environment& environment() {return *_environment;}
   CompositePart& All() {return plant;}
   Phenology& phenology() {return *_phenology;}
   RootBase& root() {return *_root;}
   Arbitrator& arbitrator() {return *_arbitrator;}
   Population& population() {return *_population;}
   Fixation& fixation() {return *_fixation;}
   Leaf& leaf() {return *_leaf;}
   Stem& stem() {return *_stem;}
   plantPart& fruit() {return *_fruit;}

   void onInit1(void);
   void onInit2(void);
   void onPrepare();
   void onProcess();
   void onSow(protocol::ApsimVariant& variant);
   void onHarvest(protocol::ApsimVariant &variant);
   void onEndCrop() ;
   void onKillStem(protocol::ApsimVariant &variant);
   void onRemoveCropBiomass(protocol::RemoveCropDmType& dmRemoved);
   void onDetachCropBiomass(float detachRate);
   void onEndRun();
   void onAction(const std::string& eventName);
   void onTick(unsigned &, unsigned &, protocol::Variant &v) ;

   void doPlantEvent(const string& oldStageName, const string& newStageName, bool phenologyRewound);

   const std::string & getCropType(void) ;
   protocol::Component *getComponent(void) ;
   std::string Name(void) {return g.module_name;}

   void doNDemand (int option       /* (INPUT) option number*/);

   void doNDemandEstimate (int option);
   void doNSenescence (int   option/*(INPUT) option number*/);
   void plant_cleanup (void);
   void plant_update(void) ;
   void plant_event(const std::string& newStageName);

   void plant_root_depth (int option /* (INPUT) option number*/);
   void doPlantRadnPartition (int option /*(INPUT) option number*/);
   bool onSetPhase (protocol::QuerySetValueData &v/*(INPUT) message variant*/);
   void plant_dormancy (protocol::Variant &v/*(INPUT) incoming message variant*/);
   void plant_harvest_update (protocol::ApsimVariant &v);
   void plant_remove_biomass_update (protocol::RemoveCropDmType dmRemoved);
   void plant_zero_all_globals (void);
   void plant_zero_variables (void);
   void plant_zero_daily_variables (void);
   void plant_start_crop(protocol::ApsimVariant& variant);
   void plant_end_crop (void);
   void plant_get_other_variables (void);
   void plant_update_other_variables (void);
   void plant_read_species_const (void);
   void plant_harvest_report (void);
   bool  plant_auto_class_change (const char *action);
   void plant_send_crop_chopped_event (const string&  crop_type               // (INPUT) crop type
                                       ,vector<string> &dm_type               // (INPUT) residue type
                                       ,vector<float>  &dlt_crop_dm           // (INPUT) residue weight (kg/ha)
                                       ,vector<float>  &dlt_dm_n              // (INPUT) residue N weight (kg/ha)
                                       ,vector<float>  &dlt_dm_p              // (INPUT) residue P weight (kg/ha)
                                       ,vector<float>  &fraction_to_residue);


   void onSetCropClass(const std::string& crop_class);

   void get_plant_status(protocol::Component *, protocol::QueryValueData &);
   status_t Status(void) {return g.plant_status;}
   void SetStatus(status_t NewStatus) {g.plant_status = NewStatus;}
   CompositePart& Tops(void) {return *tops;}
   CompositePart& Grain(void) {return *grain;}

   void get_crop_type(protocol::Component *, protocol::QueryValueData &);
   void get_crop_class(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_no(protocol::Component *, protocol::QueryValueData &);
   float getPeswSeed(void);
   float getFaswSeed(void);
   float getLeafNo(void);
   float getCumSwdefPheno(void);
   float getCumSwdefPhoto(void);
   void get_dlt_leaf_no(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_node_no(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_no_dead(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_area(protocol::Component *, protocol::QueryValueData &);
   void get_height(protocol::Component *, protocol::QueryValueData &);
   void get_width(protocol::Component *, protocol::QueryValueData &);
   void get_root_depth(protocol::Component *, protocol::QueryValueData &);
   void get_plants(protocol::Component *, protocol::QueryValueData &);

   float getTempStressPhoto(void);
   float getNfactPhoto(void);
   float getNfactGrainConc(void);
   float getOxdefPhoto(void);
   float getPfactPhoto(void);
   float getSwdefPhoto(void);
   float getSwDefPheno();
   float getNFactPheno();
   float getPFactPheno();

   void get_cover_tot(protocol::Component *, protocol::QueryValueData &);
   void get_lai_canopy_green(protocol::Component *, protocol::QueryValueData &);
   void get_pai(protocol::Component *, protocol::QueryValueData &);
   void get_root_wt(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_wt(protocol::Component *, protocol::QueryValueData &);
   void get_stem_wt(protocol::Component *, protocol::QueryValueData &);
   void get_respiration(protocol::Component *, protocol::QueryValueData &);
   void get_effective_rue(protocol::Component *, protocol::QueryValueData &);
   void get_biomass(protocol::Component *, protocol::QueryValueData &);
   void get_green_biomass(protocol::Component *, protocol::QueryValueData &);
   void get_biomass_wt(protocol::Component *, protocol::QueryValueData &);
   void get_green_biomass_wt(protocol::Component *, protocol::QueryValueData &);
   void get_stover_biomass_wt(protocol::Component *, protocol::QueryValueData &);
   void get_dm_plant_min(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_pot_rue(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_green_retrans(protocol::Component *, protocol::QueryValueData &);
   void get_biomass_n(protocol::Component *, protocol::QueryValueData &);
   void get_n_uptake(protocol::Component *, protocol::QueryValueData &);
   void get_green_biomass_n(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_n(protocol::Component *, protocol::QueryValueData &);
   void get_stem_n(protocol::Component *, protocol::QueryValueData &);
   void get_root_n(protocol::Component *, protocol::QueryValueData &);
   void get_deadleaf_n(protocol::Component *, protocol::QueryValueData &);
   void get_temp_stress_photo(protocol::Component *, protocol::QueryValueData &);
   void get_oxdef_photo(protocol::Component *, protocol::QueryValueData &);
   void get_transp_eff(protocol::Component *, protocol::QueryValueData &);
   void get_ep(protocol::Component *, protocol::QueryValueData &);
   void get_sw_uptake(protocol::Component *, protocol::QueryValueData &);
   void get_sw_supply(protocol::Component *, protocol::QueryValueData &);
   void get_esw_layr(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_stover(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_root(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_crit(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_min(protocol::Component *, protocol::QueryValueData &);

   void get_n_uptake_stover(protocol::Component *, protocol::QueryValueData &);
   void get_no3_tot(protocol::Component *, protocol::QueryValueData &);
   void get_n_demanded(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_grain_tot(protocol::Component *, protocol::QueryValueData &);
   void get_rlv(protocol::Component *, protocol::QueryValueData &);
   void get_no3_demand(protocol::Component *, protocol::QueryValueData &);
   void get_sw_demand(protocol::Component *, protocol::QueryValueData &);
   void get_sw_demand_te(protocol::Component *, protocol::QueryValueData &);
   void get_root_length(protocol::Component *, protocol::QueryValueData &);
   void get_root_length_dead(protocol::Component *, protocol::QueryValueData &);

   void get_parasite_c_gain(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_area_tot(protocol::Component *, protocol::QueryValueData &);
   void get_dm_parasite_retranslocate(protocol::Component *, protocol::QueryValueData &);
   void get_sw_supply_layr(protocol::Component *, protocol::QueryValueData &);
   void get_no3_uptake(protocol::Component *, protocol::QueryValueData &);
   void get_nh4_uptake(protocol::Component *, protocol::QueryValueData &);
   void get_zadok_stage(protocol::Component *, protocol::QueryValueData &);


   void get_p_demand(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_demand_parts(protocol::Component *, protocol::QueryValueData &qd);
   void get_biomass_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_uptake(protocol::Component *, protocol::QueryValueData &qd);
   void get_green_biomass_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_leaf_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_stem_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_root_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_deadleaf_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_dlt_p_retrans(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_conc_stover(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_conc_leaf(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_uptake_stover(protocol::Component *, protocol::QueryValueData &qd);
   void get_ll_dep(protocol::Component *systemInterface, protocol::QueryValueData &qd);
   void get_ll(protocol::Component *systemInterface, protocol::QueryValueData &qd);

   //Phosporousy things:
   void doPInit();
   bool phosphorusAware(void)  {return pStress->isPhosphorusAware();};
   bool removeBiomassReport(void)  {return c.remove_biomass_report;};
   void prepare_p(void);
   void summary_p (void);

   void  doPPartition (void);
   void  doPRetranslocate (void);
   const Co2Modifier  *getCo2Modifier(void) {return co2Modifier;};

private:
   void UpdateCanopy(void);

   //     ================================================================
   //       plant Globals
   //     ================================================================
   struct {
      bool  hasreadconstants;
      string   module_name;                             // module name
      string   crop_class;                              // crop type
      status_t plant_status;                            // status of crop
      bool  plant_status_out_today;
      string   cultivar;                                // name of cultivar
      string   pre_dormancy_crop_class;
      string   averageStressMessage;                    // Message of average stresses for each phase
      float remove_biom_pheno;
      float fr_intc_radn;                               // fraction of radiation intercepted by canopy
      float eo;                                         // potential evapotranspiration (mm)
      factorObserver cnd_photo;                         // cumulative nitrogen stress type 1
      factorObserver cnd_grain_conc ;                   // cumulative nitrogen stress type 2
      factorObserver cswd_pheno;                        // cumulative water stress type 1
      factorObserver cswd_photo;                        // cumulative water stress type 1
      factorObserver cswd_expansion ;                   // cumulative water stress type 2

      float n_fix_pot;                                  // N fixation potential (g/m^2)
      float n_fix_uptake;                               // N fixation actual (g/m^2)
      float n_fixed_tops;                               // cum. fixed N in tops

      float lai_max;                                    // maximum lai - occurs at flowering
      float ext_n_demand;

      // parasite
      float       dlt_dm_parasite_demand;  // parasite dm demand [g/m^2]
      float       dlt_sw_parasite_demand;  // parasite dm demand [g/m^2]
      float       dm_parasite_retranslocate; // plant biomass retranslocated to parasite [g/m^2]
      float       dlt_dm_parasite;         // parasite biomass growth [g/m^2]

   } g;   // Globals


   //     ================================================================
   //       plant Parameters
   //     ================================================================
   struct {
      float eo_crop_factor;                             // Crop factor for sw demand applied to Eo
   } p; // Parameters


   //     ================================================================
   //       plant Constants
   //     ================================================================
   struct {
      int   leaf_no_pot_option;
      int   n_retrans_option;
      int   n_senescence_option;

      string crop_type;                                  // crop type
      string default_crop_class;                         // crop class
      string n_supply_preference;                        // preference of n supply

      std::vector<float> n_fix_rate;                    // potential rate of N fixation (g N fixed
                                                        // per g above ground biomass
      vector<string> class_action;
      vector<string> class_change;

      float eo_crop_factor_default;                     // Default Crop factor for sw demand applied to Eo

      bool remove_biomass_report;
      bool remove_biomass_affects_phenology;
      
   }  c;   // Constants

};  // Plant

#endif //PLANT_H_
