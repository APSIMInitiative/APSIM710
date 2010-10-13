#ifndef PlantLibaryH
#define PlantLibaryH

#include <queue>
#include <numeric>
#include <iterator>
#include <list>
#include <math.h>
#include <string>
#include <algorithm>
#include "Utility/ExternalFunction.h"
#include "Utility/InterpolationFunction.h"
#include "Utility/LookupFunction.h"
using namespace std;

class plantInterface;
class ScienceAPI;

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
#define add_covers(A,B) (1.0 - (1.0-A)*(1.0-B))


/*   ================================================================
 *    Conversion constants
 *   ================================================================
 *
 *   Short description:
 *     Globally used conversion constants
 */

 // ----------------------- Declaration section ------------------------

 //  Constant values
// WEIGHT conversion
extern const float gm2kg;           // constant to convert g to kg
extern const float kg2gm ;          // conversion of kilograms to grams
extern const float mg2gm ;          // conversion of mg to grams
extern const float t2g ;            // tonnes to grams
extern const float g2t ;            // grams to tonnes
extern const float t2kg ;           // tonnes to kilograms
extern const float kg2t ;           // kilograms to tonnes


// AREA conversion
extern const float ha2scm ;         // ha to sq cm
extern const float ha2sm ;          // conversion of hectares to sq metres
extern const float sm2ha ;          // constant to convert m^2 to hectares
extern const float sm2smm ;         // conversion of square metres to square mm
extern const float smm2sm ;         // conversion factor of mm^2 to m^2
extern const float scm2smm ;        // conversion factor of cm^2 to mm^2


// PRESSURE and related conversion
extern const float g2mm ;           // convert g water/m^2 to mm water
                                          // 1 g water = 1,000 cubic mm and
                                          // 1 sq m = 1,000,000 sq mm
extern const float mb2kpa ;        // convert pressure mbar to kpa
                                          // 1000 mbar = 100 kpa

// LENGTH conversion
extern const float cm2mm;                 // cm to mm
extern const float mm2cm ;             // conversion of mm to cm
extern const float mm2m ;            // conversion of mm to m
extern const float km2m  ;               // conversion of km to metres


// VOLUME conversion
extern const float cmm2cc ;          // conversion of cubic mm to cubic cm
extern const float conv_gmsm2kgha ;       // convert g/sm -> kg/ha
extern const float conv_pc2fr ;            // convert %age to fraction
extern const float pcnt2fract ;       // convert percent to fraction
extern const float fract2pcnt ;           // convert fraction to percent
extern const float mm2lpsm ;                // mm depth to litres per sq m
extern const float lpsm2mm ;                // litres per sq m to mm depth
extern const float day2hr  ;               // days to hours
extern const float hr2s    ;          // hours to seconds
extern const float s2hr    ;           // seconds to hours

namespace protocol {
  class Component;
};

class SWStress;

// Class containing stress deficits
class StressDeficit {
   public:
     StressDeficit() {
         photo = 1.0;
         pheno = 1.0;
         pheno_flower = 1.0;
         pheno_grainfill = 1.0;
         grain = 1.0;
         expansion = 1.0;
         fixation = 1.0;
         oxdef_photo = 1.0;
};
     ~StressDeficit() {};
const  StressDeficit &operator=(const float &y)
{
         photo = y;
         pheno = y;
         pheno_flower = y;
         pheno_grainfill = y;
         grain = y;
         expansion = y;
         fixation = y;
         oxdef_photo = y;
      return *this;
}
   public:
      float photo;
      float pheno;
      float pheno_flower;
      float pheno_grainfill;
      float grain;
      float expansion;
      float fixation;
      float oxdef_photo;
};

// A queue class for returning moving averages.
class plantQueue {
   private:
      unsigned int qSize;
      deque<float> myqueue;

   public:
     plantQueue(unsigned int size=366) : qSize(size) {};
     virtual ~plantQueue() {};
     void setSize(unsigned int size)
     {
         qSize = size;
     };
     void storeValue(float value)
     {
         myqueue.push_back(value);
         if(myqueue.size() > qSize)myqueue.pop_front();
     };
      float sum(void)
      {
        return accumulate(myqueue.begin(), myqueue.end(), 0.0F);
      };
     unsigned count(void) {return myqueue.size();};
     float average(void) {return (count() > 0 ? sum()/count() : 0.0F);};
};


// A class that observes variables. Mostly report-related.
class observer {
   protected:
     int n;
     float *myLocation, mySum;

   public:
     observer() {};
     virtual ~observer() {};
     void setup(float *loc) {
     	  myLocation = loc;
     	  mySum = 0.0; n= 0;
     };
     virtual void update(void) = 0;
     void reset(void) {mySum = 0.0; n = 0;};
     float getSum(void) {return mySum;};
     float getN(void) {return (float)n;};
     float getAverage(void) {return (n > 0 ? mySum/(float)n : 1.0f);};
};

// Keep track of state variable
class stateObserver : public observer {
   public:
     void update(void) {mySum += *myLocation; n++;};
};

// Factors are "1 - state" style variables
class factorObserver : public observer {
   public:
     void update(void) {mySum += (1.0f - *myLocation); n++;};
};

// A subject (observer manager)
class stageSubject {
   private:
     std::list<observer *> observers;
   public:
     stageSubject();
     void addObserver(observer *s) {observers.push_back(s);};
     void update() ;
     void reset() ;
};


float legnew_vernal_days(float  g_maxt
                               ,float  g_mint
                               ,float  *c_x_vernal_temp
                               ,float  *c_y_vernal_days
                               ,int    c_num_vernal_temp) ;



void crop_dm_pot_rue (float current_stage,
                float *rue, float radn_int, float temp_stress_photo,
                float nfact_photo, float *dlt_dm_pot);

void plant_dm_senescence (int num_part               // (INPUT) number of plant parts
                               , float max_Table            // (INPUT) max lookup length
                               , float independant_variable // (INPUT) independant variable which is said to drive senescence.
                               , float **c_x_dm_sen_frac    // (INPUT)  lookup for independant variabl
                               , float **c_y_dm_sen_frac    // (INPUT)  fraction of  material senescin
                               , int *c_num_dm_sen_frac     // (INPUT)  fraction of  material sene
                               , float *g_dm_green          // (INPUT)  live plant dry weight (biomass
                               , float *dlt_dm_senesced);    // (OUTPUT) actual biomass senesced
                                                            //          from plant parts (g/m^2)

void crop_dm_senescence0(const int num_part,              //(INPUT)  number of plant parts
                         const int root,                  //(INPUT)  number of plant root part
                         const int leaf,                  //(INPUT)  number for plant leaf part
                         const int stem,                  //(INPUT)  number for plant stem part
                         float dm_leaf_sen_frac,         //(INPUT)  fraction of senescing leaf dry
                         float dm_root_sen_frac,         //(INPUT)  fraction of root dry matter se
                         float *dlt_dm_green,             //(INPUT)  plant biomass growth (g/m^2)
                         float *dlt_dm_green_retrans,     //(INPUT)  plant biomass retranslocated
                         float dlt_lai,                  //(INPUT)  actual change in live plant la
                         float dlt_slai,                 //(INPUT)  area of leaf that senesces fro
                         float *dm_green,                 //(INPUT)  live plant dry weight (biomass
                         float lai,                      //(INPUT)  live plant green lai
                         float *dlt_dm_senesced,          //(OUTPUT) actual biomass senesced from plant parts (g/m^2)
                         float *dlt_dm_sen_retrans);       //(OUTPUT) reduction in senesced biomass as a result of retranslocation

void crop_dm_dead_detachment(const int num_part,
                             float *dead_detach_frac,         //(INPUT)  fraction of dead plant parts detached
                             float *dm_dead,                  //(INPUT)  dry wt of dead plants (g/m^2)
                             float *dlt_dm_dead_detached);     //(OUTPUT) change in dm of dead plant

void cproc_dm_senescence1 (const int num_part,           //(INPUT)  number of plant parts
                           const int max_tabl,          //(INPUT)  max lookup length
                           float independant_variable,   //(INPUT)  independant variable which
                           float **c_x_dm_sen_frac,        //(INPUT)  lookup for independant variabl   is said to drive senescence.
                           float **c_y_dm_sen_frac,        // (INPUT)  fraction of  material senescin
                           int   *c_num_dm_sen_frac,         // (INPUT)  fraction of  material sene
                           float *g_dm_green,              // (INPUT)  live plant dry weight (biomass
                           float *g_dlt_dm_green,          // (INPUT)  plant biomass growth (g/m^2)
                           float *g_dlt_dm_green_retrans,  // (INPUT)  plant biomass retranslocat
                           float *dlt_dm_senesced);         // (OUTPUT) actual biomass senesced from plant parts (g/m^2)

void cproc_dm_detachment1( const int max_part,
                           float *c_sen_detach_frac,
                           float *g_dm_senesced,
                           float *g_dlt_dm_detached,
                           float *c_dead_detach_frac,
                           float *g_dm_dead,
                           float *g_dlt_dm_dead_detached);

void cproc_bio_yieldpart_demand1(float G_current_stage,             // (INPUT)  current phenological stage
                                 int Start_stress_stage,            // (INPUT)
                                 int Start_grainfill_stage,         // (INPUT)
                                 int End_grainfill_stage,           // (INPUT)
                                 int Yield_part,                    // (INPUT)
                                 int Root_part,                     // (INPUT)
                                 int Max_part,                      // (INPUT)
                                 float G_dlt_dm,                    // (INPUT)  the daily biomass production (
                                 float *G_dm_green,                 // (INPUT)  live plant dry weight (biomass
                                 float *G_dm_senesced,              // (INPUT)  senesced plant dry wt (g/m^2)
                                 float *G_days_tot,                 // (INPUT)  duration of each phase (days)
                                 float *G_dm_stress_max,            // (INPUT)  sum of maximum daily stress on
                                 float P_hi_incr,                  // (INPUT)  harvest index increment per da
                                 float *P_x_hi_max_pot_stress,      // (INPUT) Potential Max HI Stress dete
                                 float *P_y_hi_max_pot,             // (INPUT) Potential Max HI
                                 int P_num_hi_max_pot,             // (INPUT) Number of lookup pairs
                                 float *Dlt_dm_yieldpart_demand);    //(OUTPUT) grain dry matter potential (g/m^2)

void cproc_yieldpart_demand_stress1(float G_nfact_photo,
                                    float G_swdef_photo,
                                    float G_temp_stress_photo,
                                    float *Dlt_dm_stress_max);

void cproc_bio_init1(float *C_dm_init,         //(INPUT)
                     int   Init_stage,         //(INPUT)
                     float G_current_stage,    //(INPUT)  current phenological stage
                     float G_plants,           //(INPUT)  Plant density (plants/m^2)
                     float Max_part,           //(INPUT)
                     float *G_dm_green);        //(INPUT/OUTPUT) plant part weights  (g/m^2)

void cproc_rue_n_gradients(int   day,                  // !day of the year
                           float latitude,           //latitude in degree
                           float radiation,          //daily global radiation (MJ/m2/d)
                           float tempmax,            //daily maximum tempeature (C)
                           float tempmin,            //daily minimum tempeature (C)
                           float lai_green,          //leaf area index (-)
                           float sln_gradient,      //SLN gradients in canopy (g N/m2 leaf)
                           float pmaxmax,            //potential assimilation rate (SLN ASYMPTOTE) (mg CO2/m2.s)
                           float shadow_projection,  //shadow projection (=0.5)
                           float biomass_conversion, //biomass coversion for biochemical coversion and maintenance respiration (mg DM / mgCO2)
                           float scatter_coeff,      //scattering coefficients (=0.15)
                           float *rue_sln);            //rue based on SLN gradients in the canopy (g DM / MJ)

void crop_dm_pot_rue_co2 (float current_stage,
                          float *rue,
                          float radn_int,
                          float temp_stress_photo,
                          float nfact_photo,
                          float co2_modifier,
                          float *dlt_dm_pot);     //(OUTPUT) potential dry matter  (carbohydrate) production (g/m^2

  void legnew_bio_yieldpart_demand2(
    float g_grain_no
    ,float p_potential_grain_filling_rate
    ,float g_maxt
    ,float g_mint
    ,float *c_x_temp_grainfill
    ,float *c_y_rel_grainfill
    ,int   c_num_temp_grainfill
    ,float *g_dlt_dm_grain_demand
    ) ;

//---------------------------------------------------------------------------
  float legopt_leaf_area_sen_age1
    (float  *g_leaf_no
    ,float  *g_leaf_no_dead
    ,float  g_dlt_leaf_no_dead
    ,int    max_node
    ,float  g_lai
    ,float  g_slai
    ,float  c_min_tpla
    ,float  *g_leaf_area
    ,float  g_plants);

//  void legopt_leaf_no_init1
//    (
//     float  c_leaf_no_at_emerg
//    ,float  *leaf_no
//    ,float  *node_no
//    ) ;
//  void legopt_leaf_area_init1
//    (
//     float  c_initial_tpla
//    ,float  c_leaf_no_at_emerg
//    ,float  g_plants
//    ,float  *lai
//    ,float  *leaf_area
//    ) ;
  void legopt_leaf_area_sen1
    (float  g_dlt_lai_stressed
    ,float  g_dlt_leaf_no
    ,float  g_dlt_leaf_no_dead
    ,float  g_lai
    ,float  *g_leaf_area
    ,float  *g_leaf_no
    ,float  *g_leaf_no_dead
    ,int    max_node
    ,float  g_plants
    ,float  g_slai
    ,float  c_min_tpla
    ,float  *g_dlt_slai_age
    ,float  c_lai_sen_light
    ,float  c_sen_light_slope
    ,float  *g_dlt_slai_light
    ,float  c_sen_rate_water
    ,float  g_swdef_photo
    ,float  *g_dlt_slai_water
    ,float  *c_x_temp_senescence
    ,float  *c_y_senescence_fac
    ,int    c_num_temp_senescence
    ,float  g_mint
    ,float  *g_dlt_slai_frost
    ,float  *g_dlt_slai
    ) ;
void cproc_leaf_no_pot3(
     interpolationFunction &
    ,interpolationFunction &
    ,bool   inNodeFormationPhase
    ,bool   inEmergenceDay
    ,float  node_no_now                       // (INPUT) current number of nodes
    ,float  g_dlt_tt                          // (INPUT)  daily thermal time (growing de
    ,float  stressFactor
    ,float  *g_leaves_per_node                 // OUTPUT
    ,float  *dlt_leaf_no_pot                   // (OUTPUT) new fraction of oldest expanding leaf
    ,float  *dlt_node_no_pot                   // (OUTPUT) new fraction of oldest expanding node on main stem
    ) ;
  void plant_canopy_width
    (
     float  g_canopy_width
    ,float  *p_x_stem_wt
    ,float  *p_y_width
    ,int    p_num_stem_wt
    ,float  *g_dm_green
    ,float  g_plants
    ,int    stem
    ,float  *dlt_canopy_width
    ) ;

void legnew_canopy_fac (
     float  g_row_spacing
    ,float  g_plants
    ,float  g_skip_row_fac
    ,float  g_skip_plant_fac
    ,float  g_canopy_width
    ,float  *g_canopy_fac) ;

void crop_lai_equilib_light ( float radn_int,
                              float cover_green,
                              float sen_radn_crit,
                              float extinction_coef,
                              float lai,
                              int   day_of_year,
                              int   year,
                              float *lai_eqlb_light);     //(INPUT/OUTPUT) lai threshold for light senescence

float crop_leaf_area_sen_frost1(interpolationFunction &frost_fraction,        //(INPUT)
                               float lai,                   //(INPUT)  live plant green lai
                               float mint,                  //(INPUT)  minimum air temperature (o_c)
                               float plants,                //(INPUT)
                               float min_tpla);              //(INPUT)

void crop_leaf_area_sen_water2(int   day_of_year,                //(INPUT)  day of year
                               int   year,                       //(INPUT)  year
                               float sen_threshold,              //(INPUT)  supply:demand ratio for onset
                               float sen_water_time_const,       //(INPUT)  delay factor for water senesce
                               int   num_layer,                  //(INPUT)  number of layers in profile
                               float *dlayer,                     //(INPUT)  thickness of soil layer I (mm)
                               float lai,                        //(INPUT)  live plant green lai
                               float *lai_equilib_water,          //(INPUT)  lai threshold for water senesc
                               float root_depth,                 //(INPUT)  depth of roots (mm)
                               float sw_demand,                  //(INPUT)  total crop demand for water
                               float *sw_supply,                  //(INPUT)  potential water to take up
                               float *dlt_slai_water);            // (OUTPUT) water stress senescense

void crop_leaf_area_sen_light2 (float radn_int,                //(INPUT)
                                float radn,                    //(INPUT)
                                float sen_radn_crit,           //(INPUT)
                                int   year,                    //(INPUT)
                                int   day_of_year,             //(INPUT)
                                float *lai_equilib_light,      //(INPUT)
                                float lai,                     //(INPUT)
                                float sen_light_time_const,    //(INPUT)
                                float *dlt_slai_light);         //(OUTPUT) lai senesced by low light

void crop_leaf_area_sen_age1 (int emergence,                   //(INPUT)  emergence stage no.
                              int this_stage,                  //(INPUT)  This current stage
                              float g_dlt_lai_stressed,        //(INPUT)  potential change in live
                              float g_dlt_leaf_no,             //(INPUT)  actual fraction of oldest leaf
                              float g_dlt_leaf_no_dead,        //(INPUT)  fraction of oldest green leaf
                              float g_lai,                     //(INPUT)  live plant green lai
                              float *g_leaf_area,              //(INPUT)  leaf area of each leaf (mm^2)
                              float *g_leaf_no_dead,           //(INPUT)  no of dead leaves ()
                              float g_plants,                  //(INPUT)  Plant density (plants/m^2)
                              float g_slai,                    //(INPUT)  area of leaf that senesces fro
                              float c_min_tpla,                //(INPUT)
                              float *dlt_slai_age);             //(OUTPUT) new senesced lai from phasic devel.

float crop_leaf_area_sen_light1 (float lai_sen_light,
                                float sen_light_slope,
                                float lai,
                                float plants,
                                float min_tpla);               // return lai senesced by low light

float crop_leaf_area_sen_water1 (float sen_rate_water,    //(INPUT)  slope in linear eqn relating soil wat
                                float lai,               //(INPUT)  live plant green lai
                                float swdef_photo,       //(INPUT)
                                float plants,            //(INPUT)
                                float min_tpla);          //(INPUT)

void cproc_leaf_area_sen1 (int emergence,                 // (INPUT)  emergence stage no.
                           int this_stage,                // (INPUT)  This current stage
                           float g_dlt_lai_stressed,      // (INPUT)  potential change in live
                           float g_dlt_leaf_no,           // (INPUT)  actual fraction of oldest leaf
                           float g_dlt_leaf_no_dead,      // (INPUT)  fraction of oldest green leaf
                           float g_lai,                   // (INPUT)  live plant green lai
                           float *g_leaf_area,            // (INPUT)  leaf area of each leaf (mm^2)
                           float *g_leaf_no_dead,         // (INPUT)  no of dead leaves ()
                           float g_plants,                // (INPUT)  Plant density (plants/m^2)
                           float g_slai,                  // (INPUT)  area of leaf that senesces fro
                           float c_min_tpla,              // (INPUT)
                           float *g_dlt_slai_age,         // (OUTPUT) new senesced lai from phasic devel.
                           float c_lai_sen_light,         // (INPUT)
                           float c_sen_light_slope,       // (INPUT)
                           float *g_dlt_slai_light,       // (OUTPUT)
                           float c_sen_rate_water,        // (INPUT)
                           float g_swdef_photo,           // (INPUT)
                           float *g_dlt_slai_water,       // (OUTPUT)
                           float *cXTempSenescence,       // (INPUT)
                           float *cYSenescenceFac,        // (INPUT)
                           int c_num_temp_senescence,     // (INPUT)
                           float g_mint,                  // (INPUT)
                           float *g_dlt_slai_frost,       // (OUTPUT)
                           float *g_dlt_slai);            // (OUTPUT)

//void cproc_leaf_area_init1 (float c_initial_tpla,     //(INPUT)  initial plant leaf area (mm^2)
//                            int   init_stage,         //(INPUT)  initialisation stage
//                            float g_current_stage,    //(INPUT)  current phenological stage
//                            float *g_days_tot,        // (INPUT)  duration of each phase (days)
//                            float  g_plants,          // (INPUT)  Plant density (plants/m^2)
//                            float *lai);               //(OUTPUT) total plant leaf area

void cproc_lai_detachment1 (float c_sen_detach_frac,           //(INPUT)
                            float g_slai,                       //(INPUT)
                            float *g_dlt_slai_detached);    //(OUTPUT)

void cproc_canopy_height (float g_canopy_height,          // (INPUT)  canopy height (mm)
                          float *pXStemWt,                //(INPUT)
                          float *pYHeight,                // (INPUT)
                          int   p_num_stem_wt,            // (INPUT)
                          float *g_dm_green,              //(INPUT)  live plant dry weight (biomass
                          float g_plants,                 // (INPUT)  Plant density (plants/m^2)
                          int   stem,                     // (INPUT)  plant part no for stem
                          float *dlt_canopy_height);       //(OUTPUT) canopy height change (mm)

//void cproc_leaf_no_init1 (float c_leaf_no_at_emerg,       //(INPUT)  leaf number at emergence ()
//                          float g_current_stage,          //(INPUT)  current phenological stage
//                          int   emerg,                    //(INPUT)  emergence stage no
//                          float *g_days_tot,               //(INPUT)  duration of each phase (days)
//                          float *leaf_no,                  //(OUTPUT) initial leaf number
//                          float *node_no);                  //(OUTPUT) initial node number
//
void cproc_leaf_no_pot1 (interpolationFunction &,
                         interpolationFunction &,
                         bool   inNodeFormationPhase,
                         bool   inEmergenceDay,
                         float  node_no_now,                // (INPUT) current number of nodes
                         float  g_dlt_tt,                   // (input)
                         float *dlt_leaf_no_pot,            // (OUTPUT) new fraction of oldest expanding leaf
                         float *dlt_node_no_pot);            // (OUTPUT) new fraction of oldest expanding node on main stem

void cproc_leaf_no_pot4 (interpolationFunction &,
                         interpolationFunction &,
                         float cNodeAppRatePrePhoto,
                         float cNodeAppRatePostPhoto,
                         bool   inNodeFormationPhase,
                         bool   inPrePhotoPhyllochronPhase,
                         bool   inPhotoPhyllochronPhase,
                         bool   inPostPhotoPhyllochronPhase,
                         bool   inEmergenceDay,
                         float  photoperiod,                // (INPUT)
                         float  node_no_now,                // (INPUT) current number of nodes
                         float  g_dlt_tt,                   // (input)
                         float  *g_leaves_per_node,
                         float *dlt_leaf_no_pot,            // (OUTPUT) new fraction of oldest expanding leaf
                         float *dlt_node_no_pot);            // (OUTPUT) new fraction of oldest expanding node on main stem

float cproc_leaf_area_pot1 (interpolationFunction &leaf_size_fn, //(INPUT)  leaf size for lookup
                           float g_node_no,                    //(INPUT)  node number
                           float c_node_no_correction,         //(INPUT)  corrects for other growing lea
                           float  g_dlt_leaf_no_pot,            //(INPUT)  potential fraction of oldest l
                           float  g_plants);                     //(INPUT)  Plant density (plants/m^2)

float cproc_phyllochron(interpolationFunction &,
                        float *dlt_node_no_pot);            // (OUTPUT) new fraction of oldest expanding node on main stem

float cproc_leaf_area_stressed1 (float  g_dlt_lai_pot,         //(INPUT)
                                float  g_swdef_expansion,     //(INPUT)
                                float  g_nfact_expansion);    //(INPUT)

void cproc_leaf_area_actual1 (interpolationFunction &sla_max_fn,
                              float  dlt_dm_leaf,        //(INPUT)  leaf biomass growth (g/m^2)
                              float *g_dlt_lai,          //(OUTPUT)  actual change in live plant la
                              float  g_dlt_lai_stressed, //(INPUT)  potential change in live
                              float  g_lai);              //(INPUT)  live plant green lai

void cproc_leaf_no_pot2 (float *c_x_node_no_app,       //(INPUT)
                         float *c_y_node_app_rate,     //(INPUT)
                         float *c_y_leaves_per_node,   //(INPUT)
                         int c_num_node_no_app,       //(INPUT)
                         float g_current_stage,       //(INPUT)  current phenological stage
                         int start_node_app,          //(INPUT)  stage of start of leaf appeara
                         int end_node_app,            //(INPUT)  stage of end of leaf appearanc
                         int emerg,                   //(INPUT)  emergence stage
                         float *g_days_tot,            //(INPUT)  duration of each phase (days)
                         float g_dlt_tt,              //(INPUT)  daily thermal time (growing de
                         float *g_node_no,             //(INPUT)  number of fully expanded nodes
                         float *dlt_leaf_no_pot,       //(OUTPUT) new fraction of oldest expanding leaf
                         float *dlt_node_no_pot);       //(OUTPUT) new fraction of oldest expanding node on main stem

void cproc_tpla_max (float g_leaf_no_final,            //(INPUT)final leaf number
                     float g_tiller_no_fertile,        //(INPUT)number of fertile tillers
                     float c_tiller_coef,              //(INPUT)tiller coefficient on TPLAmax
                     float p_main_stem_coef,           //(INPUT)main stem coefficient on TPLAmax
                     float *tpla_max);                  //(OUTPUT) maximum total plant leaf area mm^2

void cproc_leaf_area_pot_tpla (int  begin_stage,                 // (INPUT)  stage number of start leaf area growth
                               int  end_stageTPLAPlateau,        // (INPUT)  stage number to stop TPLA growth
                               int  now,                         // (INPUT)  stage number now = max_stage + 1
                               float *g_phase_tt,                // (INPUT)  required thermal time between stages
                               float *g_tt_tot,                  // (INPUT)  elapsed thermal time between stages
                               float *g_days_tot,                // (INPUT)  elapsed days between stages
                               float  g_current_stage,           // (INPUT)  current stage
                               float  c_initial_tpla,            // (INPUT)  initial total plant area (mm2)
                               float  tpla_max,                  // (INPUT)  maximum total plant area (mm2)
                               float  c_tpla_inflection_ratio,   // (INPUT)  fraction of thermal time from begin to
                                                                 //          end leaf area growth where inflexion on
                                                                 //          TPLA occurs
                               float  *g_tpla_today,              // (OUTPUT)  today's total leaf area per plant (mm2)
                               float  g_tpla_yesterday,          // (INPUT)  yesterday's TPLA (mm2)
                               float  p_tpla_prod_coef,          // (INPUT)  TPLA production coefficient (initial slope)
                               float  g_plants,                  // (INPUT)  Plant density (plants/m2)
                               float  *g_lai,                     // (OUTPUT)  current leaf area index()
                               float *g_dlt_lai_pot);             // (OUTPUT) change in leaf area

float leaf_size_bellshapecurve (float c_x0_const,
                                float c_x0_slope,
                                float g_leaf_no_final,
                                float c_y0_const,
                                float c_y0_slope,
                                float cAConst,
                                float cASlope1,
                                float cASlope2,
                                float cBConst,
                                float cBSlope1,
                                float cBSlope2,
                                float leaf_no);         //(INPUT) nominated leaf number

void cproc_leaf_area_pot_bellshapecurve (int  begin_stage,             //
                                         int  now,                     //
                                         float *g_leaf_no,             //
                                         float  c_leaf_no_correction,  //
                                         float  c_x0_const,            //
                                         float  c_x0_slope,            //
                                         float  g_leaf_no_final,       //
                                         float  c_y0_const,            //
                                         float  c_y0_slope,            //
                                         float  cAConst,               //
                                         float  cASlope1,              //
                                         float  cASlope2,              //
                                         float  cBConst,               //
                                         float  cBSlope1,              //
                                         float  cBSlope2,              //
                                         float  g_dlt_leaf_no,         //
                                         float  g_plants,              //
                                         float  g_swdef_expansion,     //
                                         float *dlt_lai_pot);           // (OUTPUT) change in leaf area

void plant_leaf_detachment (float *leaf_area
			        , float dlt_slai_detached
			        , float plants
			        , int max_node);

void plant_leaf_removal_top (float *leaf_area
                           , float dlt_lai_removed
                           , float plants
                           , float *last_node);
//---------------------------------------------------------------------------
  void legnew_cover (
    float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float canopy_fac
    ,float g_lai
    ,float *g_cover_green
    ) ;
  void legnew_extinct_coef
    (
     float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float *extinct_coef
    ) ;

  float legnew_node_no_from_area
    (
     float *g_leaf_area
    ,int   num_nodes
    ,float pla
    ) ;
  float legnew_leaf_no_from_area (
     float  *g_leaf_area
    ,float  *g_leaf_no
    ,int    num_nodes
    ,float  pla
    ) ;


//--------------------------------------------------------------------------
  void cproc_n_uptake3
    (
     float  *g_dlayer
    ,float  *g_no3gsm_uptake_pot
    ,float  *g_nh4gsm_uptake_pot
    ,float  g_n_fix_pot
    ,const char   *c_n_supply_preference
    ,float  n_demand
    ,float  n_max
    ,float  g_root_depth
    ,float  *dlt_no3gsm
    ,float  *dlt_nh4gsm
    ) ;
  void cproc_n_supply3 (
     float  g_dlayer[]
    ,float  g_no3gsm[]
    ,float  g_no3gsm_min[]
    ,float  *g_no3gsm_uptake_pot
    ,float  g_root_depth
    ,float  *g_root_length
    ,float  g_bd[]
    ,float  c_total_n_uptake_max
    ,float  c_no3_uptake_max
    ,float  c_no3_conc_half_max
    ,float  *g_sw_avail_pot
    ,float  *g_sw_avail
    ,bool   inNStressPhase
    ) ;
void cproc_n_supply4 (float* g_dlayer  //  ! (INPUT)
               ,float* g_bd                   //
               ,float* g_NO3gsm               //  ! (INPUT)
               ,float* g_NO3gsm_min           //  ! (INPUT)
               ,float* G_no3gsm_uptake_pot    //
               ,float* g_NH4gsm               //  ! (INPUT)
               ,float* g_NH4gsm_min           //  ! (INPUT)
               ,float* G_nH4gsm_uptake_pot    //
               ,float g_root_depth            //  ! (INPUT)
               ,float c_kno3                   //
               ,float c_no3ppm_min
               ,float c_knh4                   //
               ,float c_nh4ppm_min
               ,float c_total_n_uptake_max    //
               ,float* g_sw_avail_pot         //  ! (INPUT)
               ,float* g_sw_avail             //  ! (INPUT)
               ,bool   inNStressPhase
               );
void legnew_n_senescence1
    (
     int    num_part
    ,float  *c_n_sen_conc
    ,float  *g_dlt_dm_senesced
    ,float  *g_n_green
    ,float  *g_dm_green
    ,float  *dlt_n_senesced_trans
    ,float  *dlt_n_senesced) ;


void crop_n_detachment(const int num_part,
                       const int root,
                       const int leaf,
                       float dm_leaf_detach_frac,  // (INPUT)  fraction of senesced leaf dry matter detaching from live plant each day (0-1)
                       float *dlt_N_senesced,      //  (INPUT)  actual N loss with senesced plant (g/m^2)
                       float *dlt_N_detached);      // (OUTPUT) actual nitrogen senesced  from plant parts (g/m^2)

void cproc_n_detachment1(const int max_part,
                         float *c_sen_detach_frac,
                         float *g_n_senesced,
                         float *g_dlt_n_detached,
                         float *c_dead_detach_frac,
                         float *g_n_dead,
                         float *g_dlt_n_dead_detached);

float crop_n_dlt_grain_conc(const int grain,
                            float sfac_slope,      //(INPUT)  soil water stress factor slope
                            float sw_fac_max,      //(INPUT)  soil water stress factor maximum
                            float temp_fac_min,    //(INPUT)  temperature stress factor minimum optimum temp
                            float tfac_slope,      //(INPUT)  temperature stress factor slope
                            float maxt,            //(INPUT)  maximum air temperature (oC)
                            float mint,            //(INPUT)  maximum air temperature (oC)
                            float nfact_grain_conc,// (INPUT)
                            float *n_conc_crit,     //(INPUT)  critical N concentration (g N/g biomass)
                            float *n_conc_min,      //(INPUT)  minimum N concentration (g N/g biomass)
                            float swdef_expansion); // (INPUT)

void crop_n_retrans_avail(const int num_part,
                          const int root,
                          const int grain,
                          float *g_N_conc_min,
                          float *g_dm_green,
                          float *g_N_green,
                          float *N_avail);

void cproc_n_senescence1 (const int num_part,         //(INPUT) number of plant part
                          float *c_n_sen_conc,         //(INPUT)  N concentration of senesced materia (g/m^2)
                          float *g_dlt_dm_senesced,   //(INPUT)  plant biomass senescence (g/m^2)
                          float *g_n_green,           //(INPUT) nitrogen in plant material (g/m^2)
                          float *g_dm_green,          //(INPUT) plant material (g/m^2)
                          float *dlt_N_senesced);      // (OUTPUT) actual nitrogen senesced from plant parts (g/m^2)

void cproc_n_uptake1(float C_no3_diffn_const,   //(INPUT)  time constant for uptake by di
                     float *G_dlayer,            //(INPUT)  thickness of soil layer I (mm)//
                     float *G_no3gsm_diffn_pot,  //(INPUT)  potential NO3 (supply) from so
                     float *G_no3gsm_mflow_avail,// (INPUT)  potential NO3 (supply) from
                     float G_n_fix_pot,         //(INPUT) potential N fixation (g/m2)
                     const char *C_n_supply_preference, //(INPUT)
                     float n_demand,                //(INPUT)  critical plant nitrogen demand
                     float n_max,                   //(INPUT)  maximum plant nitrogen demand
                     float G_root_depth,              // (INPUT)  depth of roots (mm)
                     float *dlt_no3gsm);                // (OUTPUT) actual plant N uptake from NO3 in each layer (g/m^2

void cproc_n_supply1 (float *G_dlayer,                   // (INPUT)
                      float *G_dlt_sw_dep,               // (INPUT)
                      float *G_NO3gsm,                   // (INPUT)
                      float *G_NO3gsm_min,               // (INPUT)
                      float G_root_depth,                // (INPUT)
                      float *G_sw_dep,                   // (INPUT)
                      float *G_NO3gsm_mflow_avail,       // (OUTPUT)
                      float *G_sw_avail,                 // (INPUT)
                      float *G_NO3gsm_diffn_pot);                // (INPUT)

void crop_n_mass_flow1(const int num_layer,          // (INPUT)  number of layers in profile
                       float *dlayer,                // (INPUT)  thickness of soil layer I (mm)
                       float *dlt_sw_dep,            // (INPUT)  water uptake in each layer (mm water)
                       float *no3gsm,                // (INPUT)  nitrate nitrogen in layer L (g N/m^2)
                       float *no3gsm_min,            // (INPUT)  minimum allowable NO3 in soil (g/m^2)
                       float root_depth,             // (INPUT)  depth of roots (mm)
                       float *sw_dep,                // (INPUT)  soil water content of layer L (mm)
                       float *NO3gsm_mflow_pot);      // (OUTPUT) potential plant NO3 uptake (supply) g/m^2 by mass flow

void crop_n_diffusion1 (const int num_layer,      // (INPUT)  number of layers in profile
                        float *dlayer,            // (INPUT)  thickness of soil layer I (mm)
                        float *no3gsm,            // (INPUT)  nitrate nitrogen in layer L (g N/m^
                        float *no3gsm_min,        // (INPUT)  minimum allowable NO3 in soil (g/m^
                        float root_depth,         // (INPUT)  depth of roots (mm)
                        float *sw_avail,          // (INPUT)  actual extractable soil water (mm)
                        float *sw_avail_pot,      // (INPUT)  potential extractable soil water (m
                        float *NO3gsm_diffn_pot);  // (OUTPUT) potential plant NO3 by diffusion

void cproc_n_demand1(const int max_part,          // (INPUT)
                     int   *demand_parts,         // (INPUT)
                     const int num_demand_parts,  // (INPUT)
                     float G_dlt_dm,              // (INPUT)  the daily biomass production (
                     float *G_dlt_dm_green,        // (INPUT)  plant biomass growth (g/m^2)
                     float G_dlt_dm_pot_rue,      // (INPUT)  potential dry matter productio
                     float *G_dlt_n_retrans,       // (INPUT)  nitrogen retranslocated out fr
                     float *G_dm_green,            // (INPUT)  live plant dry weight (biomass
                     float *G_n_conc_crit,         // (INPUT)  critical N concentration (g N/
                     float *G_n_conc_max,          // (INPUT)  maximum N concentration (g N/g
                     float *G_n_green,             // (INPUT)  plant nitrogen content (g N/m^
                     float *N_demand,             // (OUTPUT) critical plant nitrogen demand g/m^2)
                     float *N_max);                // (OUTPUT) max plant nitrogen demand  (g/m^2)

void cproc_n_init1(float *C_n_init_conc,  // (INPUT)  initial N concentration (
                   int    max_part,
                   float *G_dm_green,     // (INPUT)  live plant dry weight (biomass
                   float *N_green);        // plant nitrogen (g/m^2)

void crop_n_dead_detachment(int num_part,
                            float *dead_detach_frac, //(INPUT)  fraction of dead plant parts detaching each day (0-1)
                            float *n_dead,           //(INPUT)  plant N content of dead plants (g N/m^2)
                            float *dlt_N_dead_detached);  //(OUTPUT) change in dm of dead plants (g/m^2)

//---------------------------------------------------------------------------

void crop_store_value(int day_of_year,
                               int year,
                               float *array,
                               float value);

float crop_running_ave(int day_of_year,
                       int year,
                       float *array,
                       int number_of_days);

void crop_part_fraction_delta (int    part_no,
                               float *fraction,         // (INPUT)  fraction for each part
                               float part,              // (INPUT)  part value to use
                               float *dlt_part);        // (OUTPUT) change in part

void crop_part_fraction_delta (float fraction,         // (INPUT)  fraction for each part
                               float part,              // (INPUT)  part value to use
                               float *dlt_part);        // (OUTPUT) change in part

void crop_pool_fraction_delta (int num_part,         // (INPUT)  number of plant parts
                               float *fraction,      // (INPUT)  fraction of pools to detach
                               float *pool,          // (INPUT)  plant pool for detachment (g/m^2)
                               float *dlt_pool);      // (OUTPUT) change in plant pool


void cproc_sw_demand1(float dlt_dm_pot_rue,      //(INPUT)  potential dry matter production with opt
                      float transp_eff,          //(INPUT)  transpiration efficiency (g dm/m^2/mm wa
                      float *sw_demand);          //(OUTPUT) crop water demand (mm)

void cproc_sw_demand_bound (float sw_demand_unbounded,  //(INPUT)  Unbounded sw demand (mm)
                            float eo_crop_factor,       //(INPUT) crop factor for eo   (-)
                            float eo,                   //(INPUT) eo                  (mm)
                            float cover_green,          //(INPUT) green crop cover    (-)
                            float *sw_demand_bounded);    //(OUTPUT) bounded sw demand (mm)

float vpd(float svp_fract, float maxt, float mint); //(INPUT)

void cproc_transp_eff1(float svp_fract,          // (INPUT)  fraction of distance between svp at mi
                       float transp_eff_cf,     //  (INPUT)  transpiration efficiency coefficien
                       float maxt,               // (INPUT)  maximum air temperature (oC)
                       float mint,               // (INPUT)  minimum air temperature (oC)
                       float *transp_eff);       //   (OUTPUT)

void plant_bio_water1(float sw_supply        //(INPUT)  potential water to take up (supply)
                    , float transp_eff       //(INPUT)  transpiration efficiency (g dm/m^2/m
                    , float *dlt_dm_pot_te); //(OUTPUT) potential dry matter production
                                             //         by transpiration (g/m^2)

void cproc_bio_water1(int   num_layer,      //(INPUT)  number of layers in profile
                      float *dlayer,        //(INPUT)  thickness of soil layer I (mm)
                      float root_depth,    //(INPUT)  depth of roots (mm)
                      float *sw_supply,     //(INPUT)  potential water to take up (supply)
                      float transp_eff,    //(INPUT)  transpiration efficiency (g dm/m^2/m
                      float *dlt_dm_pot_te); //(OUTPUT) potential dry matter production
                                            //         by transpiration (g/m^2)

void cproc_transp_eff_co2(float svp_fract,        // (INPUT)  fraction of distance between svp at mi
                          float transp_eff_cf,    // (INPUT)  transpiration efficiency coefficien
                          float maxt,             // (INPUT)  maximum air temperature (oC)
                          float mint,             // (INPUT)  minimum air temperature (oC)
                          float co2level,         // (INPUT)  current co2 level (ppm)
                          float *co2_level_te,     // (INPUT)  co2 levels (ppm)
                          float *te_co2_modifier,  // (INPUT)  te modifiers of co2 levels (0-1)
                          int   num_co2_level_te,   // (INPUT)  number of table elements in co2-te modifier table
                          float *transp_eff);       // (OUTPUT) transpiration coefficient

void cproc_transp_eff_co2_1(float vpd,        // (INPUT)
                          float transp_eff_cf,    // (INPUT)  transpiration efficiency coefficien
                          float co2_modifier,     // (INPUT)  te modifier of co2 level (0-1)
                          float *transp_eff);       // (OUTPUT) transpiration coefficient

double divide (double dividend, double divisor, double default_value);
float l_bound (float var, float lower);
float u_bound (float var, float upper);

void bound_check_real_array (ScienceAPI& scienceAPI,
                             float *array,// (INPUT) array to be checked
                             int    array_size,    // (INPUT) array size_of
                             float  lower_bound,// (INPUT) lower bound of values
                             float  upper_bound,// (INPUT) upper bound of values
                             const char *array_name);// (INPUT) key string of array
void bound_check_integer_array (ScienceAPI& scienceAPI,
                             int *array,// (INPUT) array to be checked
                             int    array_size,    // (INPUT) array size_of
                             int  lower_bound,// (INPUT) lower bound of values
                             int  upper_bound,// (INPUT) upper bound of values
                             const char *array_name);// (INPUT) key string of array

void bound_check_real_var (ScienceAPI& scienceAPI,float value, float lower, float upper, const char *vname);
void bound_check_integer_var (ScienceAPI& scienceAPI, int value, int lower, int upper, const char *vname);

float bound(float var, float lower, float upper);

float error_margin(const float *value);

void fill_real_array (float *var, float value, int limit);
void fill_integer_array (int *var, int value, int limit);

int get_cumulative_index_real(float cum_sum, const float *array, int size_of);
template <class T> T sum(const vector<T> v) {
   T result = 0.0;
   for (unsigned int i = 0; i < v.size();  i++) result += v[i];
   return result;
};
template <class T> T sum(const vector<T> &v, unsigned int n) {
   T result = 0.0;
   for (unsigned int i = 0; i < v.size() && i < n;  i++) result += v[i];
   return result;
};

template <class T> void setTo(vector<T> &v, T value) {
   for (unsigned int i = 0; i < v.size(); i++) v[i] = value;
};

float sum_real_array (float *var, int nelem);
float sum_integer_array (int *var, int limit);
int position_in_real_array(float Number,      //(INPUT) Number to search for
                           float *Array,      //(INPUT) Array to search
                           int Array_size);    //(INPUT) Number of elements in array

//int position_in_char_array(char *Value, char *Array, int *Array_size);

float sum_between (int start, int finish, const float *array);
float sum_part_of_real(float *array,     // array to be summed
                       int start,        // index for starting element
                       int stop,         // index for stopping element
                       int size_of);      //  size of array

int count_of_real_vals (float *array, int limit);


int offset_day_of_year (int iyr,     //(INPUT) day of year number
                        int doy,     //(INPUT) year
                        int ndays);   //(INPUT) number of days to adjust by
bool leap_year(int *year);
bool check_date (int day, int month, int year);
double date_to_jday (int day, int month, int year);
void   jday_to_date (int *day, int *month, int *year, double jday);

//---------------------------------------------------------------------------

float linear_interp_real (float x, const float *x_cord, const float *y_cord, int num_cord);

bool stage_is_between (int start, int finish, float current_stage);

int find_layer_no(float depth, float *dlayr, int num_layers);
int find_layer_no(float depth, const std::vector<float> &dlayr);

bool on_day_of (int stage_no, float current_stage);

float root_proportion (int    layer,              // (INPUT) layer to look at
                       float *dlayr,              // (INPUT) array of layer depths
                       float  root_depth);         // (INPUT) depth of roots

int stage_no_of (float stage_code,           //(INPUT) stage code to look up
                 float *stage_code_list,     //(INPUT) list of stage codes
                 int   list_size);            //(INPUT) size_of of stage code list

void accumulate (float value,             //  (INPUT) value to add to array
                 float *array,            //  (INPUT/OUTPUT) array to split
                 float p_index,           //  (INPUT) current p_index no
                 float dlt_index);         //  (INPUT) increment in p_index no

void subtract_real_array (const float *amount, float *store, int count);

void add_real_array (const float *amount, float *store, int count);

void crop_radn_int0(float cover_green,
                    float fr_intc_radn,
                    float radn,
                    float *radn_int);

float day_length (int day_of_year, float latitude, float twilight);

float linint_3hrly_temp (float tmax, float tmin, externalFunction *ttFn);
float temp_3hr (float tmax, float tmin, int period);

void jday_to_day_of_year(double*, int*, int*);

inline bool leap_year(int year) {return leap_year(&year);};

inline bool reals_are_equal(float A, float B, float C) {return(fabs(A-B)<C);}
inline bool isEqual(float A, float B, float C) {return(fabs(A-B)<C);}

inline bool reals_are_equal(float A, float B) {return(fabs(A-B)<1.0E-6);}
inline bool isEqual(float A, float B) {return(fabs(A-B)<1.0E-6);}


std::string ftoa(double Float, char *fmtwidth);
std::string itoa(int value, int width);

bool char2any(const char *str, int &value) ;
bool char2any(const char *str, float &value);

std::string any2string(float value) ;
std::string any2string(int value) ;

#endif
