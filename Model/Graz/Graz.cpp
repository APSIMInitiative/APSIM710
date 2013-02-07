#include <sstream>
#include <math.h>

#include <ApsimShared/ApsimVersion.h>
#include <ComponentInterface2/ScienceAPI2.h>
#include <ComponentInterface2/DataTypes.h>
#include <General/math_functions.h>
#include <General/stl_functions.h>
#include <General/string_functions.h>
#include <General/date_class.h>
#include <General/StringTokenizer.h>

#include "Graz.h"
using namespace std;

const float gm2kg = 1.0/1000.0;           // constant to convert g to kg
const float kg2gm = 1000.0;               // conversion of kilograms to grams
const float ha2sm = 10000.0;              // conversion of hectares to sq metres
const float sm2ha = 1.0/10000.0;          // constant to convert m^2 to hectares

#define min(a, b)  (((a) < (b)) ? (a) : (b))
#define max(a, b)  (((a) > (b)) ? (a) : (b))

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// Create an instance of the GRAZ module
// ------------------------------------------------------------------
extern "C" grazComponent EXPORT * STDCALL createComponent(ScienceAPI2& scienceAPI)
   {
   return new grazComponent(scienceAPI);
   }
extern "C" void EXPORT STDCALL deleteComponent(grazComponent* component)
   {
   delete component;
   }
// ------------------------------------------------------------------
// initialise the GRAZ component.
// ------------------------------------------------------------------
grazComponent::grazComponent(ScienceAPI2& scienceapi)
   : scienceAPI(scienceapi)
   {
   scienceAPI.subscribe("init2", nullFunction(&grazComponent::onInit2));
   alw = 0.0;
   stocking_rate = 0.0;
   pasture_source = "";
   dlt_lwg = 0.0;
   acc_eaten = 0.0;
   tsdm_eaten = 0.0;
   acc_growth = 0.0;
   intake_restr = 0.0;
   }

// ------------------------------------------------------------------
// initialise the GRAZ component - STAGE 2.
// ------------------------------------------------------------------
void grazComponent::onInit2(void)
   {
   scienceAPI.subscribe("do_stock", nullFunction(&grazComponent::onProcess));

   //expose..  (rw)
   scienceAPI.expose("stocking_rate", "beasts/ha", "Stocking rate",   1, stocking_rate);
   scienceAPI.expose("alw",           "kg",        "Weight of beast", 1, alw);
   scienceAPI.expose("pasture_source", "",         "Crop or pasture module to eat", 1, pasture_source);
   scienceAPI.expose("summer_lwg",    "kg/hd/season",   "Potential lwg",   1, summer_lwg);
   scienceAPI.expose("autumn_lwg",    "kg/hd/season",   "Potential lwg",   1, autumn_lwg);
   scienceAPI.expose("winter_lwg",    "kg/hd/season",   "Potential lwg",   1, winter_lwg);
   scienceAPI.expose("spring_lwg",    "kg/hd/season",   "Potential lwg",   1, spring_lwg);
   scienceAPI.expose("acc_eaten",     "kg/ha",     "Amount of feed eaten",    1, acc_eaten);
   scienceAPI.expose("acc_growth",    "kg/ha",     "Cumulative grass growth", 1, acc_growth);
   scienceAPI.expose("allow_supplements", "",      "Allow supplements to be fed", 1, allow_supplements);

   //expose..  (r)
   scienceAPI.expose("lwg",          "kg",     "Live weight gain", 0,        dlt_lwg);
   scienceAPI.expose("intake_restr", "",       "Intake of feed",   0,        intake_restr);
   scienceAPI.expose("tsdm_eaten",   "kg/ha",  "Daily amount of feed eaten", 0, tsdm_eaten);

   // read ...
   scienceAPI.read("intake_util_intercept", "", 0, intake_util_intercept,(float)  0.0, (float)100.0);
   scienceAPI.read("intake_util_slope",     "", 0, intake_util_slope,    (float)-100.0,(float)100.0);
   scienceAPI.read("yld_eat_restr",         "", 0, yld_eat_restr,        (float)  0.0, (float)300.0);
   scienceAPI.read("summer_lwg",            "", 0, summer_lwg,           (float)  0.0, (float)200.0);
   scienceAPI.read("autumn_lwg",            "", 0, autumn_lwg,           (float)  0.0, (float)200.0);
   scienceAPI.read("winter_lwg",            "", 0, winter_lwg,           (float)  0.0, (float)200.0);
   scienceAPI.read("spring_lwg",            "", 0, spring_lwg,           (float)  0.0, (float)200.0);
   scienceAPI.read("leaf_diet",             "", 0, leaf_diet,            (float)  0.0, (float)100.0);
   scienceAPI.read("min_alw",               "", 0, MIN_ALW,              (float)  0.0, (float)1000.0);
   scienceAPI.read("std_alw",               "", 0, std_alw,              (float)  0.0, (float)1000.0);
   scienceAPI.read("metabol_expon",         "", 0, metabol_expon,        (float)  0.0, (float)100.0);
   scienceAPI.read("prop_can_eat",          "", 0, prop_can_eat,         (float)  0.0, (float)100.0);
   scienceAPI.read("acc_eaten_reset",       "", 0, acc_eaten_reset,      (int)  0, (int)366);
   scienceAPI.read("allow_supplements",     "", 0, allow_supplements);

   // (optional) initial values
   scienceAPI.read("stocking_rate", "beasts/ha", 1, stocking_rate,(float)  0.0, (float)100.0);
   scienceAPI.read("alw",           "kg",        1, alw,          (float)  0.0, (float)1000.0);
   scienceAPI.read("pasture_source","",          1, pasture_source);

   cout << endl;
   cout << "------- " << scienceAPI.name() << " Initialisation ";
   cout.width(79-24-scienceAPI.name().length());
   cout.fill('-');
   cout << '-' << endl;
   cout.fill(' ');
   cout << "  Initial Stocking rate: " << stocking_rate << endl;
   cout << "  Initial Live Weight: " << alw << endl;
   cout << endl;
   }


void grazComponent::onPrepare(void)
   {
   // Gather other variables
   scienceAPI.get("day", "", 0, jday, 1, 366);
   scienceAPI.get("month", "", 0, month, 1, 12);

   if (stocking_rate <= 0.0)
      {
      green_leaf = green_stem = dead_leaf = dead_stem = 0.0;
      grass_growth = 0.0;
      }
   else
      {
      std::string prefix;
      if (pasture_source == "")
         prefix = "";
      else
         prefix = pasture_source + ".";

      AvailableToAnimalType avail;
      scienceAPI.get(prefix + "AvailableToAnimal", false, avail);

      green_leaf = green_stem = dead_leaf = dead_stem = -1.0;
//        cout << "getAvail:" << endl;
      for (unsigned cohort = 0; cohort < avail.Cohorts.size(); cohort++) 
        {
//        cout << avail.Cohorts[cohort].Organ << " " << avail.Cohorts[cohort].AgeID << endl;
        if (Str_i_Eq(avail.Cohorts[cohort].Organ , "leaf") &&
            Str_i_Eq(avail.Cohorts[cohort].AgeID , "live")) 
           green_leaf = avail.Cohorts[cohort].Weight;        // comes in kg/ha
        else if (Str_i_Eq(avail.Cohorts[cohort].Organ , "stem") &&
                 Str_i_Eq(avail.Cohorts[cohort].AgeID , "live"))
           green_stem = avail.Cohorts[cohort].Weight;
        else if (Str_i_Eq(avail.Cohorts[cohort].Organ , "leaf") &&
                 Str_i_Eq(avail.Cohorts[cohort].AgeID , "dead"))
           dead_leaf = avail.Cohorts[cohort].Weight;
        else if (Str_i_Eq(avail.Cohorts[cohort].Organ , "stem") &&
                 Str_i_Eq(avail.Cohorts[cohort].AgeID , "dead")) 
           dead_stem = avail.Cohorts[cohort].Weight;
        }
      if (green_leaf < 0 || green_stem < 0 || dead_leaf < 0 || dead_stem < 0 )
         throw std::runtime_error("Missing parts in AvailableToAnimalType??");

      scienceAPI.get(prefix + "GrowthRate",         "g/m2", false, grass_growth, (float)0.0, (float)10000.0);
      grass_growth *= gm2kg / sm2ha;
   }
   // Set deltas
   green_leaf_eaten = 0.0;
   green_stem_eaten = 0.0;
   dead_leaf_eaten = 0.0;
   dead_stem_eaten = 0.0;
   tsdm_eaten = 0.0;
   dlt_lwg = 0.0;
   dead_leaf_tramp = 0.0;
   dead_stem_tramp = 0.0;
   }

void grazComponent::onProcess(void)
   {
   onPrepare(); // We collect daily growth from grasp - try and ask it _after_ it's done its calulations

   if (stocking_rate > 0.0)
      eat();

   event ();

   update ();
   }

void grazComponent::eat(void)
   {
   float green_pool, dead_pool;
   float tsdm;
   float green_prop;           // Proportion of green in tsdm (kg/ha)
   float green_prop_leaf;      // Proportion of green leaf in tsdm (kg/ha)
   float dead_prop_leaf;       // Proportion of dead leaf in tsdm (kg/ha)
   float mod_green_prop;       // adjusted green biomass ratio (0.1-1)
   float green_diet;           // proportion of green in diet
   float prop_consumed;        // (biomass eaten : biomass grown) since
                               // start of season
   float intake_restr_growth;  // restriction of intake by proportion of
                               // growth over season (0-1)
   float intake_restr_tsdm;    // restriction of intake by low level of tsdm
   float anim_intake;          // intake of biomass (kg/beast)
   float green_eaten, dead_eaten; // pool eaten     (kg/ha)
   float curve_factor;         // competition curve
   float trampled;             // trampled dead leaf + stem
   float trampled_stem;        // trampled dead stem
   float trampled_leaf;        // trampled dead leaf
   float stock_equiv;          // stock equivalent

   green_pool = green_leaf + green_stem;
   dead_pool = dead_leaf + dead_stem;
   tsdm = max(green_pool + dead_pool, 0.0);

   green_prop = divide (green_pool, tsdm, 0.0);
   green_prop_leaf = divide (green_leaf, green_pool, 0.0);
   dead_prop_leaf = divide (dead_leaf, dead_pool, 0.0);

   mod_green_prop = (green_prop - 0.10) / 0.90;
   mod_green_prop = max (mod_green_prop, (float)0.0);

   // If green is less than 10%, no active selection for green by stock
   if (mod_green_prop > 0.0) {
      green_diet = graz_comp_curve(mod_green_prop, 19.0);
   } else {
      green_diet = green_prop;
   }

   // Calculate utilization and its effects on intake
   prop_consumed = 0.0;
   if (acc_growth > 0.01) {
      prop_consumed = divide(acc_eaten, acc_growth, 0.0);
      prop_consumed = bound(prop_consumed, 0.0, 1.0);
   } 

   // restriction by proportion of growth:
   intake_restr_growth = intake_util_intercept + intake_util_slope * prop_consumed;

   // restriction by low tsdm:
   intake_restr_tsdm = divide (tsdm, yld_eat_restr, 0.0);
   intake_restr = min (intake_restr_tsdm, intake_restr_growth) ;
   intake_restr = bound (intake_restr, 0.0, 1.0);

   // This is the animal lwg model. calculate intake per head
   anim_intake = intake_restr * (graz_pot_lwg () + 1.058) / 0.304;
   anim_intake = max(anim_intake, 0.0);
   
   // Restrict intake such that the herd cannot eat more than is
   //     in sward, then adjust individual animal intake accordingly
   stock_equiv = graz_stock_equiv ();
   tsdm_eaten = bound (stock_equiv * anim_intake, 0.0, tsdm);

   anim_intake = divide(tsdm_eaten, stock_equiv, 0.0);

   //  Lwg calculation.
   if (allow_supplements) {
      // if supplementing, there is no restriction effect on LWG
      dlt_lwg = graz_pot_lwg();
   } else {
      dlt_lwg = anim_intake * 0.304 - 1.058;
   }

   //  Restrict the lwg, so that alw never goes below minimum
   // not in spaghetti !!!
   dlt_lwg = max (MIN_ALW - alw, dlt_lwg);

   curve_factor = divide (0.5 * leaf_diet - leaf_diet,
                          0.5 * leaf_diet - 0.5, 0.0);

   green_eaten = green_diet * tsdm_eaten;
   green_eaten = min(green_pool, green_eaten);
   dead_eaten = (1.0 - green_diet) * tsdm_eaten;
   dead_eaten = min(dead_pool, dead_eaten);

   green_leaf_eaten = green_eaten *
          graz_comp_curve(green_prop_leaf, curve_factor);
   green_leaf_eaten = max(min(green_leaf, green_leaf_eaten), 0.0);

   dead_leaf_eaten = dead_eaten *
          graz_comp_curve(dead_prop_leaf, curve_factor);
   dead_leaf_eaten = max(min(dead_leaf, dead_leaf_eaten), 0.0);

   green_stem_eaten = green_eaten - green_leaf_eaten;
   green_stem_eaten = max(min(green_stem, green_stem_eaten), 0.0);

   dead_stem_eaten = dead_eaten - dead_leaf_eaten ;
   dead_stem_eaten = max(min(dead_stem, dead_stem_eaten), 0.0);

   // Trampling
   trampled = tsdm_eaten * ( divide(1.0, prop_can_eat, 0.0) - 1.0);

   // Apportion to leaf and stem
   trampled_leaf = max(trampled * graz_comp_curve(dead_prop_leaf, curve_factor), 0.0);
   trampled_stem = max(trampled - trampled_leaf, 0.0);

   // Limit the trampling so that we don't trample more than is
   //  actually there.
   dead_leaf_tramp = max(min(dead_leaf - dead_leaf_eaten, trampled_leaf), (float)0.0);
   dead_stem_tramp = max(min(dead_stem - dead_stem_eaten, trampled_stem), (float)0.0);
   }

// Calculate stock equivalent
float grazComponent::graz_stock_equiv (void)
   {
   float a = divide (alw, std_alw, 0.0);
   return (stocking_rate * (pow(a, metabol_expon)));
   }

// Calculate potential lwg.
float grazComponent::graz_pot_lwg (void)
   {
   enum { SUMMER, AUTUMN, WINTER, SPRING};
   const float DAYS_PER_SEASON = 91.25;
   const int season[] = {-1,SUMMER,SUMMER,
                            AUTUMN,AUTUMN,AUTUMN,
                            WINTER,WINTER,WINTER,
                            SPRING,SPRING,SPRING,SUMMER};
   if (month < 1 || month > 12)
      throw ("Unknown month??");

   if (season[month] == SUMMER) {
       return(divide (summer_lwg, DAYS_PER_SEASON, 0.0));
   } else if (season[month] == AUTUMN) {
       return(divide (autumn_lwg, DAYS_PER_SEASON, 0.0));
   } else if (season[month] == WINTER) {
       return( divide (winter_lwg, DAYS_PER_SEASON, 0.0));
   } else if (season[month] == SPRING) {
       return(divide (spring_lwg, DAYS_PER_SEASON, 0.0));
   }
   throw ("Unknown season??");
}

float grazComponent::graz_comp_curve(float ndx, float a)
{
//*     Standard competition curve (or at least so McKeon calls it) This
//*     function is used by McKeon in several places to transform an index
//*     in the range [0-1] to another index in the same range, but
//*     weighted in a different way. The weighting is controlled by the a
//*     parameter. An a value of 1 leaves the index untransformed.
   return( divide(a * ndx, (ndx * (a - 1) + 1), 0.0));
}

void grazComponent::event (void)
   {
   if (jday == acc_eaten_reset)
      {
      acc_eaten = 0.0;
      acc_growth = green_leaf + green_stem;
      }
   }

void grazComponent::update (void)
   {
   acc_eaten +=
      green_leaf_eaten +
      green_stem_eaten +
      dead_leaf_eaten +
      dead_stem_eaten;

   acc_growth += grass_growth;

   alw += dlt_lwg;

   if (dead_leaf_eaten + green_leaf_eaten +
       dead_stem_eaten + green_stem_eaten > 0.0 )
      {
      RemovedByAnimalType dmEaten;
      RemovedByAnimalCohortsType pool;

      pool.CohortID = pasture_source;
      pool.Organ = "leaf";
      pool.AgeID = "live";
      pool.WeightRemoved = green_leaf_eaten;
      pool.Top = 0;
      pool.Bottom = 0;
      pool.Chem = "";
      dmEaten.Cohorts.push_back(pool);
      pool.AgeID = "dead";
      pool.WeightRemoved = dead_leaf_eaten;
      dmEaten.Cohorts.push_back(pool);
      pool.Organ = "stem";
      pool.AgeID = "live";
      pool.WeightRemoved = green_stem_eaten;
      dmEaten.Cohorts.push_back(pool);
      pool.AgeID = "dead";
      pool.WeightRemoved = dead_stem_eaten;
      dmEaten.Cohorts.push_back(pool);
      string s;
      if (pasture_source == "")
         s = "RemovedByAnimal";
      else
         s = pasture_source + ".RemovedByAnimal";
//cout << " eating green leaf = " << green_leaf_eaten << endl;
//cout << " eating green stem = " << green_stem_eaten << endl;
//cout << " eating dead leaf = " << dead_leaf_eaten << endl;
//cout << " eating dead stem = " << dead_stem_eaten << endl;
      scienceAPI.set(s, dmEaten);
      }

   if (dead_leaf_tramp + dead_stem_tramp > 0.0 )
      {
      RemoveCropBiomassType dmTrampled;

      RemoveCropBiomassdmType deadTrampled;
      deadTrampled.pool = "senesced";
      deadTrampled.part.push_back("leaf");
      deadTrampled.dlt.push_back(dead_leaf_tramp * kg2gm / ha2sm);   // send in g/sm
      deadTrampled.part.push_back("stem");
      deadTrampled.dlt.push_back(dead_stem_tramp * kg2gm / ha2sm);
      dmTrampled.dm.push_back(deadTrampled);

      string s;
      if (pasture_source == "")
         s = "detach_crop_biomass_rate";
      else
         s = pasture_source + ".detach_crop_biomass_rate";

      scienceAPI.publish(s, dmTrampled);
      }
   }


double divide (double dividend, double divisor, double default_value)
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values
   double LARGEST = 1.0e30;    //largest acceptable no. for quotient
   double SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   double nought = 0.0;

   //Local Varialbes
   double quotient;

   //Implementation
   if(isEqual(divisor, 0.0))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(isEqual(dividend, 0.0))      //multiplying by 0
      {
      quotient = 0.0;
      }

   else if(fabs(divisor) < 1.0)            //possible overflow
      {
      if(fabs(dividend) > fabs(LARGEST * divisor)) //overflow
         {
         quotient = default_value;
         }
      else
         {
         quotient = dividend / divisor;          //ok
         }
      }
   else if(fabs(divisor) > 1.0)             //possible underflow
      {
      if(fabs(dividend) < fabs(SMALLEST * divisor))    //underflow
         {
         quotient = nought;
         }
      else
         {
         quotient = dividend / divisor;                //ok
         }
      }
   else
      {
      quotient = dividend / divisor;                   //ok
      }
   return quotient;
   }


float bound(float var, float lower, float upper)
//===========================================================================

/*Definition
 *   Returns "lower", if "var" is less than "lower".  Returns "upper"
 *   if "var" is greater than "upper".  Otherwise returns "var".  A
 *   warning error is flagged if "lower" is greater than "upper".
 *Parameters
 *   var:   variable to be constrained
 *   lower: lower limit of variable
 *   upper: upper limit of variable
 *Calls
 *   l_bound
 *   u_bound
 *   warningError
 */

   {
   //Local variables
   float result;           // result for return
   float high;             //temporary variable constrained
                           //to upper limit of variable

   numeric_limits<float> mathInfo;
   float epsilon =  mathInfo.epsilon();

   //check that lower & upper bounds are valid
//   if (lower > upper)
   if ((lower - epsilon) > (upper + epsilon))
      {
      // bounds are invalid
      char msg[80];
      sprintf(msg,
            "Lower bound %f is > upper bound %f\n",
            lower, upper);
      throw std::invalid_argument(msg);
      }
   // constrain variable
   high = u_bound (var, upper);
   result = l_bound (high, lower);
   return result;
   }


//===========================================================================
float l_bound (float var, float lower)
//===========================================================================

/*Definition
 *   Returns "var" providing that it is greater than or equal to
 *   the lower bound, "lower".  Otherwise returns "lower".
 *Parameters
 *   var:   variable to be constrained
 *   lower: lower limit of variable
 *Calls
 *
 */

   {
   if (var < lower) return lower;
   return var;
   }

//===========================================================================
float u_bound (float var, float upper)
//===========================================================================

/*Definition
 *   Returns "var" providing that it is less than or equal to the
 *   upper bound, "upper".  Otherwise returns "upper".
 *Parameters
 *   var:   variable to be constrained
 *   upper: upper limit of variable
 *Calls
 *
 */

   {
   if (var > upper) return upper;
   return var;
   }

