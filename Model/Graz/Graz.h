//---------------------------------------------------------------------------
#ifndef grazComponentH
#define grazComponentH
#include <string>
#include <vector>
#include <fstream>
class ScienceAPI;

// ------------------------------------------------------------------
// Main component
// ------------------------------------------------------------------
class grazComponent
   {
   public:
      grazComponent(ScienceAPI& scienceAPI);
      void onInit2(void);
//      void onTick(protocol::TimeType &Tick);
      void onPrepare(void);
      void onProcess(void);

   private:
      ScienceAPI& scienceAPI;
      int jday;                 // julian day of year
      int month;
      // States
      float stocking_rate;      // stocking rate (beasts / ha).
      float alw;                // animal live wgt. (kg)
      float green_leaf;         // daily green leaf from grasp (kg/ha)
      float dead_leaf;          // daily dead leaf from grasp (kg/ha)
      float green_stem;         // daily green stem from grasp (kg/ha)
      float dead_stem;          // daily dead stem from grasp (kg/ha)
      float grass_growth;       // daily grass growth (kg/ha)
      float acc_eaten;          // accumulated intake by animals
      float acc_growth;         // accumulated growth of sward from grasp
      float intake_restr;       // final intake restriction (0-1)
      std::string pasture_source; // The pasture or crop to eat.

      
      // Deltas
      float green_leaf_eaten;
      float green_stem_eaten;
      float dead_leaf_eaten;
      float dead_stem_eaten;
      float dlt_lwg;
      float dead_leaf_tramp;
      float dead_stem_tramp;


      bool allow_supplements; // Whether lwg is restricted by what is eaten
      float intake_util_intercept; // Parameter for feed quality
                               // restriction. Restriction of intake
                               // by animal.
      float intake_util_slope;  // Restriction of intake by animal.
      float yld_eat_restr;      // Parameter for low quality restriction.
      float summer_lwg;         // potential lwg for season
      float autumn_lwg;         // potential lwg for season
      float winter_lwg;         // potential lwg for season
      float spring_lwg;         // potential lwg for season
      float leaf_diet;          // something to do with competition
                                //  curve. see "curve_factor"
      float std_alw;            // standard alw for beast (200 kg)
      float metabol_expon;      // ???
      float prop_can_eat;       // ???
      int   acc_eaten_reset;    // Day that pool is reset

      // Internal routines
      float graz_stock_equiv (void);
      float graz_pot_lwg (void);
      float graz_comp_curve(float ndx, float a);
      
      void eat(void);
      void event(void);
      void update(void);
   };

float bound(float var, float lower, float upper);
float u_bound (float var, float upper);
float l_bound (float var, float lower);

double divide (double dividend, double divisor, double default_value);
inline bool isEqual(float A, float B, float C) {return(fabs(A-B)<C);}
inline bool isEqual(float A, float B) {return(fabs(A-B)<1.0E-6);}

#endif
