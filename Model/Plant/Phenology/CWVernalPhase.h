#ifndef CWVernalPhaseH
#define CWVernalPhaseH

#include "Phase.h"
class CWVernalPhase : public Phase
   // A CERES WHEAT vernalisation phase.
   {
   protected:
      float vern_eff;
      float photop_eff;
      float cumvd;
      float dlt_cumvd;
      float vern_sens;
      float photop_sens;
      float dlt_tt;
      float twilight;       // twilight in angular distance between
                            // sunset and end of twilight - altitude
                            // of sun. (deg)
      interpolationFunction y_tt;

      void vernalisation();
      float crown_temp_nwheat(float maxt, float mint, float snow);
      float wheat_vernaliz_days(float g_maxt    //Daily maximum Temperature
                                ,float g_mint    //Daily minimum temperature
                                ,float tempcr    //Crown temperature
                                ,float //g_snow    //Snow depth of the day (mm)
                                ,float g_cumvd);
      float wheat_vernaliz_effect(float p_vern_sens
                                  ,float cumvd
                                  ,float dlt_cumvd
                                  ,float reqvd);
      float wheat_photoperiod_effect(float photoperiod, float p_photop_sen);

      virtual float TT() {return dlt_tt;}
            
   public:
      CWVernalPhase(ScienceAPI& scienceAPI, plantInterface& p, const std::string& StartStageName, const std::string& EndStageName);

      virtual void reset(void);
      virtual void process();
      virtual void read();
   };


#endif
