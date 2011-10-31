#include "StdPlant.h"

#include "CWVernalPhase.h"
#include "../Environment.h"
#include "Phenology.h"

CWVernalPhase::CWVernalPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
   : Phase (scienceAPI, p, stage_name)
   {
   reset();
   }

void CWVernalPhase::reset(void)
   {
   Phase::reset();
   vern_eff = 0.0;
   photop_eff = 0.0;
   cumvd = 0.0;
   }

void CWVernalPhase::read()
   {
   Phase::read();
   scienceAPI.read("vern_sens", vern_sens, 0.0f, 10.0f);
   scienceAPI.read("photop_sens", photop_sens, 0.0f, 10.0f);
   scienceAPI.read("twilight", twilight, -90.0f, 90.0f);
   y_tt.read(scienceAPI,
               "x_temp", "oC", 0.0, 100.0,
               "y_tt", "oC days", 0.0, 100.0);

   }

void CWVernalPhase::process()
   {
   Phase::process();
   vernalisation();
   }


// vernalisation & photoperiod stresses
void CWVernalPhase::vernalisation()
   {
   float tempcr = crown_temp_nwheat (plant.environment().maxt(), plant.environment().mint(), 0.0);

   dlt_cumvd = wheat_vernaliz_days(plant.environment().maxt() ,plant.environment().mint() ,tempcr, 0.0 , cumvd);

   //maximum vernalisation requirement is 50 days
   vern_eff = wheat_vernaliz_effect(vern_sens, cumvd, dlt_cumvd, 50.0);

   float photoperiod = plant.environment().dayLength(twilight);
   photop_eff = wheat_photoperiod_effect(photoperiod, photop_sens);

   // Thermal time is calculated from crown temperature
   dlt_tt = y_tt[tempcr];

   cumvd += dlt_cumvd;
   }

// Crown temperature from nwheat
float CWVernalPhase::crown_temp_nwheat (float maxt, float mint, float snow)
   {
   // Calculate max crown temperature
   float cx;
   if (maxt < 0.)
        cx = 2.0 + maxt * (0.4 + 0.0018 * pow(snow - 15., 2));
   else
        cx = maxt;

   // Calculate min crown temperature
   float cn;
   if (mint < 0.)
        cn = 2.0 + mint * (0.4 + 0.0018 * pow(snow - 15., 2));
   else
        cn = mint;

   return ((cn+cx)/2.0);
   }

//+  Mission Statement
//     Photoperiod factor
float CWVernalPhase::wheat_photoperiod_effect(float photoperiod, float p_photop_sen)
    {
    float photop_eff = 1.0;

    if (plant.phenology().inPhase("eme2ej"))
        {
        float  photop_sen_factor = p_photop_sen * 0.002;
        photop_eff = 1. - photop_sen_factor * pow(20. - photoperiod, 2);
        photop_eff = bound (photop_eff, 0.0, 1.0);
        }
    return photop_eff;
    }

//+  Purpose
//     Calculate daily vernalisation and accumulate to g_cumvd

//+  Mission Statement
//     Calculate todays vernalization (used to affect phenology)

//+  Notes
//   Nwheat originally had the following if logic for determining whether
//   vernalisation is calculated for today
//     if     (          cumvd .lt. reqvd
//     :                        .and.
//     :       (istage .eq.emerg .or. istage .eq. germ)   )
//     :then
//
//   In order to remove the explicit value 'reqvd' and make the stages
//   more flexibile this logic was replaced. - NIH 14/07/98
float CWVernalPhase::wheat_vernaliz_days(float g_maxt    //Daily maximum Temperature
                                         ,float g_mint    //Daily minimum temperature
                                         ,float tempcr    //Crown temperature
                                         ,float //g_snow    //Snow depth of the day (mm)
                                         ,float g_cumvd)  //cumulative vernalisation days till yesterday
   {
   float dlt_cumvd = 0.0;

   if (plant.phenology().inPhase("vernalisation"))
      {
      if (g_mint < 15.0 && g_maxt > 0.0)
          {
          // Cold
          float vd,vd1,vd2;
          vd1 = 1.4 - 0.0778 * tempcr;
          vd2 = 0.5 + 13.44 / pow(g_maxt-g_mint + 3., 2) * tempcr;
          vd = min (vd1, vd2);
          dlt_cumvd = l_bound (vd, 0.0);
          }
      if (g_maxt > 30. && g_cumvd + dlt_cumvd < 10.)
          {
          // high temperature will reduce vernalization
          dlt_cumvd = - 0.5*(g_maxt - 30.);
          dlt_cumvd = - min(-(dlt_cumvd), g_cumvd);
          }
      }
   return dlt_cumvd;
   }

//+  Purpose
//     Vernalisation factor
float CWVernalPhase::wheat_vernaliz_effect(float p_vern_sens
                                           ,float cumvd
                                           ,float dlt_cumvd
                                           ,float reqvd) {
    float vfac;                // vernalization factor
    float vern_sens_fac;
    float vern_effect = 1.0;

    if (plant.phenology().inPhase("eme2ej"))
        {
        if (reqvd < 0.0) { reqvd = 50.0; }
        vern_sens_fac =  p_vern_sens* 0.0054545 + 0.0003;
        vfac = 1. - vern_sens_fac * (reqvd - (cumvd+dlt_cumvd));
        vern_effect = bound (vfac, 0.0, 1.0);
        }
    return vern_effect;
    }
