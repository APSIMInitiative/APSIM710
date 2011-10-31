#include "StdPlant.h"
#include "Zadok.h"

#include "Phenology.h"

Zadok::Zadok(ScienceAPI& scienceAPI, plantInterface& p)
   : plantThing(scienceAPI, "zadok"), plant(p)
   {
   scienceAPI.exposeFunction("zadok_stage", "", "Zadok's growth developmental stage",
                             FloatGetter(&Zadok::calcZadok));
   }


// XX This is heavily tied to current ini file. !!Watch Out!!
// NB. "5.2" is half way between FI and flag leaf in wheat
float Zadok::calcZadok()
   {
   float fracInCurrent = plant.phenology().fractionInCurrentPhase();
   float zadok_stage = 0.0;
   if (plant.phenology().inPhase("sowing"))
       zadok_stage = 5.0 * fracInCurrent;
   else if (plant.phenology().inPhase("germination"))
       zadok_stage = 5.0 + 5 * fracInCurrent;
   else if (plant.phenology().inPhase("emergence"))
       zadok_stage = 10.0;
   else if (plant.phenology().inPhase("end_of_juvenile") &&
            fracInCurrent <= 0.9)
      {
      float leaf_no_now = max(0.0, plant.getLeafNo() - 2.0);

      static float tillerno_y[] =   // tiller no.
           {0, 0, 5};
      static float tillerno_x[] =   // lf. no.
           {0, 5, 8};
      float tiller_no_now = linear_interp_real (leaf_no_now
                                               , tillerno_x
                                               , tillerno_y
                                               , sizeof(tillerno_x)/sizeof(float));
      if (tiller_no_now <= 0.0)
         zadok_stage = 10.0 + leaf_no_now;
      else
         zadok_stage = 20.0 + tiller_no_now;
      }
   else if (!plant.phenology().inPhase("out"))
      {
      // from senthold's archive:
      //1    2    3         4.5     5       6
      //eme  ej   eveg(fl)  anth    sgf   mat
      //10   30   43 	     65      70     9

      // from CropMod
      //                 sow ger eme  juv    fi   flag    fl st_gf end_gf mat hv_rpe end_crop
      //stage_code       = 1   2   3    4      5     6     7    8    9    10   11   12
      //Zadok_stage      = 0   5  10   10     15    40    60   71   87    90   93  100

      // Plant:
      //                 sow    ger   eme  juv    fi       fl   st_gf end_gf  mat hv_rpe  end
      //stage_code      = 1      2    3     4     5   4.9 5.4   6     7      8     9    10     11  ()     ! numeric code for phenological stages
      //                  na     na   na    na    na   30  43   65    71     87    90    100

      static float zadok_code_y[] =
           {30,   40, 65, 71, 87, 90, 100};
      static float zadok_code_x[] =
           {4.9, 5.4,  6,  7,  8,  9,  10};
      interpolationFunction zadok;
      zadok.setXY(vector<float> (zadok_code_x, zadok_code_x + sizeof(zadok_code_x)/sizeof(float)),
                  vector<float> (zadok_code_y, zadok_code_y + sizeof(zadok_code_y)/sizeof(float)));

      zadok_stage = plant.phenology().doInterpolation(zadok);
      }
   return zadok_stage;
   }

