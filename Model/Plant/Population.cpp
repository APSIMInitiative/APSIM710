#include "StdPlant.h"

#include "Population.h"
#include "Environment.h"
#include "CompositePart.h"
#include "Phenology/Phenology.h"
#include "Leaf/Leaf.h"
Population::Population(ScienceAPI& api, plantInterface& plant)
   : plantThing(api, "population"), Plant(plant)
   {
   SoilTemp.push_back(0);
   SoilTemp.push_back(0);
   SoilTemp.push_back(0);
   scienceAPI.expose("plants", "plants/m^2", "Plant desnity", plants);
   scienceAPI.subscribe("prepare", NullFunction(&Population::OnPrepare));
   scienceAPI.subscribe("kill_crop", KillCropFunction(&Population::OnKillCrop));
   scienceAPI.subscribe("end_crop", NullFunction(&Population::OnEndCrop));

   Zero();
   }

void Population::onInit1(protocol::Component*)
   {
   scienceAPI.read("leaf_no_crit", leaf_no_crit, 0.0f, 100.0f);
   scienceAPI.read("tt_emerg_limit", tt_emerg_limit, 0.0f, 1000.0f);
   scienceAPI.read("days_germ_limit", days_germ_limit, 0.0f, 365.0f);
   scienceAPI.read("swdf_pheno_limit", swdf_pheno_limit, 0.0f, 1000.0f);
   scienceAPI.read("swdf_photo_limit", swdf_photo_limit, 0.0f, 1000.0f);
   scienceAPI.read("swdf_photo_rate", swdf_photo_rate, 0.0f, 1.0f);
   PlantDeathTemperature.read(scienceAPI, "x_weighted_temp", "oC", 0.0, 100.0,
                                          "y_plant_death", "0-1", 0.0, 100.0);
   }

void Population::Zero()
   {
   plants = 0.0;
   dlt_plants_death_external = 0.0;
   ZeroDaily();
   das = 0;
   }

void Population::ZeroDaily()
   {
   // from zero daily.
   dlt_plants = 0.0;
   dlt_plants_death_seedling = 0.0;
   dlt_plants_death_drought = 0.0;
   dlt_plants_failure_phen_delay = 0.0;
   dlt_plants_failure_leaf_sen = 0.0;
   dlt_plants_failure_emergence = 0.0;
   dlt_plants_failure_germ = 0.0;
   }

void Population::Update()
   {
   plants = plants + dlt_plants;
   }

void Population::OnPrepare()
   {
   ZeroDaily();
   SoilTemp[2] = SoilTemp[1];
   SoilTemp[1] = SoilTemp[0];
   scienceAPI.getOptional("maxt_soil_surface", "oC", SoilTemp[0], 0.0, 80.0);
   }
void Population::OnEndCrop()
   {
   Zero();
   }

void Population::SetPlants(float Density)
   {
   plants = Density;
   bound_check_real_var(scienceAPI, plants, 0.0, 10000.0, "plants");
   }

void Population::PlantDeath()
   {
   //=========================================================================

   das++;
   if (Plant.phenology().inPhase("sowing"))
      dlt_plants_failure_germ = CropFailureGermination();

   else
      dlt_plants_failure_germ = 0.0;

   if (Plant.phenology().inPhase("germination"))
      dlt_plants_failure_emergence = CropFailureEmergence();
   else
      dlt_plants_failure_emergence = 0.0;

   dlt_plants_death_seedling = 0.0;
   if (Plant.phenology().onDayOf("emergence"))
      dlt_plants_death_seedling = DeathSeedling();

     /*XXXX this needs tou be coupled with dlt_leaf_area_sen, c_sen_start_stage  FIXME*/
   if (Plant.phenology().inPhase("above_ground"))
      dlt_plants_failure_leaf_sen = CropFailureLeafSen();
   else
      dlt_plants_failure_leaf_sen = 0.0;

   if (Plant.phenology().inPhase("preflowering"))
      dlt_plants_failure_phen_delay = CropFailurePhenDelay();
   else
      dlt_plants_failure_phen_delay = 0.0;

   if (Plant.phenology().inPhase("preflowering"))
      dlt_plants_death_drought = DeathDrought();
   else
      dlt_plants_death_drought = 0.0;

   DeathActual();

   if (reals_are_equal (dlt_plants + plants, 0.0))
      {
      //!!!!! fix problem with deltas in update when change from alive to dead ?zero deltas

      if (Plant.Status() == alive)
         {
         Plant.SetStatus(dead);
         float biomass = Plant.Tops().Total.DM() * gm2kg /sm2ha;

         // report
         char msg[80];
         sprintf(msg, "Plant death. standing above-ground dm = %.2f (kg/ha)", biomass);
         scienceAPI.write(msg);
         }
      // XX Needs to signal a need to call zero_variables here...
      // Present method is to rely on calling zero_xx at tomorrow's prepare() event.. :(
      }
   }

float Population::CropFailureGermination()

   {
   //===========================================================================
   // Crop failure from lack of germination within a specific maximum number of days.
   if (das >= days_germ_limit)
      {
      ostringstream out;
      out << "      crop failure because of lack of" << endl
          << "         germination within "
          << days_germ_limit
          << " days of sowing" << endl;
      scienceAPI.write(out.str());
      return -1.0 * plants;
      }
   return 0.0;
   }

float Population::CropFailureEmergence()
   {
   //=============================================================================
   // Crop failure from lack of emergence within a specific maximum
   // thermal time sum from germination.
   if (Plant.phenology().TTInPhase("germination") > tt_emerg_limit)
      {
      scienceAPI.write(" failed emergence due to deep planting");
      return -1.0 * plants;
      }
   return 0.0;
   }

float Population::CropFailurePhenDelay()
   {
   // Determine plant death from prolonged phenology delay.
   if (Plant.getCumSwdefPheno() >= swdf_pheno_limit)
      {
      scienceAPI.write("Crop failure because of prolonged"
                       "phenology delay through water stress.");
      return -1.0 * plants;
      }
  return 0.0;
  }
float Population::CropFailureLeafSen()
   {
   // Determine plant population death from leaf area senescing
   float leaf_area = divide (Plant.leaf().getLAI(), plants, 0.0); // leaf area per plant

   if (reals_are_equal (leaf_area, 0.0, 1.0e-6))
      {
      scienceAPI.write("Crop failure because of total leaf senescence.");
      return -1.0 * plants;
      }
   return 0.0;
   }


float Population::DeathSeedling()
   {
   //=======================================================================================
   // Determine seedling death rate due to high soil temperatures

   float killfr = DeathHighTemperature();
   float dlt_plants = - plants*killfr;

   if (killfr > 0.0)
      {
      string msg= "Plant kill. ";
      msg = msg + ftoa(killfr*fract2pcnt, ".2").c_str();
      msg = msg + "% failure because of high soil surface temperatures.";
      scienceAPI.write(msg);
      }
   return dlt_plants;
   }

float Population::DeathHighTemperature()
   {
   //=======================================================================================
   // Calculate fraction of plants killed by high temperature during
   // emergence (0-1).

   int Today = 0;
   int Yesterday = 1;
   int DayBefore = 2;

   float weighted_temp = 0.25 * SoilTemp[DayBefore]
                       + 0.50 * SoilTemp[Yesterday]
                       + 0.25 * SoilTemp[Today];

   return PlantDeathTemperature.value(weighted_temp);
   }



float Population::DeathDrought()
   {
   //=======================================================================================
   //   Determine plant death rate due to drought

   float killfr;                                 // fraction of crop population to kill
   float dlt_plants = 0.0;                       // population to kill

   if (Plant.getLeafNo() < leaf_no_crit
       && Plant.getCumSwdefPhoto() > swdf_photo_limit
        && Plant.getSwdefPhoto() < 1.0)
      {
      killfr = swdf_photo_rate * (Plant.getCumSwdefPhoto() - swdf_photo_limit);
      killfr = bound (killfr, 0.0, 1.0);
      dlt_plants = -plants*killfr;

      string msg= "Plant kill. ";
      msg = msg + ftoa(killfr*fract2pcnt, ".2").c_str();
      msg = msg + "% failure because of water stress.";
      scienceAPI.write(msg);
      }
   return dlt_plants;
   }

void Population::DeathActual()
   {
   //=======================================================================================
   //   Determine plant death rate due to a range of given processes

   // dlt's are negative so take minimum.
   float pmin = dlt_plants_failure_germ;             // Progressive minimum
   pmin = min(pmin, dlt_plants_failure_emergence);
   pmin = min(pmin, dlt_plants_failure_leaf_sen);
   pmin = min(pmin, dlt_plants_failure_phen_delay);
   pmin = min(pmin, dlt_plants_death_drought);
   pmin = min(pmin, dlt_plants_death_seedling);
   pmin = min(pmin, dlt_plants_death_external);

   dlt_plants = pmin;

   dlt_plants_death_external = 0.0;                //Ugly hack here??

   }

void Population::OnKillCrop(protocol::KillCropType& Kill)
//=======================================================================================
// Event Handler for Kill Crop Event
   {
   if (Plant.Status() != out)
      {
      bound_check_real_var(scienceAPI, Kill.KillFraction, 0.0, 1.0, "KillFraction");
      dlt_plants_death_external = dlt_plants_death_external - plants * Kill.KillFraction;

      if (Kill.KillFraction > 0.0)
         {
         string msg= "Plant kill. ";
         msg = msg + ftoa(Kill.KillFraction*fract2pcnt, ".2").c_str();
         msg = msg + "% crop killed because of external action.";
         scienceAPI.write(msg);
         }
      }
   else
      {
      string msg = Plant.Name() + " is not in the ground - unable to kill crop.";
      scienceAPI.warning(msg);
      }
   }

float Population::DyingFractionPlants(void)
   {
   float dying_fract_plants = divide (-dlt_plants, plants, 0.0);
   dying_fract_plants = bound (dying_fract_plants, 0.0, 1.0);
   return dying_fract_plants;
   }



