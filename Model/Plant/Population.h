#ifndef PopulationH
#define PopulationH

class Population : public plantThing
   {
   private:

      float plants;                                     // Plant density (plants/m^2)
      float dlt_plants;                                 // change in Plant density (plants/m^2)
      float dlt_plants_death_seedling;
      float dlt_plants_death_drought;
      float dlt_plants_failure_phen_delay;
      float dlt_plants_failure_leaf_sen;
      float dlt_plants_failure_emergence;
      float dlt_plants_failure_germ;
      float dlt_plants_death_external;
      float leaf_no_crit;                               // critical number of leaves below
                                                        // which portion of the crop may
                                                        // die due to water stress
      float tt_emerg_limit;                             // maximum degree days allowed for
                                                        // emergence to take place (deg day)
      float days_germ_limit;                            // maximum days allowed after sowing
                                                        // for germination to take place (days)
      float swdf_pheno_limit;                           // critical cumulative phenology
                                                        // water stress above which the crop
                                                        // fails (unitless)
      float swdf_photo_limit;                           // critical cumulative photosynthesis
                                                        // water stress above which the crop
                                                        // partly fails (unitless)
      float swdf_photo_rate;                            // rate of plant reduction with
                                                        // photosynthesis water stress
      interpolationFunction PlantDeathTemperature;
      std::vector<float> SoilTemp;                      // soil surface temperature (oC)
                                                        // TODAY (index 0)
                                                        // YESTERDAY (index 1)
                                                        // DAY BEFORE YESTERDAY (index 2)

      plantInterface& Plant;
      int das;

      float DeathDrought();
      float DeathSeedling();
      void DeathActual();
      float DeathHighTemperature();

      float CropFailureGermination();
      float CropFailureEmergence();
      float CropFailurePhenDelay();
      float CropFailureLeafSen();
      void OnPrepare();
      void OnEndCrop();
      void OnKillCrop(protocol::KillCropType& Kill);
      void Zero();
      void ZeroDaily();

   public:
      Population(ScienceAPI& api, plantInterface& plant);

      virtual void onInit1(protocol::Component *);
      void Update();
      void PlantDeath();
      void SetPlants(float Density);
      float DyingFractionPlants(void);
      float Density() {return plants;}

   };



#endif
