#ifndef SoilH
#define SoilH

class Soil
   {
   public:
      ~Soil(){};
      Soil(ScienceAPI& scienceAPI, plantInterface& plant);
      void onInit1(protocol::Component *system);
      void onNewProfile(protocol::NewProfileType &v);
      void ZeroDeltas(void);
      void Read(void);
      void write();
      void getOtherVariables();
      float waterUptake (void);
      float WFPS(int layer);
      void UpdateOtherVariables(string uptake_source);
      void plant_nit_supply(float root_depth, float *root_lengt);
      float pesw(int depth);
      float fasw(int depth);
      float peswTotal(float root_depth);
      float swSupply(float root_depth);
      float NSupply(float root_depth);
      void doWaterUptake (string uptake_source, string crop_type, float SWDemand, float root_depth);
      void doNUptake(string uptake_source, string crop_type, float root_depth, float sumNMax, float sumSoilNDemand, float nDemand, float n_fix_pot);
      float swAvailable(float root_depth);


      float dlayer[max_layer];                         // thickness of soil layer I (mm)
      int num_layers;
      int find_layer_no(float);
      float root_proportion (int layer, float root_depth);
      float layer_fasw(int layerNo);

      vector<float> xf;                                 // root exploration factor (0-1)
      vector<float> ll_dep;                          // lower limit of plant-extractable
      float dul_dep[max_layer];                        // drained upper limit soil water content for soil layer L (mm water)

      float sw_dep[max_layer];                         // soil water content of layer L (mm)
      float dlt_sw_dep[max_layer];                      // water uptake in each layer (mm water)
      float sw_avail_pot[max_layer];                    // potential extractable soil water (mm)

      float no3gsm [max_layer];                         // nitrate nitrogen in layer L (g N/m^2)
      float nh4gsm [max_layer];                         // nitrate nitrogen in layer L (g N/m^2)
      float no3gsm_uptake_pot[max_layer];
      float nh4gsm_uptake_pot[max_layer];
      float dlt_nh4gsm[max_layer];                      // actual NH4 uptake from soil (g/m^2)
      float dlt_no3gsm[max_layer];                      // actual NO3 uptake from soil (g/m^2)

      float sw_supply [max_layer];                      // potential water to take up (supply)
                                                        // from current soil water (mm)

   private:
      void zero(void);


      void doSWAvailable(float root_depth);
      void doSWSupply(float root_depth);
      void doPotentialExtractableSW(float root_depth);
      void doWaterSupply (float root_depth);
      void doWaterUptakeInternal (float sw_demand, float root_depth);


      void plant_get_ext_uptakes (const char *uptake_source,        //(INPUT) uptake flag
                           const char *crop_type,            //(INPUT) crop type name
                           const char *uptake_type,          //(INPUT) uptake name
                           float unit_conversion_factor,     //(INPUT) unit conversion factor
                           float uptake_lbound,              //(INPUT) uptake lower limit
                           float uptake_ubound,              //(INPUT) uptake upper limit
                           float *uptake_array);              //(OUTPUT) crop uptake array



      void doWaterUptakeExternal (string uptake_source, string crop_type);

      void crop_check_sw(float minsw, float *dlayer, float *dul_dep, float *sw_dep);






      float ll15_dep[max_layer];
      float sat_dep[max_layer];
      float bd[max_layer];




      float sw_avail[max_layer];                        // actual extractable soil water (mm)


      float no3gsm_mflow_avail[max_layer];              // potential NO3 (supply) from soil (g/m^2) by mass flow
      float no3gsm_diffn_pot[max_layer];                // potential NO3 (supply) from soil (g/m^2), by diffusion


      float nh4gsm_mflow_avail[max_layer];              // potential NH4 (supply) from soil (g/m^2) by mass flow
      float nh4gsm_diffn_pot[max_layer];                // potential NH4 (supply) from soil (g/m^2), by diffusion


      int find_layer_no(float,const vector<float>&);
      int find_layer_no(float, float *, int);


      float sw_lb;                                      // lower limit of soilwater  (mm/mm)
      float sw_ub;                                      // upper limit of soilwater  (mm/mm)
      float sw_dep_ub;                                  // upper limit of soilwater depth (mm)
      float sw_dep_lb;                                  // lower limit of soilwater depth (mm)
      float kl_ub;                                      // upper limit of water uptake factor
      float no3_ub;                                     // upper limit of soil NO3 (kg/ha)
      float no3_lb;                                     // lower limit of soil NO3 (kg/ha)
      float nh4_ub;                                     // upper limit of soil NH4 (kg/ha)
      float nh4_lb;                                     // lower limit of soil NH4 (kg/ha)
      int   n_uptake_option;
      float no3_uptake_max;
      float no3_conc_half_max;
      float kno3;
      float no3ppm_min;
      float knh4;
      float nh4ppm_min;
      float total_n_uptake_max;
      string n_supply_preference;                        // preference of n supply
      float no3_diffn_const;                            // time constant for uptake by
                                                        // diffusion (days). H van Keulen &
                                                        // NG Seligman. Purdoe 1987. This is the
                                                        // time it would take to take up by
                                                        // diffusion the current amount of N if
                                                        // it wasn't depleted between time steps


      vector<float> kl;                                // root length density factor for water



   protected:
      ScienceAPI& scienceAPI;
      plantInterface& plant;



   private:


   };


#endif /* SoilH */
