#ifndef FloretPartH
#define FloretPartH
#include "../SimplePart.h"
#include "../Co2Modifier.h"

class FloretPart : public SimplePart {
  public:
   FloretPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name) ;
   ~FloretPart() {};
   void onInit1(protocol::Component *);
   void update(void);
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);


   void doDmDemand(float  dlt_dm_supply);
   void doProcessBioDemand(void);
   void readConstants (protocol::Component *, const string &);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   float dltDmRetranslocateSupply(float DemandDifferential) ;

   void zeroAllGlobals(void);
   void zeroDeltas(void);
   float coverTotal(void)  ;
   float coverGreen(void)  ;
   float coverSen(void)  ;
   void doCover (PlantSpatial &spatial);
   void doDmPotRUE (void );

   void doSWDemand(float SWDemandMaxFactor);
   float DMSupply(void);

   virtual float nDeadVeg(void) {return 0;}
   virtual float pDeadVeg(void)  {return 0;}





   private:
      float fracFloret(void);
      void doDmDemand2(float  dlt_dm_supply);

      float cExtinctionCoeffFloret;
      float cSpec_Floret_area;
      float cRue_Floret;

      float gPai;
      float gDlt_pai;
      float cSvp_fract;
      interpolationFunction cY_frac_Floret;                // fraction of dm allocated to Floret
      int   cNum_stage_no_partition;
      float cFloret_trans_frac;                            // fraction of Floret used in translocat

      struct Cover
      {
         float green;
         float sen;
      };

      Cover coverFloret;
      void calcDlt_Floret_area (void);

};

#endif /* FloretPartH */
