#ifndef PodPartH
#define PodPartH
#include "../SimplePart.h"
class Co2Modifier;
class FruitCohort;

class fruitGrainPart;
class PlantPartArea {
  public:
   PlantPartArea(ScienceAPI& api, plantInterface *p, const string &name) ;
   ~PlantPartArea() {};
   void onInit1(protocol::Component *);
   void update(void);
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   void readSpeciesParameters (protocol::Component *, vector<string> &);

   void zeroAllGlobals(void);
   void zeroDeltas(void);
   float coverTotal(void)  ;
   float coverGreen(void)  ;
   float coverSen(void)  ;
   void doCover (PlantSpatial &spatial);
   void calcDlt_area (float dltDm);

   protected:

      float cExtinctionCoeff;
      float cSpec_area;
      float cRue;

      float partAI;
      float dlt_partAI;

      struct Cover
      {
         float green;
         float sen;
      };

      Cover cover;
   string myName;                        // What we call ourselves
   plantInterface *plant;                 // The plant we are attached to
   ScienceAPI& scienceAPI;
};
class fruitPodPart : public SimplePart {
  public:
   fruitPodPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name) ;
   ~fruitPodPart() {};
   void onInit1(protocol::Component *);
   void prepare(void);
   void update(void);
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   void onFlowering(void);
   void onStartGrainFill(void);
   void doDmMin(void);
   void doDmDemand(float  dlt_dm_supply);
   void doProcessBioDemand(void);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   float dltDmRetranslocateSupply(float DemandDifferential) ;

   void zeroAllGlobals(void);
   void zeroDeltas(void);
   void interceptRadiationGreen(float radiation);
   float calcInterceptRadiationTotal(float radiation);
   float coverTotal(void)  ;
   float coverGreen(void)  ;
   float coverSen(void)  ;
   void doCover (PlantSpatial &spatial);
   void calcDlt_pod_area (void);
   void doDmPotRUE (void );

   void doSWDemand(float SWDemandMaxFactor);
   float DMSupply(void);
   float removePodFraction(float DM);

   protected:

      float cRue_pod;

      externalFunction *fracPod;
      Co2Modifier *co2Modifier;
      lookupFunction TECoeff;
      PlantPartArea pod;
      float radiationInterceptedGreen;
};

#endif /* PodPartH */
