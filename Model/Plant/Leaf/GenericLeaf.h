#ifndef GenericLeafH
#define GenericLeafH

#define max_node 1000                         // maximum number of plant nodes
#define max_table 30

#include "Leaf.h"
#include "../Population.h"
class GenericLeaf : public Leaf {

  public:
   GenericLeaf(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
      : Leaf(scienceAPI, p, name) {};
   ~GenericLeaf() {};
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   void zeroAllGlobals(void);
   void zeroDeltas(void);
   void checkBounds(void);
   void onInit1(protocol::Component *);
   void readConstants (protocol::Component *, const string &);
   void readSpeciesParameters(protocol::Component *system, vector<string> &search_order);
   void onEmergence(void);
   void onTransplanting(void);
   void onKillStem(void);

  public:
   float getLAI(void)  {return gLAI;};
   float getSLAI(void)  {return gSLAI;};
   float getLeafNo(void);
   float getNodeNo(void);
   float getDltNodeNo(void);
   float senFract (void)
     { return(divide (dltSLAI, gLAI + dltLAI, 0.0)); };             // fraction of canopy senescing
   float dmGreenDemand(void)
     { return(divide (dltLAI_stressed, cSLAMin * smm2sm, 0.0));};   // Maximum DM this part can take today
   float dltLeafAreaPot(void) {return (divide(dltLAI_stressed, plant->population().Density(), 0.0) * sm2smm);};

   void CanopyExpansion (int option, float, float, float);
   void leaf_area_stressed(float);
   void actual(void);

   void leaf_death (float nfact_expansion, float  dlt_tt);
   void leaf_area_sen (float);
   void Detachment (void);
   void update (void) ;

   void removeBiomass(void);

  private:
   void get_tlai(protocol::Component *system, protocol::QueryValueData &qd);
   void get_lai_sum(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_no(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_no_sen(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_area(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_area_tot(protocol::Component *system, protocol::QueryValueData &qd);

   void leaf_no_pot (int option, float, float);
   void leaf_area_potential(void);
   void leaf_area_actual(void);
   void leaf_no_actual (void);
   void initialiseAreas(void);
   void remove_detachment (float dlt_slai_detached, float dlt_lai_removed );

  // State & rate variables
   float gLAI;                                        // area of leaf
   float gSLAI;                                       // area of leaf senesced from plant
   float gNodeNo;                                     // number of fully expanded nodes ()
   float gLeafNo[max_node];                           // number of fully expanded leaves ()
   float gLeafNoSen[max_node];                        // no of senesced leaves ()
   float gLeafArea[max_node];                         // leaf area of each leaf (mm^2)
   float gLeavesPerNode;

   float cInitialTPLA;                                // initial plant leaf area (mm^2)  XX needed by Plant::remove_biomass
   float cLeafNumberAtEmerg;                          // leaf number at emergence ()
   float cFrLeafSenRate;                              // rate of leaf senescence
   float cNFactLeafSenRate;
   float cNodeSenRate;
   float cSLAMin;                                     // minimum specific leaf area for
                                                      // new leaf area (mm^2/g)
   float cMinTPLA;                                    // minimum plant leaf area(mm2/plant)
   float cNodeNoCorrection;                           //
   float cLAISenLight;                                // critical lai above which light
   float cSenLightSlope;                              // slope of linear relationship
                                                      // between lai and
                                                      // light competition factor for
                                                      // determining leaf senesence rate.
   float cSenRateWater;                               // slope in linear eqn
                                                      // relating soil water
                                                      // stress during photosynthesis
                                                      // to leaf senesense rate

   interpolationFunction cNodeAppRate;
   interpolationFunction cLeavesPerNode;
   interpolationFunction cLeafSize;
   interpolationFunction cSLAMax;
   interpolationFunction cLeafNoFrac;
   interpolationFunction cSenescenceFac;              // temperature senescence table (oC)

   float dltLAI;                                      // area of leaf
   float dltSLAI;                                     // area of leaf that senesces from plant
   float dltLAI_pot;                                  // potential change in live plant lai
   float dltLAI_stressed;                             // potential change in lai allowing for stress
   float dltSLAI_detached;                            // senesced lai detached
   float dltSLAI_age;                                 // senesced lai from age
   float dltSLAI_light;                               // senesced lai from light
   float dltSLAI_water;                               // senesced lai from water
   float dltSLAI_frost;                               // senesced lai from frost

   float dltLeafNoSen;                               // fraction of oldest green leaf senesced ()
   float dltLeafNo;                                // actual fraction of oldest leaf expanding ()
   float dltNodeNo;                                // actual fraction of oldest node expanding ()
   float dltLeafNoPot;                            // potential fraction of oldest leaf expanding ()
   float dltNodeNoPot;                            // potential fraction of oldest leaf expanding ()

};

#endif

