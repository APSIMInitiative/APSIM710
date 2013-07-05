#ifndef RootBaseH
#define RootBaseH
#include "../SimplePart.h"

class RootBase : public SimplePart
   {
   public:
      RootBase(ScienceAPI& scienceAPI, plantInterface *p, const string &name);
      ~RootBase(){};

      virtual void zeroAllGlobals(void){};
      virtual void zeroDeltas(void){};
      virtual void onInit1(protocol::Component *system){};
      virtual void read(){};
      virtual void onSowing(void){};
      virtual void onGermination(void){};
      virtual void onEmergence(void){};
      virtual void onFlowering(void){};
      virtual void onStartGrainFill(void){};
      virtual void onHarvest(float height, float remove_fr,
                     vector<string> &dm_type,
                     vector<float> &dlt_crop_dm,
                     vector<float> &dlt_dm_n,
                     vector<float> &dlt_dm_p,
                     vector<float> &fraction_to_residue){};
      virtual void onEndCrop(vector<string> &dm_type,
                     vector<float> &dlt_crop_dm,
                     vector<float> &dlt_dm_n,
                     vector<float> &dlt_dm_p,
                     vector<float> &fraction_to_residue){};
      virtual void onKillStem(void){};
      virtual void update(){};
      virtual void updateOthers(){};
      virtual void checkBounds(void){};
      virtual void removeBiomass2(float chop_fr){};
      virtual void sen_length(void){};
      virtual void root_length_growth (void){};
      virtual void plant_root_depth (void){};

      virtual void collectDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_residue
                                          , vector<float> &dm_n
                                          , vector<float> &dm_p
                                          , vector<float> &fract){};
      virtual void redistribute(const vector<float> &, const vector<float> &, float){};
//      virtual int find_layer_no(float) {};
//      virtual int find_layer_no(float,const vector<float>&){};
//      virtual int find_layer_no(float, float *, int){};
      virtual float sw_avail_ratio(int layer) {return 0;};

      virtual void doPlantWaterStress (float sw_demand, SWStress *swStress) {};


      virtual void doWaterUptakeInternal(float sw_demand){};
      virtual float waterUptake(void){return 0;};
      virtual void getOtherVariables(){};
      virtual void UpdateOtherVariables(){};

      virtual float wet_root_fr (void){return 0;};
      virtual void onNewProfile(protocol::NewProfileType &v){};
      virtual void write(){};

      virtual void plant_nit_supply(){};
      virtual void doNUptake(float sumNMax, float sumSoilNDemand, float NDemand, float n_fix_pot){};
      virtual void doWaterUptake (int option, float SWDemand){};
      virtual float peswTotal(){return 0;};
      virtual float pesw(int depth){return 0;};
      virtual float swSupply(){return 0.0;};
      virtual float NSupply(){return 0.0;};
      virtual float swAvailable(){return 0.0;};
      virtual float swAvailablePotential(){return 0.0;};
      virtual float nUptake(){return 0.0;};
      virtual float fasw(int depth){return 0;};
      virtual void  get_AvailableToAnimal(protocol::AvailableToAnimalType &avail) {};
   };


#endif /* RootBaseH */
