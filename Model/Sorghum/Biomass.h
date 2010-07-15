//---------------------------------------------------------------------------

#ifndef BiomassH
#define BiomassH

#include "PlantComponents.h"
#include "Utilities.h"

//------------------------------------------------------------------------------------------------

class Biomass : public PlantProcess
   {
   private:
   float effectiveRue;


   public:
// Parameters ----------------------------------------------------------
   vector<float> ratioRootShoot;
   float stem2FlowerFrac;

//  Variables  ---------------------------------------------------------
   float aboveGroundBiomass;
   float aboveGroundGreenBiomass;
   float totalBiomass;
   float greenBiomass;
   float hi;
   float dltDMPotTE;
   float dltDMPotRUE;
   float dltDM;
   float stage;

   vector<float> greenDM;
   vector<float> senescedDM;
   vector<float> dltDMGreen;
   vector<float> dltDMDetachedSen;
   vector<float> dltDMRetranslocate;

// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);
   float calcDltDMPotTE(void);
   void  calcBiomassPartitioning(void);
   void  calcBiomassRetranslocation(void);

   // plant
   void  calcBiomassTE(void);
   void  calcDltBiomass(void);
   void  calcPartitioning(void);
   void  calcRetranslocation(void);
   void  dmScenescence(void);

// public Methods -------------------------------------------------------
   public:
   // plant
   Biomass(ScienceAPI2 &, Plant *p);
   ~Biomass();

   void  readParams (void);
   void  updateVars(void);
   void  process(void);
   void  calcBiomassRUE(float rue, float radnIntercepted);
   float getDltDMPotRUE(void)const{return dltDMPotRUE;}
   float getDltDMPotTE(void)const{return dltDMPotTE;}
   float getEffectiveRue(void)const{return effectiveRue;}

   // grain
   float getTotalBiomass(void)const{return totalBiomass;}
   float getAboveGroundBiomass(void)const{return aboveGroundBiomass;}
   float getDltDM(void)const{return dltDM;}

   void  detachment(vector<float> senDetachFrac);
   void  incorporateResidue(void);

   void  getDMGreen(float &);
   void  getDMSenesced(float &);
   void  getDltDMGreen(float &);
   void  getDltDMDetached(vector<float> &);
   void  getDltDMGreenRetrans(vector<float> &);
   void  getBiomass(float &);

   void  Update(void) {updateVars();}
   void  Harvest(void) {initialize();}

   void  Summary(void);
   
   // phenology
   void  phenologyEvent(int){};
};

#endif
