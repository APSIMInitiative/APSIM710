//---------------------------------------------------------------------------

#ifndef BiomassH
#define BiomassH

#include "PlantComponents.h"
#include "Utilities.h"
namespace Maize {
   //------------------------------------------------------------------------------------------------

   class Biomass : public PlantProcess
      {
      private:
         double effectiveRue;

      
         // Parameters ----------------------------------------------------------
         vector<double> ratioRootShoot;
         double stem2FlowerFrac;

         //  Variables  ---------------------------------------------------------
         double aboveGroundBiomass;
         double aboveGroundGreenBiomass;
         double totalBiomass;
         double greenBiomass;
         double hi;
         double yield;
         double dltDMPotTE;
         double dltDMPotRUE;
         double dltDM;
         double stage;
         double stover;
         double biomGreen;
         double biomStover;

         vector<double> greenDM;
         vector<double> senescedDM;
         vector<double> dltDMGreen;
         vector<double> dltDMDetachedSen;
         vector<double> dltDMRetranslocate;

         // Private Methods -------------------------------------------------------
         void  doRegistrations(void);
         void  initialize(void);
         double calcDltDMPotTE(void);
         void  calcBiomassRetranslocation(void);


         // public Methods -------------------------------------------------------
      public:
         // plant
         Biomass(ScienceAPI2 &, Plant *p);
         ~Biomass();

         void  readParams (void);
         void  updateVars(void);
         void  process(void);
         void  calcBiomassRUE(double rue, double radnIntercepted);

         // plant
         void  calcBiomassTE(void);
         void  calcDltBiomass(void);
         void  calcPartitioning(void);
         void  calcRetranslocation(void);
         void  dmScenescence(void);

			double getDltDMPotTE(void)const{return dltDMPotTE;}
         double getEffectiveRue(void)const{return effectiveRue;}
         double getDltDMPotRUE(void)const{return dltDMPotRUE;} // plant


         // grain
         double getTotalBiomass(void)const{return totalBiomass;}
         double getAboveGroundBiomass(void)const{return aboveGroundBiomass;}
         double getDltDM(void)const{return dltDM;}

         void  detachment(vector<double> senDetachFrac);
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
   }
#endif
