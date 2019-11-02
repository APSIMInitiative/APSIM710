//---------------------------------------------------------------------------

#ifndef PlantComponentsH
#define PlantComponentsH



#include <vector>
#include <string>
#include "Utilities.h"

class TableFn;
class ScienceAPI2;

namespace Maize {
   class Plant;

   //---------------------------------------------------------------------------

   class PlantComponent
      {
      private:
      protected:
      public:
         ScienceAPI2  &scienceAPI;
         PlantComponent(ScienceAPI2 &api) : scienceAPI(api) {};

         Plant *plant;

         virtual void initialize(void) = 0;
         virtual void readParams (void) = 0;
         virtual void updateVars(void) = 0;

      };
   //---------------------------------------------------------------------------
   class PlantPart : public PlantComponent
      {
      private:

      protected:

         // variables
         double stage;
         int   partNo;
         std::string name;


         // Biomass
         double dmGreen;
         double dltDmGreen;

         double dmSenesced;
         double dltDmSenesced;
         double dltDetDmSenesced;

         double dmPlantMin;
         double dmRetranslocate;


         // Nitrogen
         double nGreen;
         double dltNGreen;

         double nDemand;
         double dltNRetranslocate;

         double nSenesced;
         double dltNSenesced;
         double dltDetNSenesced;

         double nConc;

         // Phosphorus
         double pGreen;
         double pSenesced;
         double dltPGreen;
         double dltPSenesced;
         double dltPDetached;
         double pDemand;
         double dltPRetranslocate;
         double pConc;

         // phosphorus  parameters
         TableFn pMaxTable;
         TableFn pMinTable;
         TableFn pSenTable;
         double initialPConc;


      public:
         PlantPart(ScienceAPI2 &api) ;
         virtual ~PlantPart() {};
         void initialize(void);
         virtual void  phenologyEvent(int) = 0;
         virtual double calcNDemand(void) = 0;
         virtual double calcPDemand(void) = 0;

         //Detatchmenet Routines
         virtual void dmDetachment(std::vector<double>);
         virtual void NDetachment(std::vector<double>);

         //Getters
         virtual double getNGreen(void){return nGreen;};
         virtual double getNSenesced(void){return nSenesced;};

         virtual double getDmGreen(void){return dmGreen;};
         virtual double getDmSenesced(void){return dmSenesced;};

         virtual double getDltNGreen(void){return dltNGreen;};
         virtual double getDltDetNSenesced(void){return dltDetNSenesced;};

         virtual double getDltDmGreen(void){return dltDmGreen;};
         virtual double getDltDetDmSenesced(void){return dltDetDmSenesced;};

         virtual double getNDemand(void){return nDemand;};
         virtual double getDltDmRetranslocate(void){return dmRetranslocate;};
         virtual double getDltNRetranslocate(void){return dltNRetranslocate;};

         virtual void  resetDailyVars(void);

         virtual double pConcMax(void){return pMaxTable.value(stage);}
         virtual double pConcMin(void){return pMinTable.value(stage);}
         virtual double pConcSen(void){return pSenTable.value(stage);}
         virtual double getPGreen(void){return pGreen;};
         virtual double getPSenesced(void){return pSenesced;};
         virtual double getPDemand(void){return pDemand;};
         virtual double getDltPGreen(void){return dltPGreen;};
         virtual double getDltPRetrans(void){return dltPRetranslocate;};


         virtual void setPRetrans(double P){dltPRetranslocate = P;}


         void partitionN(double N){dltNGreen += N;}
         void partitionP(double P){dltPGreen += P;}
		 void setStage(double newStage) { stage = newStage; }
         void calcDltPSenesced(void);
         void calcDltPDetached(void);
         void updateP(void);

         std::string getName(void){return name;}
      };
   //---------------------------------------------------------------------------
   class PlantProcess : public PlantComponent
      {
      private:

      public:
         PlantProcess(ScienceAPI2 &api) : PlantComponent(api) {};
         virtual ~PlantProcess() {};
         virtual void  phenologyEvent(int) = 0;

         // variables
      };
   //---------------------------------------------------------------------------
   }
#endif
