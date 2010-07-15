//---------------------------------------------------------------------------

#ifndef PlantComponentsH
#define PlantComponentsH



#include <vector>
#include <string>
#include "Utilities.h"

class TableFn;
class ScienceAPI2;
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
   float stage;
   int   partNo;
   std::string name;


   // Biomass
   float dmGreen;
   float dltDmGreen;

   float dmSenesced;
   float dltDmSenesced;
   float dltDetDmSenesced;

   float dmPlantMin;
   float dmRetranslocate;
   

   // Nitrogen
   float nGreen;
   float dltNGreen;

   float nDemand;
   float dltNRetranslocate;

   float nSenesced;
   float dltNSenesced;
   float dltDetNSenesced;

   float nConc;

   // Phosphorus
   float pGreen;
   float pSenesced;
   float dltPGreen;
   float dltPSenesced;
   float dltPDetached;
   float pDemand;
   float dltPRetranslocate;
   float pConc;

   // phosphorus  parameters
   TableFn pMaxTable;
   TableFn pMinTable;
   TableFn pSenTable;
   float initialPConc;


   public:
   PlantPart(ScienceAPI2 &api) ;
   void initialize(void);
   virtual void  phenologyEvent(int) = 0;
   virtual float calcNDemand(void) = 0;
   virtual float calcPDemand(void) = 0;

   //Detatchmenet Routines
   virtual void dmDetachment(std::vector<float>);
   virtual void NDetachment(std::vector<float>);

   //Getters
   virtual float getNGreen(void){return nGreen;};
   virtual float getNSenesced(void){return nSenesced;};

   virtual float getDmGreen(void){return dmGreen;};
   virtual float getDmSenesced(void){return dmSenesced;};

   virtual float getDltNGreen(void){return dltNGreen;};
   virtual float getDltDetNSenesced(void){return dltDetNSenesced;};

   virtual float getDltDmGreen(void){return dltDmGreen;};
   virtual float getDltDetDmSenesced(void){return dltDetDmSenesced;};

   virtual float getNDemand(void){return nDemand;};
   virtual float getDltDmRetranslocate(void){return dmRetranslocate;};
   virtual float getDltNRetranslocate(void){return dltNRetranslocate;};

   virtual void  resetDailyVars(void);

   virtual float pConcMax(void){return pMaxTable.value(stage);}
   virtual float pConcMin(void){return pMinTable.value(stage);}
   virtual float pConcSen(void){return pSenTable.value(stage);}
   virtual float getPGreen(void){return pGreen;};
   virtual float getPSenesced(void){return pSenesced;};
   virtual float getPDemand(void){return pDemand;};
   virtual float getDltPGreen(void){return dltPGreen;};
   virtual float getDltPRetrans(void){return dltPRetranslocate;};


   virtual void setPRetrans(float P){dltPRetranslocate = P;}


   void partitionN(float N){dltNGreen += N;}
   void partitionP(float P){dltPGreen += P;}
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
   virtual void  phenologyEvent(int) = 0;

   // variables
   };
//---------------------------------------------------------------------------
#endif
