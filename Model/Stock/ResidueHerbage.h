//---------------------------------------------------------------------------
#ifndef ResidueHerbageH
#define ResidueHerbageH
#include "HerbageBase.h"
#include "ResiduePool.h"


// ------------------------------------------------------------------
class ResidueHerbage : public HerbageBase
   {
   public:
      ResidueHerbage(void);
      ResidueHerbage(protocol::Component *system);
      ~ResidueHerbage(void);
      void doInit1(const protocol::Init1Data&);
      void doInit2(void);
//      void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      void doDmdPoolsToHerbageParts(protocol::RemoveHerbageType &grazed, protocol::RemoveCropDmType &crop);
      void doDigestibility (void);

      void doRunTimeReg(void);
      void getParts(ResiduePartType &parts, unsigned partsID);
////      void getPstanding(ResiduePartType &pstanding, ResiduePool &dm);
////      void getPlying(ResiduePartType &plying, ResiduePool &dm);
////      void getPDead(ResiduePartType &pDead, ResiduePool &dm);
////      void getHeight(float &height);
////      void getThermalTime(float &thermalTime);
      void getVariables(void);
      void getStage(void);
      void readHerbageModuleParameters ( void );
      void calcDmdDistribution(ResiduePool dmdFraction[], ResiduePool dQ);
      void calcDmdDistributionB(ResiduePool dmdFraction[], ResiduePool dQ);
      void calcDmdClass(ResiduePool &dmdClassMax, ResiduePool &dmdClassMin);
      void calcDmdAverage(void);

      float dmTotalVeg(void);
      float dmTotVeg(int pool);
      float cpConcVeg(int pool);
      float pConcVeg(int pool);
      float ashAlkVeg(int pool);
      float sConcVeg(int pool);

      float dmTotalSeed(void);
      float dmTotSeed(int pool);
      float cpConcSeed(int pool);
      float pConcSeed(int pool);
      float ashAlkSeed(int pool);
      float sConcSeed(int pool);

      float proportionGreen(void);
      float proportionLegume(void);
      float selectionFactor ( void );


      void doGrazed(protocol::RemoveHerbageType &grazed);
      void readParameters ( void );

      void proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[]);
      void dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin);

      float hHeight(void);
      float heightRatioVeg(void);
      float bD(void);
      float dmdValueVeg(int pool);
      float protDgVeg(int pool);

      float heightRatioSeed(void);
      float dmdValueSeed(int pool);
      float protDgSeed(int pool);

      int seedClass(int pool);
      int seedMaturity(void);
      float trampling(void);


      int numDmdPoolsVeg ( void );
      int numDmdPoolsSeed ( void );
      string herbageModuleName(void);
      string debug();

   protected:
////      protocol::Component *system;

      unsigned removeHerbageID;

      unsigned SOMID;
      unsigned dmFeedOnOfferID;
      unsigned dmFeedRemovedID;
      unsigned removeCropBiomassID;

      ResiduePool dmdFractionVeg[maxDmdPoolsVeg];
      ResiduePool dmdPoolDmVeg[maxDmdPoolsVeg];
      ResiduePool partFractionVeg[maxDmdPoolsVeg];
      ResiduePool dmdMaxVeg;
      ResiduePool dmdAvgVeg;
      ResiduePool dmdMinVeg;

      ResiduePool dmdClassMaxVeg;
      ResiduePool dmdClassMinVeg;

      ResiduePool dmVeg;
      ResiduePool NVeg;
      ResiduePool PVeg;
      ResiduePool dQVeg;

      ResiduePool dmdFractionSeed[maxDmdPoolsSeed];
      ResiduePool dmdPoolDmSeed[maxDmdPoolsSeed];
      ResiduePool partFractionSeed[maxDmdPoolsSeed];
      ResiduePool dmdMaxSeed;
      ResiduePool dmdAvgSeed;
      ResiduePool dmdMinSeed;

      ResiduePool dmdClassMaxSeed;
      ResiduePool dmdClassMinSeed;

      ResiduePool dmSeed;
      ResiduePool NSeed;
      ResiduePool PSeed;
      ResiduePool dQSeed;

      float  height;
////      float  thermalTime;

         string cHerbageModuleName;
         string cDebug;

         float cDmdValueVeg[maxDmdPoolsVeg];
         int   cNumDmdPoolsVeg;

         float cDmdValueSeed[maxDmdPoolsSeed];
         int   cNumDmdPoolsSeed;

   private:
////      protocol::Component *system;
      float divide (float dividend, float divisor, float default_value);

      unsigned surfaceOMID;

      struct
      {

         float dmdValueVeg[maxDmdPoolsVeg];
         int   numDmdPoolsVeg;

         float dmdValueSeed[maxDmdPoolsSeed];
         int   numDmdPoolsSeed;

         float pConccelluloseDefault;
         float pConccarbohydrateDefault;
         float pConcligninDefault;

         float AshAlkcelluloseDefault;
         float AshAlkligninDefault;
         float AshAlkcarbohydrateDefault;

         float NSRatiocelluloseDefault;
         float NSRatiocarbohydrateDefault;
         float NSRatioligninDefault;

         float NPRatiocelluloseDefault;
         float NPRatiocarbohydrateDefault;
         float NPRatioligninDefault;

         float dmdstanding[3];
         float dmdlying[3];

         float cpNRatio;
         float proportionLegume;

         float dmdWeightingCarbohydrate;
         float dmdWeightingCellulose;
         float dmdWeightingLignin;

      } c;

   };

//class ResiduePoolTypeC
//{
//}

#endif
