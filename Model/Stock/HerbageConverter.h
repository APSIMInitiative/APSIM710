//---------------------------------------------------------------------------
#ifndef HerbageConverter_H
#define HerbageConverter_H
#include <ComponentInterface\Component.h>
#include <string>
#include <vector>
#include "PlantHerbage.h"
#include "ResidueHerbage.h"
#include "ConverterBase.h"

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
// Maximum number of layers in soil
#define max_layer 100

// ------------------------------------------------------------------
class HerbageConverter : public ConverterBase
   {
   public:
      HerbageConverter(void);
      HerbageConverter(protocol::Component *system);
      virtual ~HerbageConverter(void);
      virtual void doInit1(const protocol::Init1Data&);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

   private:
      void doRunTimeReg(void);
      void sendFeedOnOffer(protocol::QueryValueData& queryData);
      void sendFeedRemoved(protocol::QueryValueData& queryData);
      void sendDmdAvgFeedRemoved(protocol::QueryValueData& queryData);
      void sendDmdFeedRemoved(protocol::QueryValueData& queryData);
      void sendPlant2Stock(protocol::QueryValueData& queryData);
      void sendTrampling(protocol::QueryValueData& queryData);
      void readParameters ( void );
      float divide (float dividend, float divisor, float default_value);

      unsigned tramplingID;
      unsigned plant2stockID;
      unsigned removeHerbageID;

      unsigned dmFeedOnOfferID;
      unsigned dmFeedRemovedID;
      unsigned DmdAvgFeedRemovedID;
      unsigned DmdFeedRemovedID;
      unsigned removeCropBiomassID;
      bool plant2StockSent;

////      protocol::Component *system;
      protocol::Plant2StockType feed;
      protocol::RemoveHerbageType grazed;

      HerbageBase *conversion;

//      PlantPool dmdPoolDm[maxDmdPools];
//      PlantPool partFraction[maxDmdPools];
//      PlantPool dmdMax;
//      PlantPool dmdAvg;
//      PlantPool dmdMin;
//
//      PlantPool dmdClassMax;
//      PlantPool dmdClassMin;
//
//      SeedPool dmdPoolDm[maxDmdPools];
//      SeedPool partFraction[maxDmdPools];
//      SeedPool dmdMax;
//      SeedPool dmdAvg;
//      SeedPool dmdMin;
//
//      SeedPool dmdClassMax;
//      SeedPool dmdClassMin;

      string herbage_model;
      struct
      {
         string herbageModuleName;
         string debug;

         float dmdValueVeg[maxDmdPoolsVeg];
         int   numDmdPoolsVeg;

         float dmdValueSeed[maxDmdPoolsSeed];
         int   numDmdPoolsSeed;


         float cpNRatio;
      } c;

   };

//class PlantPoolTypeC
//{
//}

#endif
