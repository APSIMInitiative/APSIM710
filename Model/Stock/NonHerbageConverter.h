//---------------------------------------------------------------------------
#ifndef NonHerbageConverterH
#define NonHerbageConverterH
#include "ConverterBase.h"

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
// Maximum number of layers in soil
#define max_layer 100

//std::string ftoa(double Float, char *fmtwidth=".2");
//std::string itoa(int value, int width);


//      const int maxDmdPools = 6;

// ------------------------------------------------------------------
// TRACKER component for APSIM.
// eg of parameter file specification:
//    sum(rain)[1jan-31dec]
//    sum(rain)[sow-harvest]
//    sum(rain)[3]
// ------------------------------------------------------------------
class NonHerbageConverter : public ConverterBase
   {
   public:
      NonHerbageConverter(void);
      NonHerbageConverter(protocol::Component *system);
      ~NonHerbageConverter(void);
      virtual void doInit1(const protocol::Init1Data&);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void stockBuy (protocol::Variant &v/*(INPUT) message variant*/);
      virtual void stockSell (protocol::Variant &v/*(INPUT) message variant*/);
      virtual void stockMove (protocol::Variant &v/*(INPUT) message variant*/);

   private:
      void doRunTimeReg(void);
      void daylengthRelay (protocol::QueryValueData& queryData);
      void intakeRelay (protocol::QueryValueData& queryData);
      void sendAddSurfaceOMEvent (const string& omName, const string& omType, protocol::AddExcretafaeces_omType faecesOM);
      void addUrine (protocol::AddExcretaurineType urine);
      void readParameters ( void );

      unsigned day_lengthID;
      unsigned dayLengthID;
      unsigned tramplingID;
      unsigned ureaID;
      unsigned dltUreaID;
      unsigned labilePID;
      unsigned dltLabilePID;
      unsigned plant2stockID;
      unsigned addExcretaID;

      unsigned stockBuyID;
      unsigned stockSellID;
      unsigned stockMoveID;
      unsigned buyID;
      unsigned sellID;
      unsigned moveID;
      unsigned addManureID;

      unsigned intakeSendID;
      unsigned intakeGetID;

////      protocol::Component *system;
      protocol::AddExcretaType excreted;

      struct
      {
         string debug;
         float fractionFaecesAdded;
         float fractionUrineAdded;
      } c;

   };

#endif
