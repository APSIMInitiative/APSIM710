//---------------------------------------------------------------------------
#ifndef PastureConverterH
#define PastureConverterH


#include <General/pch.h>
#include <math.h>
#include <strstream>
#include <string>
#include <vector>

#include <General/string_functions.h>
#include <General/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>

#include "PastureUptake.h"

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
// Maximum number of layers in soil
#define max_layer 100

// ------------------------------------------------------------------
// ------------------------------------------------------------------
class PastureConverter : public protocol::Component
   {
   public:
      PastureConverter(void);
      ~PastureConverter(void);
      virtual void doInit1(const protocol::Init1Data&);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

   private:
      void readParameters ( void );
      void sendSand (protocol::QueryValueData& queryData);
      void sendVPD (protocol::QueryValueData& queryData);
      void sendCO2 (protocol::QueryValueData& queryData);
      float svp(float temp); //(INPUT)  fraction of distance between svp at mi
      float vpd(float svp_fract, float maxt, float mint); //(INPUT)
      void doPrepare(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doProcess(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doPost(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doCropUptake(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doAddFOM(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void dosowPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void docutPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void docultivatePasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void dokillPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doburnPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void dospraytopPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      double getVariableValue (protocol::Variant& variant, string eventName, string variableName, string unitName);

      unsigned prepareID;
      unsigned processID;
      unsigned postID;
      unsigned sandID;
      unsigned vpdID;
      unsigned co2ppmID;
      unsigned maxtID;
      unsigned mintID;
      unsigned fomAddedID;
      unsigned incorpFOMID;

      unsigned spraytopID;
      unsigned spraytopPastureID;
      unsigned burnID;
      unsigned burnPastureID;
      unsigned killID;
      unsigned killPastureID;
      unsigned cutID;
      unsigned cutPastureID;
      unsigned cultivateID;
      unsigned cultivatePastureID;
      unsigned sowID;
      unsigned sowPastureID;

      string cDebug;
      vector <float> pSandLayer;

      float cSVPFract;
      float cCO2ppm;

   PastureUptake *SW;
   PastureUptake *NO3;
   PastureUptake *NH4;
   };

#endif
