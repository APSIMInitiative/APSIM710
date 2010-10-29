//---------------------------------------------------------------------------
#ifndef SupplementConverterH
#define SupplementConverterH
#include <ComponentInterface/Component.h>
#include <string>
#include <vector>

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
// Maximum number of layers in soil
#define max_layer 100

// ------------------------------------------------------------------
// ------------------------------------------------------------------
class SupplementConverter : public protocol::Component
   {
   public:
      SupplementConverter(void);
      ~SupplementConverter(void);
      virtual void doInit1(const protocol::Init1Data&);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

   private:
      void readParameters ( void );
      void dobuySupplement(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void dofeedSupplement(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void domixSupplement(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doConserveSupplement(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

      unsigned buyID;
      unsigned buySupplementID;
      unsigned feedID;
      unsigned feedSupplementID;
      unsigned mixID;
      unsigned mixSupplementID;
      unsigned onConserveID;

      string cDebug;

   };

#endif
