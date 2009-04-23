//---------------------------------------------------------------------------
#ifndef YPComponentH
#define YPComponentH
#include <ComponentInterface\Component.h>
#include <string>
#include <vector>
// ------------------------------------------------------------------
// Yield Prophet component.
// ------------------------------------------------------------------
class YPComponent : public protocol::Component
   {
   public:
      YPComponent(void);
      virtual void doInit1(const protocol::Init1Data&);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);

   private:
      unsigned cllID;
      unsigned dulID;
      unsigned swID;
      unsigned critSwID;
      unsigned nID;
      unsigned rootDepthID;
      unsigned lldepID;
      unsigned duldepID;
      unsigned swdepID;
      unsigned dlayerID;
      unsigned no3ID;
      unsigned nh4ID;

      std::vector<double> cll, dul, Depth, no3, nh4;

      double interpFromArray(std::vector<double>& values, const std::string& variableName);
      void getStaticVariables();
      std::vector<double> Accum(std::vector<double>& values);
      void ::getDoubles(unsigned id, std::vector<double>& values, bool optional);

   };


#endif
