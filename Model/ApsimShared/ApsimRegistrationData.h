//---------------------------------------------------------------------------
#ifndef ApsimRegistrationDataH
#define ApsimRegistrationDataH
#include <General/xml.h>
#include <General/platform.h>

// ------------------------------------------------------------------
// This class encapsulates a single component registration from an
// interface file.
// ------------------------------------------------------------------
class EXPORT ApsimRegistrationData
   {
   public:
      ApsimRegistrationData(XMLNode &n) : node(n) { }

      bool doAutoRegister(void) const;
      std::string getName(void) const;
      std::string getUnits(void) const;
      std::string getType(void) const;
      std::string getDataTypeName(void) const;
      bool isOfType(const std::string& type) const;
      std::string getInternalName(void) const;

   private:
      XMLNode node;
   };
#endif


