//---------------------------------------------------------------------------
#ifndef ApsimServiceDataH
#define ApsimServiceDataH
#include <General/platform.h>

// ------------------------------------------------------------------
// This class encapsulates the data in a service section of
// an APSIM simulation file(.SIM).
// ------------------------------------------------------------------
class EXPORT ApsimServiceData
   {
   public:
      ApsimServiceData(const XMLNode& n);
      ApsimServiceData(const std::string& xml);
      ~ApsimServiceData(void);

      std::string getName(void) const;
      std::string getExecutableFileName(void) const;
      void setName(const std::string& name);
      void setExecutableFileName(const std::string& executable);

      // property methods
      std::string getProperty(const std::string& name) const;
      void setProperty(const std::string& name,
                       const std::string& value);

      std::string getXML(void) const;

   private:
      std::string name;
      std::string executable;
      XMLNode node;
      XMLDocument* xmlDoc;

      XMLNode getInitData(void) const;

   };
#endif
