//---------------------------------------------------------------------------
#ifndef ApsimSimulationFileH
#define ApsimSimulationFileH
#include <General/platform.h>

class XMLDocument;
// ------------------------------------------------------------------
// This class encapsulates an APSIM simulation file (.SIM)
// ------------------------------------------------------------------
class EXPORT ApsimSimulationFile
   {
   public:
      ApsimSimulationFile(void);
      ApsimSimulationFile(const std::string& filename);
      ApsimSimulationFile(const std::string& xml, bool dummy);
      ~ApsimSimulationFile(void);

      void write(void) const;

      std::string getFileName(void) const {return fileName;}
      std::string getName(void) const;
      std::string getExecutableFileName(void) const;
      std::string getTitle(void) const;
      bool doPrintReport(void) const;
      void setFileName(const std::string& file) {fileName = file;}
      void setName(const std::string& name);
      void setExecutableFileName(const std::string& executableFileName);
      void setTitle(const std::string& title);

      // system methods.
      void getSystemNames(std::vector<std::string>& systemNames) const;
      ApsimSystemData getSystem(const std::string& name) const;
      ApsimSystemData addSystem(const std::string& name);

      // component methods.
      void getComponentNames(std::vector<std::string>& componentNames) const;
      ApsimComponentData getComponent(const std::string& name) const;
      ApsimComponentData addComponent(const std::string& name);
      ApsimComponentData addComponent(ApsimComponentData& component);
      bool deleteComponent(const std::string& name);

      // service methods.
      void getServiceNames(std::vector<std::string>& serviceNames) const;
      ApsimServiceData getService(const std::string& name) const;
      ApsimServiceData addService(const std::string& name);

      // return this simulation as a system or a component
      ApsimSystemData asSystem(void);
      ApsimComponentData asComponent(void);

      // return true if simulation is dirty.
      bool isDirty(void) const {return xmlDoc->isDirty();}
   protected:
      XMLDocument* xmlDoc;
      std::string fileName;
   };
#endif
