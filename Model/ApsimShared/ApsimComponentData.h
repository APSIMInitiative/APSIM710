//---------------------------------------------------------------------------
#ifndef ApsimComponentDataH
#define ApsimComponentDataH

#include <general/platform.h>
#include <general/xml.h>
#include <map>
class ApsimSystemData;
class ApsimRegistrationData;
class ApsimDataTypeData;
class ApsimDataTypesFile;

// ------------------------------------------------------------------
// This class encapsulates the data in a component section of
// an APSIM simulation file(.SIM).
// ------------------------------------------------------------------
class EXPORT ApsimComponentData
   {
   public:
      ApsimComponentData(void);
      ApsimComponentData(const std::string& xml);
      ApsimComponentData(const XMLNode& n);
      ApsimComponentData(const ApsimComponentData& rhs);
      ~ApsimComponentData(void);

      ApsimComponentData& operator=(const ApsimComponentData& rhs);

      std::string getName(void) const;
      std::string getExecutableFileName(void) const;
      std::string getComponentInterfaceFileName(void) const;

      std::string getXML(void) const;
      void setName(const std::string& name);
      void setExecutableFileName(const std::string& executable);

      // property methods
      std::string getProperty(const std::string& propertyType,
                              const std::string& name) const;
      void getProperties(const std::string& propertyType,
                         std::vector<std::string>& names,
                         std::vector<std::string>& values) const;
      void setProperty(const std::string& propertyType,
                       const std::string& groupName,
                       const std::string& name,
                       const std::string& value);
      bool replaceProperty(const std::string& propertyType,
                           const std::string& name,
                           const std::string& value);
      void clearProperties(const std::string& propertyType);
      std::string findProperty(const std::string& name);

      void getGroupNames(const std::string& propertyType,
                         std::vector<std::string>& groupNames);

      // variable methods.
      void clearVariables(void);
      void getVariables(std::vector<std::string>& variables) const;
      void addVariable(const std::string& name);

      // rule methods.  All rules have a unique name.
      void clearRules(void);
      void getRuleNames(std::vector<std::string>& names) const;
      void getRule(const std::string& name,
                   std::string& condition,
                   std::string& contents) const;
      void addRule(const std::string& name,
                   const std::string& condition,
                   const std::string& contents);

      // registration methods.
      typedef TreeNodeAliasIterator< TreeNodeIterator<XMLNode>, ApsimRegistrationData> RegIterator;
      RegIterator regBegin(void) const;
      RegIterator regEnd(void) const;
      ApsimDataTypeData getDataType(const std::string& name) const;

      // return the name of the interface file for this component
      std::string getInterfaceFileName(void) const;

   private:
      XMLDocument* xmlDoc;
      XMLNode node;
      XMLNode getInitData(void) const;
      mutable ApsimDataTypesFile* dataTypesFile;
      mutable bool haveReadBaseProperties;

      mutable std::vector<std::string> baseNames, baseValues;

      void replaceAllMacros(XMLNode::iterator rules, std::string& contents) const;
      std::string getValuesFromTable(const std::string& name, XMLNode TableNode) const;
      std::string matchProperty(const std::string& name,
                                const std::vector<std::string>& names,
                                const std::vector<std::string>& values) const;

      typedef std::multimap<std::string, std::string> Parameters;
      struct Section
         {
         std::string name;
         std::string getName() {return name;}
         Parameters parameters;
         };
      typedef std::vector<Section> Sections;
      mutable Sections sections;
      Sections::iterator findSection(const std::string& sectionName) const;


      friend class ApsimSystemData;  // so that ApsimSystemData::appendChild can get to node.
   };
#endif
