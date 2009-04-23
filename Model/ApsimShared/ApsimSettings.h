//---------------------------------------------------------------------------
#ifndef ApsimSettingsH
#define ApsimSettingsH
#include <General/platform.h>
#include <General/xml.h>

class IniFile;
// ------------------------------------------------------------------
// This class looks after all storing of apsim settings.
// Settings are stored under keys - much like the Windows registry.
// Keys can be hierarchical and are in the format:
//           section1|section2|...|name
// ------------------------------------------------------------------
class EXPORT ApsimSettings
    {
    public:
      ApsimSettings(void);
      ~ApsimSettings(void);

      void refresh(void);

      // Read in a setting for the specified key.  Key should be in the format:
      //    section1|section2|...|keyvalue.
      // Will throw if key value cannot be converted to int or double.
      void read(const std::string& key, std::string& value, bool replaceMacros = false) const;
      void read(const std::string& key, int& value) const;
      void read(const std::string& key, bool& value) const;
      void read(const std::string& key, double& value) const;
      void read(const std::string& key, std::vector<std::string>& values, bool replaceMacros = false) const;

      // Write a setting for the specified key.
      void write(const std::string& key, const std::string& value);
      void write(const std::string& key, const char* value) {write(key, std::string(value));}
      void write(const std::string& key, int value);
      void write(const std::string& key, bool value);
      void write(const std::string& key, double value);
      void write(const std::string& key, const std::vector<std::string>& values);

      // Return a complete list of all keys under the specified key.
      void getKeysUnder(const std::string& key, std::vector<std::string>& keys);
      //void getSectionNames(std::vector<std::string>& sections) const;

      // Erase the specified section or key.
      //void deleteSection(const std::string& section);
      void deleteKey(const std::string& key);

      // Add in a %apsim% macro to the specified string if possible
      static void addMacro(std::string& st);

      // Return a list of conversion nodes to get the specified version number
      // up to the most recent APSIM version.
      void getConversionNodes(const std::string& version,
                              std::vector<XMLNode>& ConversionNodes,
                              std::string& toVersionString);
      // return a component ordering.
      void getComponentOrder(std::vector<std::string>& order);

   private:
      XMLDocument* original;

      std::string getSection(const std::string& key) const;
      std::string getKey(const std::string& key) const;
   };

#endif
