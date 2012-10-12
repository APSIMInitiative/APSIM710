#ifndef IniFileH
#define IniFileH
#include <General/platform.h>

// ------------------------------------------------------------------
// Class handling the reading and writting to/from INI files.
// ------------------------------------------------------------------
class EXPORT IniFile
        {
   public:
      IniFile(void);
      ~IniFile(void);
      IniFile(const std::string& fileName, bool createBackups = false);

      void refresh(void);

      void setFileName(const std::string& fileName);
      std::string getFileName(void) const {return fileName;}

      bool read(const std::string& section,
                const std::string& key,
                std::string& value) const;
      bool read(const std::string& section,
                const std::string& key,
                std::vector<std::string>& values) const;
      void readSectionNames(std::vector<std::string>& sections) const;
      void readSection(const std::string& section, std::string& contents) const;
      void writeSection(const std::string& section, const std::string& contents);

      void write(const std::string& section,
                 const std::string& key,
                 const std::string& value);
      void write(const std::string& section,
                 const std::string& key,
                 const std::vector<std::string>& values);

      bool renameKey(const std::string& section,
                     const std::string& oldKey,
                     const std::string& newKey);
      void deleteKey(const std::string& section, const std::string& key);
      void deleteSection(const std::string& Section);
      void getKeysInSection(const std::string& section,
                            std::vector<std::string>& keys) const;
      void renameSection(const std::string& oldSection,
                         const std::string& newSection);

   private:
      bool createBackups;
      bool haveDoneBackup;
      std::string fileName;
      std::string contents;
      std::vector<std::string> sectionNames;
      std::vector<unsigned> sectionIndexes;

      void parse(void);
      bool findMatchingKeys(const std::string& sectionName,
                            const std::string& key,
                            std::vector<std::string>& values,
                            bool allowMultiple) const;
      bool getSectionPosition(const std::string& section,
                              size_t& posStartSection,
                              size_t& posEndSection) const;
      void updateIndexesAfter(const std::string& section, unsigned numChars);

      void doBackup();
   };


#endif
