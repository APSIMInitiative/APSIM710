//---------------------------------------------------------------------------

#ifndef SimCreatorH
#define SimCreatorH
#include "ApsimControlFile.h"

#include "SimCreatorSection.h"
#include <vector>
#include <string>
#include <General/platform.h>
//---------------------------------------------------------------------------
// This class creates SIM files given a control file.
//---------------------------------------------------------------------------
class EXPORT SimCreator
   {
   public:
      SimCreator(bool NewFormat) : newFormat(NewFormat) { }
      ~SimCreator();

      void ConToSim(const std::string& controlFileName);

      void ConToSim(const std::string& controlFileName,
                    std::vector<std::string>& sectionNames);

      void ConToSim(const std::string& controlFileName,
                    const std::string& sectionName);

      std::string convertIniToSim(const std::string& includeFileName);

   private:
      bool newFormat;

      // ------------------------------------------------------
      // Encapsulates a converted APSIM parameter file
      // ------------------------------------------------------
      class ParFile
         {
         public:
            ParFile(const std::string& file) : fileName(file) { }
            ~ParFile()
               {
               for (unsigned i = 0; i != sections.size(); i++)
                  delete sections[i];
               sections.erase(sections.begin(), sections.end());
               }
            ParFile(const ParFile& rhs)
               {
               fileName = rhs.fileName;
               for (unsigned i = 0; i != rhs.sections.size(); i++)
                  sections.push_back(rhs.sections[i]->clone());
               }
            std::string fileName;
            std::string getFilename() {return fileName;}
            std::vector<SimCreatorSection*> sections;
         };
      std::vector<ParFile*> convertedParFiles;

      void ConToSimInternal(const std::string& controlFileName,
                            const std::vector<std::string>& sectionNames);

      // -------------------------------------------
      // Create a bit of .sim file (.xml format) for
      // the specified module instance and output it
      // to the specified output stream.
      // -------------------------------------------
      void ConvertConModule(ApsimControlFile::ModuleInstance& moduleInstance,
                            std::ostream& out);


      // --------------------------------------------
      // for the specified instance name, return the
      // matching sections in the specified file.
      // --------------------------------------------
      void GetMatchingParFileSections(const std::string& instanceName,
                                      const std::string& fileName,
                                      const std::string& sectionName,
                                      std::vector<SimCreatorSection*>& outputParFiles);

      // --------------------------------------------
      // Convert a whole par file to XML storing an
      // entry in 'convertedparfiles' vector.
      // --------------------------------------------
      ParFile* ConvertParFile(const std::string& fileName);

      // ---------------------------------------------
      // Return true if the 2 section names match
      // ---------------------------------------------
      bool sectionIsAMatch(const std::string& instanceName,
                           const std::string& conSectionName,
                           const std::string& parSectionName);

      // ---------------------------------------------
      // write the specified sections to the specified
      // output stream in OLD format
      // ---------------------------------------------
      void writeOldFormat(std::vector<SimCreatorSection*>& sectionsToOutput,
                          std::ostream& out);

      // ---------------------------------------------
      // write the specified sections to the specified
      // output stream in NEW format
      // ---------------------------------------------
      void writeNewFormat(std::vector<SimCreatorSection*>& sectionsToOutput,
                          std::ostream& out);


   };
#endif
