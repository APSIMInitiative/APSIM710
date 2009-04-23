#ifndef MacroH
#define MacroH

#include <string>
#include <vector>
#include <General/platform.h>
class XMLNode;
// ------------------------------------------------------------------
// This class takes a macro file and an XMLDocument and performs a
// macro replacement.
// ------------------------------------------------------------------
class EXPORT Macro
   {
   public:
      // ------------------------------------------------------------------
      // Constructor & Destructor
      // ------------------------------------------------------------------
      Macro();
      ~Macro();

      // ------------------------------------------------------------------
      // generate the files.
      // ------------------------------------------------------------------
      void go(const XMLNode& macroValues,
              const std::string& macroContents,
              std::ostream& out);

      // ------------------------------------------------------------------
      // generate the files.
      // ------------------------------------------------------------------
      void go(const XMLNode& macroValues,
              const std::string& macroContents,
              std::vector<std::string>& filesGenerated,
              const std::string& outputDirectory);
              
      void go(const XMLNode& macroValues,
              const std::string& macroContents,
              std::vector<std::string>& filesGenerated); 


   private:
      const XMLNode* macroValues;

      // ------------------------------------------------------------------
      // Parse and remove all for_each macros from specified string.
      // ------------------------------------------------------------------
      std::string parseForEach(const std::string& contents,
                               const std::string& parentName,
                               const XMLNode& valuesNode) const;

      // ------------------------------------------------------------------
      // Do all macro replacement in specified text for the given macroname.
      // ------------------------------------------------------------------
      void doMacroReplacement(const std::string& macroName, std::string& contents);

      // ------------------------------------------------------------------
      // Write everything between #file/#endfile pairs to the
      // file name listed after the #file macro.
      // ------------------------------------------------------------------
      void writeStringToFiles(std::string contents,
                              std::vector<std::string>& fileNamesCreated,
                              const std::string& outputDirectory) const;

      // ------------------------------------------------------------------
      // Replace all global counter in the specified string.
      // ------------------------------------------------------------------
      void replaceGlobalCounter(std::string& contents) const;

      // ------------------------------------------------------------------
      // Resolve any #if defines.
      // ------------------------------------------------------------------
      void parseIf(std::string& forEachBody) const;

      // ------------------------------------------------------------------
      // Evaluated the specified #if statement.
      // ------------------------------------------------------------------
      bool evaluateIf(const std::string& conditionLine) const;

   };

#endif
