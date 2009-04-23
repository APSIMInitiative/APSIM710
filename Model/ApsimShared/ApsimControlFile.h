//---------------------------------------------------------------------------
#ifndef APSIMControlFileH
#define APSIMControlFileH

#include <stdexcept>
#include <vector>
#include <string>
#include <boost/function.hpp>
#include <general/platform.h>
class IniFile;
// ------------------------------------------------------------------
// This class encapsulates an apsim control file.  It provides several
// methods to extract information from the control file and associated
// parameter files.
// ------------------------------------------------------------------
class EXPORT ApsimControlFile
   {
   public:
      ApsimControlFile(const std::string& controlFilename);
      ~ApsimControlFile(void);

      std::string getFileName(void) const;

      // ------------------------------------------------------------------
      // return true if the specified section is a valid one.
      // ------------------------------------------------------------------
      bool isValid(const std::string& section);

      // ------------------------------------------------------------------
      // return a list of all section names in the control file.
      // ------------------------------------------------------------------
      void getAllSectionNames(std::vector<std::string>& sectionNames);

      // ------------------------------------------------------------------
      // return a list of all filenames specified in the control file section.
      // NB: This list doesn't include output file names.
      // ------------------------------------------------------------------
      void getAllFiles(const std::string& section,
                       std::vector<std::string>& fileNames) const throw(std::runtime_error);

      // ------------------------------------------------------------------
      // Return the referenced file for the specified module e.g. met and soi.
      // ------------------------------------------------------------------
      std::string getFileForInstance(const std::string& section,
                                     const std::string& instanceName) const;

      // ------------------------------------------------------------------
      // Return the .ini file name for the specified instance.
      // ------------------------------------------------------------------
      std::string getIniFileForInstance(const std::string& section,
                                        const std::string& instanceName) const;

      // ------------------------------------------------------------------
      // return a list of output/summary filenames
      // ------------------------------------------------------------------
      void getOutputFileNames(const std::string& section,
                              std::vector<std::string>& fileNames) const;
      std::string getSummaryFileName(const std::string& section) const;

      // ------------------------------------------------------------------
      // Return a list of all parameter values for the specified module
      // and parameter name.
      // ------------------------------------------------------------------
      void getParameterValues(const std::string& section,
                              const std::string& instanceName,
                              const std::string& parameterName,
                              std::vector<std::string>& values) const;

      // ------------------------------------------------------------------
      // Return a single parameter value for the specified module
      // and parameter name.
      // ------------------------------------------------------------------
      std::string getParameterValue(const std::string& section,
                                    const std::string& instanceName,
                                    const std::string& parameterName) const throw(std::runtime_error);

      // ------------------------------------------------------------------
      // Set the value of a parameter for a module.
      // If moduleName is blank then parameter will be written to control file
      // ------------------------------------------------------------------
      void setParameterValues(const std::string& sectionName,
                              const std::string& instanceName,
                              const std::string& parameterName,
                              const std::string& parameterSectionName,
                              const std::vector<std::string>& parameterValues) throw(std::runtime_error);
      void setParameterValue(const std::string& sectionName,
                             const std::string& instanceName,
                             const std::string& parameterName,
                             const std::string& parameterValue) throw(std::runtime_error);

      // ------------------------------------------------------------------
      // Rename the specified parameter in all instances of module.
      // Return true if change was made.
      // ------------------------------------------------------------------
      bool renameParameter(const std::string& sectionName,
                           const std::string& moduleName,
                           const std::string& oldParameterName,
                           const std::string& newParameterName);

      // ------------------------------------------------------------------
      // Delete the specified parameter from all instances of module.
      // Return true if change was made.
      // ------------------------------------------------------------------
      bool deleteParameter(const std::string& sectionName,
                           const std::string& moduleName,
                           const std::string& parameterName);

      // ------------------------------------------------------------------
      // Moves the specified parameter from all instances of module to the
      // specified destination instance.
      // Return true if change was made.
      // ------------------------------------------------------------------
      bool moveParameter(const std::string& sectionName,
                         const std::string& moduleName,
                         const std::string& parameterName,
                         const std::string& destModuleName);

      // ------------------------------------------------------------------
      // change the name of a module in the control file.  Return true
      // on success.
      // ------------------------------------------------------------------
      bool changeModuleName(const std::string& section,
                            const std::string& oldModuleName,
                            const std::string& newModuleName);

      // ------------------------------------------------------------------
      // return title to caller.
      // ------------------------------------------------------------------
      std::string getTitle(const std::string& section) const;
      void setTitle(const std::string& section, const std::string& title);

      // ------------------------------------------------------------------
      // Return true if the control file has parameters in it.
      // ------------------------------------------------------------------
      bool hasParametersInCon(const std::string& section);

      // ------------------------------------------------------------------
      // remove all references to this control file from the list of
      // parameter files for all modules.  Return true if parameters were moved.
      // ------------------------------------------------------------------
      bool moveParametersOutOfCon(const std::string& section,
                                  const std::string& parFileForConParams);

      // ------------------------------------------------------------------
      // Get a parameter file from the control file - any module will do.
      // Also return a section name.
      // ------------------------------------------------------------------
      void getDefaultParFileAndSection(const std::string& section,
                                       std::string& defaultFile,
                                       std::string& defaultSection) const;

      // ------------------------------------------------------------------
      // return a version number for this control file.
      // ------------------------------------------------------------------
      static std::string getVersionNumber(const std::string& fileName);

      // ------------------------------------------------------------------
      // Set the version number in the control file to the current
      // apsim version number
      // ------------------------------------------------------------------
      static void setVersionNumber(const std::string& fileName, const std::string& versionNumber);

      // ------------------------------------------------------------------
      // Return a list of instance names for the specified module name.
      // ------------------------------------------------------------------
      void getInstances(const std::string& section,
                        const std::string& moduleName,
                        std::vector<std::string>& instanceNames) const;

      // ------------------------------------------------------------------
      // Return a list of module names and their instance names.
      // ------------------------------------------------------------------
      struct ModuleInstance
         {
         std::string moduleName;
         std::string instanceName;
         std::string dllFileName;
         typedef std::pair<std::string, std::string> ParFileSection;
         std::vector<ParFileSection> ParFiles;
         };
      typedef std::vector<ModuleInstance> ModuleInstances;
      void getAllModuleInstances(const std::string& section,
                                 ModuleInstances& moduleInstances) const;

      // ------------------------------------------------------------------
      // Enumerate all parameter sections for the specified module name.
      // ------------------------------------------------------------------
//      typedef boost::function2<void, IniFile*, const std::string&> ParamCallbackEvent;
//      typedef void (__closure *ParamCallbackEvent)
//         (IniFile* par, const std::string& section);
      void enumerateParameters(const std::string& section,
                               const std::string& moduleName,
                               bool includeConstants,
                               boost::function2<void, IniFile*, const std::string&> callback);
      void enumerateParametersForInstance(const std::string& section,
                                          const std::string& instanceName,
                                          bool constantsOnly,
                                          boost::function2<void, IniFile*, const std::string&> callback);
      // ------------------------------------------------------------------
      // Add a parameter file reference to all instances of the
      // specified module. Return true if the con file was modified.
      // ------------------------------------------------------------------
      bool addParameterFileReference(const std::string& section,
                                     const std::string& moduleName,
                                     const std::string& parameterFileName,
                                     const std::string& parameterSectionName);

      // ------------------------------------------------------------------
      // Rename the specified module
      // ------------------------------------------------------------------
      bool renameModule(const std::string& section,
                        const std::string& oldModuleName,
                        const std::string &newModuleName);

      // ------------------------------------------------------------------
      // Delete the specified module
      // ------------------------------------------------------------------
      bool deleteModule(const std::string& section,
                        const std::string& moduleName);

      // ------------------------------------------------------------------
      // Rename all standard .ini files to .xml
      // ------------------------------------------------------------------
      bool iniToXml(const std::string& section);

      // ------------------------------------------------------------------
      // Perform a Search and Replace on the sections of the specified module.
      // ------------------------------------------------------------------
      bool searchReplace(const std::string& section,
                         const std::string& moduleName,
                         const std::string& stringToFind,
                         const std::string& replacementString);

      // ------------------------------------------------------------------
      // Perform a Search and Replace on a control file section
      // ------------------------------------------------------------------
      bool searchReplaceCon(const std::vector<std::string>& stringToFinds,
                            const std::vector<std::string>& replacementStrings);

      // ------------------------------------------------------------------
      // Enumerate all matching manager action lines and call a callback
      // for each one. The callee can then change the parameters if
      // they so wish. Return's true if parameters were modified.
      // ------------------------------------------------------------------
      struct ManagerActionParameter
         {
         std::string name, value, units;
         };
      typedef std::vector<ManagerActionParameter> ManagerActionParameters;

      //typedef void (__closure *ManagerActionCallback)(ManagerActionParameters& parameters, bool& modified);
      bool enumerateManagerActionLines(const std::string& section,
                                       const std::string& managerAction,
                                       boost::function2<void, ManagerActionParameters& , bool& > callback);

      // -----------------------------------------------------------------
      // Return a list of all sections within par files for a given module
      // instance.
      // -----------------------------------------------------------------
      void getParameterSections(const std::string& section,
                                const std::string& instanceName,
                                std::vector<IniFile*>& ini,
                                std::vector<std::string>& fullSectionNames);


   private:
      IniFile* ini;
      mutable std::vector<IniFile*> openedParFiles;

      // ------------------------------------------------------------------
      // remove all references to this control file from the list of
      // parameter files for all modules.
      // ------------------------------------------------------------------
      void removeSelfReferences(const std::string& section,
                                const std::string& parFileForConParams);

      // ------------------------------------------------------------------
      // return an opened parameter file ready to read.
      // ------------------------------------------------------------------
      IniFile* getParFile(const std::string& fileName, bool checkNonExistant=true) const;

      // ------------------------------------------------------------------
      // Find a parameter in parameter file.  Return true and the par and
      // section name where parameter is located.
      // ------------------------------------------------------------------
      bool findParameterName(const std::string& section,
                             const std::string& instanceName,
                             const std::string& parameterName,
                             IniFile*& par,
                             std::string& parameterFileName,
                             std::string& parameterSection) const;
      // ------------------------------------------------------------------
      // write new module= line to control file.
      // ------------------------------------------------------------------
      bool addModuleLine(const std::string& section,
                         const std::string& moduleName,
                         const std::string& instanceName,
                         const std::string& parFileName,
                         const std::string& parSectionName);

      // ------------------------------------------------------------------
      // convert a module name to an instance name.
      // ------------------------------------------------------------------
      std::string moduleToInstance(const std::string& section,
                                   const std::string& moduleName) const;

   };

#endif
