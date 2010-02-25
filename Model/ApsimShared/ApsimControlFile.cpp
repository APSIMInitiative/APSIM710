#include <set>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>

#include <General/string_functions.h>
#include <General/stl_functions.h>
#include <General/io_functions.h>
#include <General/stristr.h>
#include <General/IniFile.h>
#include <General/path.h>
#include <General/date_class.h>
#include <General/StringTokenizer.h>
#include <General/TreeNodeIterator.h>
#include <General/xml.h>

#include <boost/bind.hpp>
#include <boost/regex.hpp>
#include "ApsimDirectories.h"
#include "ApsimDataTypesFile.h"

#include "ApsimControlFile.h"

#include "Phi_functions.h"
using namespace std;
using namespace boost;

struct ParamFile
   {
   string moduleName;
   string dllFileName;
   string instanceName;
   string fileName;
   string sectionName;

   bool operator==(const ParamFile& rhs)
      {
      return (Str_i_Eq(moduleName, rhs.moduleName) &&
              Str_i_Eq(instanceName, rhs.instanceName) &&
              Str_i_Eq(fileName, rhs.fileName) &&
              Str_i_Eq(sectionName, rhs.sectionName));
      }

   void fixUpModuleName(void)
      {
      if (moduleName.find('/') != string::npos ||
          moduleName.find('\\') != string::npos ||
          moduleName.find('.') != string::npos)
         {
         dllFileName = moduleName;
         moduleName = fileRoot(fileTail(moduleName));
         }
      else
         {
         dllFileName = "%apsim%/Model/"
                     + moduleName +
#ifdef __WIN32__
                     ".dll";
#else
                     ".so";
#endif
         }
      }
   };

// ------------------------------------------------------------------
// Parse the specified 'module=' line to extract the module name,
// the instance name and the parameter files and sections referenced.
// Throws exception on error.
// ------------------------------------------------------------------
void parseModuleLine(const string& controlFileName, const string& moduleLine,
                     vector<ParamFile>& paramFiles,
                     bool removeMacros = true) throw(runtime_error)
   {
   string line = moduleLine;

   // remove any comments
   unsigned posComment = line.find('!');
   if (posComment != string::npos)
      line.erase(posComment);

   // initialise everything to zero.
   string moduleName;
   string instanceName;
   string paramFile;
   string section;
   bool foundAFile = false;

   // setup state names
   static const int READ_MODULE_NAME = 0;
   static const int READ_INSTANTIATION = 1;
   static const int READ_PARAM_FILE = 2;
   static const int READ_SECTION = 3;

   int state = READ_MODULE_NAME;
   int currentPos = 0;
   bool inQuotes = false;
   while (line[currentPos] != 0)
      {
      char ch = line[currentPos];

      switch (state)
         {
         case READ_MODULE_NAME :
                      if (ch == '\'' || ch == '\"')
                         inQuotes = !inQuotes;
                      if (inQuotes)
                         moduleName += ch;
                      else if (ch == ' ')
                         state = READ_PARAM_FILE;
                      else if (ch == '(')
                         state = READ_INSTANTIATION;
                      else if (ch == '[')
                         state = READ_SECTION;
                      else
                         moduleName += ch;
                      break;
         case READ_INSTANTIATION :
                      if (ch == ')')
                         state = READ_PARAM_FILE;
                      else
                         instanceName += ch;
                      break;
         case READ_PARAM_FILE :
                      foundAFile = true;
                      if (ch == '\'' || ch == '\"')
                         inQuotes = !inQuotes;
                      if (inQuotes)
                         paramFile += ch;
                      else if (ch == '(')
                         state = READ_INSTANTIATION;
                      else if (ch == '[')
                         state = READ_SECTION;
                      else
                         paramFile += ch;
                      break;
         case READ_SECTION :
                      if (ch == ']')
                         {
                         state = READ_PARAM_FILE;
                         stripLeadingTrailing(paramFile, " ");
                         stripLeadingTrailing(section, " ");
                         if (paramFile.length() == 0)
                            paramFile = controlFileName;
                         else if (removeMacros)
                            {
                            // replace any apsuite macros.
                            Replace_all(paramFile, "%apsim%", getApsimDirectory().c_str());
                            }
                         replaceAll(paramFile, "\'", "");
                         replaceAll(paramFile, "\"", "");

                         replaceAll(moduleName, "\'", "");
                         replaceAll(moduleName, "\"", "");
                         ParamFile p;
                         p.fileName = paramFile;
                         p.sectionName = section;
                         p.moduleName = moduleName;
                         p.instanceName = instanceName;
                         if (instanceName == "")
                            {
                            if (moduleName.find('.') != string::npos)
                               p.instanceName = Path(moduleName).Get_name_without_ext();
                            else
                               p.instanceName = moduleName;
                            }
                         p.fixUpModuleName();
                         paramFiles.push_back(p);
                         paramFile = "";
                         section = "";
                         }
                      else
                         section += ch;
                      break;
         }
      currentPos++;
      }
   if (!foundAFile && moduleName != "")
      {
      replaceAll(moduleName, "\'", "");
      replaceAll(moduleName, "\"", "");
      ParamFile p;
      p.moduleName = moduleName;
      p.instanceName = instanceName;
      if (instanceName == "")
         {
         p.instanceName = fileRoot(moduleName);
         }
      p.fixUpModuleName();
      paramFiles.push_back(p);
      return;
      }

   if (state != READ_PARAM_FILE)
      throw runtime_error("Invalid control file line: " + moduleLine);
   }
// ------------------------------------------------------------------
// Return a list of all modules that this control file references.
// ------------------------------------------------------------------
void getParameterFiles(IniFile* ini,
                       const string& section,
                       vector<ParamFile>& paramFiles)
   {
   // read in all 'module=' lines from control file.
   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);

   // loop through all module lines
   for (vector<string>::const_iterator moduleLineI = moduleLines.begin();
                                       moduleLineI != moduleLines.end();
                                       moduleLineI++)
      {
      // remove any param files with a blank filename.
      vector<ParamFile> parFiles;
      parseModuleLine(ini->getFileName(), *moduleLineI, parFiles);
      for (unsigned p = 0; p != parFiles.size(); p++)
         {
         if (parFiles[p].fileName != "")
            paramFiles.push_back(parFiles[p]);
         }
      }
   }
// ------------------------------------------------------------------
// Return all the parameter files for the specified section and instance.
// ------------------------------------------------------------------
void getParameterFilesForInstance(IniFile* ini,
                                  const string& section,
                                  const string& instanceName,
                                  vector<ParamFile>& paramFiles,
                                  bool constants)
   {
   vector<ParamFile> parFiles;
   getParameterFiles(ini, section, parFiles);
   for (vector<ParamFile>::const_iterator paramFile = parFiles.begin();
                                          paramFile != parFiles.end();
                                          paramFile++)
      {
      bool doAdd = (instanceName == "" || Str_i_Eq(paramFile->instanceName, instanceName));
      if (doAdd)
         {
         if (!constants)
            doAdd = (ExtractFileExt(paramFile->fileName.c_str()) != ".ini");
         }
      if (doAdd)
         paramFiles.push_back(*paramFile);
      }
   }
// ------------------------------------------------------------------
// Return all the parameter files for the specified section and instance.
// ------------------------------------------------------------------
void getParameterFilesForModule(IniFile* ini,
                                const string& section,
                                const string& moduleName,
                                vector<ParamFile>& paramFiles,
                                bool constants)
   {
   vector<ParamFile> parFiles;
   getParameterFiles(ini, section, parFiles);
   for (vector<ParamFile>::const_iterator paramFile = parFiles.begin();
                                          paramFile != parFiles.end();
                                          paramFile++)
      {
      bool doAdd = (moduleName == "" || Str_i_Eq(paramFile->moduleName, moduleName));
      if (doAdd)
         {
         if (!constants)
            doAdd = (ExtractFileExt(paramFile->fileName.c_str()) != ".ini");
         }
      if (doAdd)
         paramFiles.push_back(*paramFile);
      }
   }
// ------------------------------------------------------------------
// return a list of parameter file sections for the given par
// that match the specified ParamFile struct.
// ------------------------------------------------------------------
class MatchPrefixStringAndStore
   {
   private:
      vector<string>& matchingStrings;
      const string& prefixToMatch;
   public:
      MatchPrefixStringAndStore(const string& prefix, vector<string>& matchingstrings)
         : prefixToMatch(prefix), matchingStrings(matchingstrings) { }
      void operator() (const string& st)
         {
         if (Str_i_Eq(st.substr(0, prefixToMatch.length()), prefixToMatch))
            matchingStrings.push_back(st);
         }
   };

void getParFileSectionsMatching(IniFile* par, ParamFile paramFile,
                                vector<string>& paramFileSections)
   {
   string sectionToMatch = paramFile.sectionName + "." + paramFile.instanceName + ".";

   vector<string> sections;
   par->readSectionNames(sections);
   for_each(sections.begin(), sections.end(),
            MatchPrefixStringAndStore(sectionToMatch, paramFileSections));
   }

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimControlFile::ApsimControlFile(const string& fileName)
   {
   ini = new IniFile(fileName);
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
ApsimControlFile::~ApsimControlFile(void)
   {
   delete ini;
   for (unsigned i = 0; i != openedParFiles.size(); i++)
      delete openedParFiles[i];
   }
// ------------------------------------------------------------------
// return filename to caller.
// ------------------------------------------------------------------
string ApsimControlFile::getFileName(void) const
   {
   return ini->getFileName();
   }
// ------------------------------------------------------------------
// return true if the specified section is a valid one.
// ------------------------------------------------------------------
bool ApsimControlFile::isValid(const std::string& section)
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);
   return (paramFiles.size() > 0);
   }
// ------------------------------------------------------------------
// Return a list of all section names to caller.
// ------------------------------------------------------------------
void ApsimControlFile::getAllSectionNames(vector<string>& sectionNames)
   {
   ini->readSectionNames(sectionNames);
   }
// ------------------------------------------------------------------
// return a list of all filenames specified in the control file section.
// NB: This list doesn't include output file names.
// ------------------------------------------------------------------
void ApsimControlFile::getAllFiles(const string& section,
                                   vector<string>& fileNames) const throw(runtime_error)
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);
   for (vector<ParamFile>::const_iterator paramFileI = paramFiles.begin();
                                          paramFileI != paramFiles.end();
                                          paramFileI++)
      {
      if (find(fileNames.begin(), fileNames.end(), paramFileI->fileName) == fileNames.end())
         fileNames.push_back(paramFileI->fileName);
      }
   }
// ------------------------------------------------------------------
// Return the referenced file for the specified instance e.g. met and soi.
// ------------------------------------------------------------------
string ApsimControlFile::getFileForInstance(const string& section,
                                            const string& instanceName) const
   {
   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);
   for (unsigned m = 0; m != moduleLines.size(); m++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(ini->getFileName(), moduleLines[m], paramFiles);
      if (paramFiles.size() > 0 && Str_i_Eq(paramFiles[0].instanceName, instanceName))
         return paramFiles[0].fileName;
      }
   return "";
   }
// ------------------------------------------------------------------
// Return the .ini file name for the specified instance.
// ------------------------------------------------------------------
std::string ApsimControlFile::getIniFileForInstance(const std::string& section,
                                                    const std::string& instanceName) const
   {
   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);
   for (unsigned m = 0; m != moduleLines.size(); m++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(ini->getFileName(), moduleLines[m], paramFiles);
      for (unsigned p = 0; p != paramFiles.size(); p++)
         {
         if (paramFiles[0].instanceName != instanceName)
            break;
         if (fileExtensionEquals(paramFiles[p].fileName, "ini"))
            return paramFiles[p].fileName;
         }
      }
   return "";
   }
// ------------------------------------------------------------------
// Return a list of all output file names for the specified section
// ------------------------------------------------------------------
void ApsimControlFile::getOutputFileNames(const string& section,
                                          vector<string>& fileNames) const
   {
   vector<string> instanceNames;
   getInstances(section, "report", instanceNames);
   for (unsigned i = 0; i != instanceNames.size(); i++)
      getParameterValues(section, instanceNames[i], "outputfile", fileNames);
   }
// ------------------------------------------------------------------
// Return the name of the summary file
// ------------------------------------------------------------------
string ApsimControlFile::getSummaryFileName(const string& section) const
   {
   return getParameterValue(section, "summaryfile", "summaryfile");
   }
// ------------------------------------------------------------------
// Return a list of instance names for the specified module name.
// ------------------------------------------------------------------
void ApsimControlFile::getInstances(const string& section,
                                    const string& moduleName,
                                    vector<string>& instanceNames) const
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);
   for (vector<ParamFile>::const_iterator paramFileI = paramFiles.begin();
                                          paramFileI != paramFiles.end();
                                          paramFileI++)
      {
      if (Str_i_Eq(moduleName, paramFileI->moduleName) &&
          find(instanceNames.begin(), instanceNames.end(),
               paramFileI->instanceName) == instanceNames.end())
         instanceNames.push_back(paramFileI->instanceName);
      }
   }
// ------------------------------------------------------------------
// Return a list of module names and their instance names.
// ------------------------------------------------------------------
void ApsimControlFile::getAllModuleInstances(const std::string& section,
                                             ModuleInstances& moduleInstances) const
   {
   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);
   for (unsigned m = 0; m != moduleLines.size(); m++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(ini->getFileName(), moduleLines[m], paramFiles);
      if (paramFiles.size() > 0)
         {
         ModuleInstance instance;
         instance.moduleName = paramFiles[0].moduleName;
         instance.instanceName = paramFiles[0].instanceName;
         instance.dllFileName = paramFiles[0].dllFileName;
         for (unsigned p = 0; p != paramFiles.size(); p++)
            instance.ParFiles.push_back(make_pair(paramFiles[p].fileName, paramFiles[p].sectionName));

         moduleInstances.push_back(instance);
         }
      }
   }
// ------------------------------------------------------------------
// Return a list of all parameter values for the specified module
// and parameter name.
// ------------------------------------------------------------------
void ApsimControlFile::getParameterValues(const string& section,
                                          const string& instanceName,
                                          const string& parameterName,
                                          vector<string>& values) const
   {
   vector<ParamFile> paramFiles;
   getParameterFilesForInstance(ini, section, instanceName, paramFiles, false);
   for (vector<ParamFile>::const_iterator
                 paramFile = paramFiles.begin();
                 paramFile != paramFiles.end();
                 paramFile++)
      {
      IniFile* par = getParFile(paramFile->fileName);
      if (par != NULL)
         {
         vector<string> paramFileSections;
         getParFileSectionsMatching(par, *paramFile, paramFileSections);
         for (unsigned p = 0; p != paramFileSections.size(); p++)
            par->read(paramFileSections[p], parameterName, values);
         }
      }
   }
// -----------------------------------------------------------------
// Return a list of all sections within par files for a given module
// instance.
// -----------------------------------------------------------------
void ApsimControlFile::getParameterSections(const std::string& section,
                                            const std::string& instanceName,
                                            std::vector<IniFile*>& inis,
                                            std::vector<std::string>& fullSectionNames)
   {
   vector<ParamFile> paramFiles;
   getParameterFilesForInstance(ini, section, instanceName, paramFiles, false);
   for (vector<ParamFile>::const_iterator
                 paramFile = paramFiles.begin();
                 paramFile != paramFiles.end();
                 paramFile++)
      {
      IniFile* par = getParFile(paramFile->fileName);
      if (par != NULL)
         {
         vector<string> paramFileSections;
         getParFileSectionsMatching(par, *paramFile, paramFileSections);
         for (unsigned p = 0; p != paramFileSections.size(); p++)
            {
            inis.push_back(par);
            fullSectionNames.push_back(paramFileSections[p]);
            }
         }
      }
   }
// ------------------------------------------------------------------
// Return a single parameter value for the specified module
// and parameter name.
// ------------------------------------------------------------------
string ApsimControlFile::getParameterValue(const string& section,
                                           const string& instanceName,
                                           const string& parameterName) const throw(runtime_error)
   {
   vector<string> values;
   getParameterValues(section, instanceName, parameterName, values);
   if (values.size() == 0)
      return "";
   if (values.size() > 1)
      return "";
   return values[0];
   }
// ------------------------------------------------------------------
// Set the values of a parameter for a module
// If moduleName is blank then parameter will be written to control file
// ------------------------------------------------------------------
void ApsimControlFile::setParameterValues(const string& sectionName,
                                          const string& instanceName,
                                          const string& parameterName,
                                          const string& parameterSectionName,
                                          const vector<string>& parameterValues) throw(std::runtime_error)
   {
   if (instanceName == "")
      throw runtime_error("shouldn't be here in ApsimControlFile::setParameterValues");

   else
      {
      IniFile* par = NULL;
      string paramFileName;
      string paramSectionName;
      if (!findParameterName(sectionName, instanceName, parameterName,
                             par, paramFileName, paramSectionName))
         {
         // see if there are any parameter files for this instance.  If so
         // then use the first one to write the parameter to under a default
         // section name.  If not then use a default parameter file and section name.
         vector<ParamFile> paramFiles;
         getParameterFilesForInstance(ini, sectionName, instanceName, paramFiles, false);
         if (paramFiles.size() > 0)
            {
            paramFileName = paramFiles[0].fileName;
            par = getParFile(paramFileName);
            if (par != NULL)
               {
               vector<string> paramFileSections;
               getParFileSectionsMatching(par, paramFiles[0], paramFileSections);
               if (paramFileSections.size() > 0)
                  paramSectionName = paramFileSections[0];
               else
                  paramSectionName = "default";
               }
            }
         else
            {
            getDefaultParFileAndSection(sectionName, paramFileName, paramSectionName);
            par = getParFile(paramFileName);
            if (par != NULL)
               paramSectionName = paramSectionName + "." + instanceName + ".parameters";
            }
         }
      if (par != NULL)
         {
         if (parameterSectionName != "")
            paramSectionName = parameterSectionName;
         par->write(paramSectionName, parameterName, parameterValues);
         addModuleLine(sectionName, instanceName, instanceName,
                       paramFileName, paramSectionName);
         }
      }
   }
// ------------------------------------------------------------------
// Set the value of a parameter for a module
// If moduleName is blank then parameter will be written to control file
// ------------------------------------------------------------------
void ApsimControlFile::setParameterValue(const string& sectionName,
                                         const string& instanceName,
                                         const string& parameterName,
                                         const string& parameterValue) throw(std::runtime_error)
   {
   vector<string> values;
   values.push_back(parameterValue);
   setParameterValues(sectionName, instanceName, parameterName, "", values);
   }
// ------------------------------------------------------------------
// change the name of a module in the control file.  Return true
// on success.
// ------------------------------------------------------------------
bool ApsimControlFile::changeModuleName(const std::string& section,
                                        const std::string& oldModuleName,
                                        const std::string& newModuleName)
   {
   bool changeMade = false;

   vector<string> lines;
   ini->read(section, "module", lines);


   // loop through all lines in control file looking for a module = oldModuleName
   for(vector<string>::iterator line = lines.begin();
                                line != lines.end();
                                line++)
      {
      unsigned posStartModuleName =line->find_first_not_of(' ');
      if (posStartModuleName != string::npos)
         {
         string moduleName = *line;
         unsigned posEndModuleName = line->find_first_of(" \n", posStartModuleName);
         if (posEndModuleName != string::npos)
            moduleName = line->substr(posStartModuleName, posEndModuleName - posStartModuleName);

         if (Str_i_Eq(moduleName, oldModuleName))
            {
            line->replace(posStartModuleName, moduleName.length(), newModuleName);
            changeMade = true;
            }
         }
      }
   if (changeMade)
      ini->write(section, "module", lines);
   return changeMade;
   }
// ------------------------------------------------------------------
// Return a title to caller.
// ------------------------------------------------------------------
string ApsimControlFile::getTitle(const std::string& section) const
   {
   string title;
   ini->read(section, "title", title);
   return title;
   }
// ------------------------------------------------------------------
// set the title of control file.
// ------------------------------------------------------------------
void ApsimControlFile::setTitle(const std::string& section,
                                const std::string& title)
   {
   ini->write(section, "title", title);
   }
// ------------------------------------------------------------------
// Return the version number of the specified control file.
// ------------------------------------------------------------------
std::string ApsimControlFile::getVersionNumber(const std::string& fileName)
   {
   ifstream control(fileName.c_str());
   string line;
   while (getline(control, line) && getSectionName(line) == "")
      {
      string version = getKeyValue(line, "version");
      if (version != "")
         return version;
      }
   return "2.1"; // assumes if no version number then it is version 2.1
   }
// ------------------------------------------------------------------
// Set the version number in the control file to the current
// apsim version number
// ------------------------------------------------------------------
void ApsimControlFile::setVersionNumber(const std::string& fileName,
                                        const string& versionNumber)
   {
   // Get entire control file contents.
   ifstream control(fileName.c_str());

   string linesSoFar;
   string line;
   while (getline(control, line) && getSectionName(line) == "")
      {
      string version = getKeyValue(line, "version");
      if (version != "")
         break;
         linesSoFar += line + "\n";
      }
   ostringstream out;
   out << control.rdbuf();
   control.close();
   ofstream controlOut(fileName.c_str());
   controlOut << linesSoFar;
   controlOut << "version = " << versionNumber << endl;
   if (getSectionName(line) != "")
      controlOut << line << endl;
   controlOut << out.str();
   }
// ------------------------------------------------------------------
// return an opened parameter file ready to read.
// ------------------------------------------------------------------
IniFile* ApsimControlFile::getParFile(const std::string& parFileName, bool checkNonExistant) const
   {
   IniFile* iniToReturn = NULL;

   string filePath = parFileName;
   if (fileDirName(filePath) == "")
      {
      string parFileDir = fileDirName(ini->getFileName());
      if (parFileDir != "")
         filePath = parFileDir + "/" + parFileName;
      }
   for (unsigned i = 0; iniToReturn == NULL && i != openedParFiles.size(); i++)
      {
      if (Str_i_Eq(openedParFiles[i]->getFileName(), filePath))
         iniToReturn = openedParFiles[i];
      }
   if (checkNonExistant && !fileExists(filePath))
      throw runtime_error("The control file has referenced a non-existant file.\n"
                          "File = " + filePath);

   if (iniToReturn == NULL &&
       !fileExtensionEquals(filePath, "met") &&
       !fileExtensionEquals(filePath, "soi"))
      {
      IniFile* par = new IniFile(filePath, true);
      openedParFiles.push_back(par);
      iniToReturn = par;
      }

   return iniToReturn;
   }
// ------------------------------------------------------------------
// Find a parameter in parameter file.  Return true and the par and
// section name where parameter is located.
// ------------------------------------------------------------------
bool ApsimControlFile::findParameterName(const string& section,
                                         const string& instanceName,
                                         const string& parameterName,
                                         IniFile*& par,
                                         string& parameterFileName,
                                         string& parameterSection) const
   {
   vector<ParamFile> paramFiles;
   getParameterFilesForInstance(ini, section, instanceName, paramFiles, false);
   for (unsigned p = 0; p != paramFiles.size(); p++)
      {
      parameterFileName = paramFiles[p].fileName;
      par = getParFile(parameterFileName);
      if (par != NULL)
         {
         vector<string> paramFileSections;
         getParFileSectionsMatching(par, paramFiles[p], paramFileSections);
         for (unsigned s = 0; s != paramFileSections.size(); s++)
            {
            string value;
            if (par->read(paramFileSections[s], parameterName, value))
               {
               parameterSection = paramFileSections[s];
               return true;
               }
            }
         }
      }
   par = NULL;
   parameterFileName = "";
   parameterSection = "";
   return false;
   }
// ------------------------------------------------------------------
// create a new module line from the given ParFiles.
// ------------------------------------------------------------------
string createModuleLine(vector<ParamFile>& paramFiles)
   {
   if (paramFiles.size() > 0)
      {
      string newModuleLine =paramFiles[0].moduleName;
      if (paramFiles[0].moduleName != paramFiles[0].instanceName)
         newModuleLine += "(" + paramFiles[0].instanceName + ")";
      newModuleLine += "  ";
      for (unsigned p = 0; p != paramFiles.size(); p++)
         {
         if (paramFiles[p].fileName != "")
            newModuleLine += " " + paramFiles[p].fileName + " [" + paramFiles[p].sectionName + "]";
         }
      return newModuleLine;
      }
   return "";
   }
// ------------------------------------------------------------------
// write new module= line to control file.
// ------------------------------------------------------------------
bool ApsimControlFile::addModuleLine(const string& section,
                                     const string& moduleName,
                                     const string& instanceName,
                                     const string& parFileName,
                                     const string& parSectionName)
   {
   bool found = false;
   ParamFile newParamFile;
   newParamFile.moduleName = moduleName;
   newParamFile.instanceName = instanceName;
   newParamFile.fileName = parFileName;
   newParamFile.sectionName = parSectionName;
   unsigned posPeriod = parSectionName.find('.');
   if (posPeriod != string::npos)
     newParamFile.sectionName.erase(posPeriod);

   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);
   for (unsigned i = 0; i != moduleLines.size(); i++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(ini->getFileName(), moduleLines[i], paramFiles, false);
      if (Str_i_Eq(paramFiles[0].instanceName, instanceName))
         {
         newParamFile.moduleName = paramFiles[0].moduleName;
         if (find(paramFiles.begin(), paramFiles.end(), newParamFile) != paramFiles.end())
            return false;
         paramFiles.push_back(newParamFile);

         moduleLines[i] = createModuleLine(paramFiles);
         found = true;
         }
      }
   if (!found)
      {
      vector<ParamFile> paramFiles;
      paramFiles.push_back(newParamFile);
      moduleLines.push_back(createModuleLine(paramFiles));
      }
   ini->write(section, "module", moduleLines);
   return found;
   }
// ------------------------------------------------------------------
// convert a module name to an instance name.
// ------------------------------------------------------------------
string ApsimControlFile::moduleToInstance(const string& section,
                                          const string& moduleName) const
   {
   vector<string> instanceNames;
   getInstances(section, moduleName, instanceNames);
   if (instanceNames.size() == 0)
      return "";
   return instanceNames[0];
   }
// ------------------------------------------------------------------
// Get a parameter file from the control file - any module will do.
// Also return a section name.
// ------------------------------------------------------------------
void ApsimControlFile::getDefaultParFileAndSection(const string& section,
                                                   string& defaultFile,
                                                   string& defaultSection) const
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);

   defaultFile = "";
   defaultSection = "";

   unsigned i = 0;
   while (i < paramFiles.size()
      && (defaultFile == ""
          ||ExtractFileExt(defaultFile.c_str()) == ".ini"
          || ExtractFileExt(defaultFile.c_str()) == ".met"
          || ExtractFileExt(defaultFile.c_str()) == ".con"))
      {
      defaultFile = paramFiles[i].fileName;
      defaultSection = paramFiles[i].sectionName;
      i++;
      }
   if (defaultFile == ""
       || ExtractFileExt(defaultFile.c_str()) == ".ini"
       || ExtractFileExt(defaultFile.c_str()) == ".met")
      {
      defaultFile = "default.par";
      defaultSection = "default";
      }
   }
// ------------------------------------------------------------------
// Rename the specified parameter in all instances of module.
// Return true if change was made.
// ------------------------------------------------------------------
class RenameParameter
   {
   public:
      RenameParameter(const string& oldP, const string& newP, bool& m)
         : oldName(oldP), newName(newP), modified(m) { }

      void callback(IniFile* par, const string& section)
         {
         if (par->renameKey(section, oldName, newName))
            modified = true;
         }

      bool& modified;
   private:
      const string& oldName;
      const string& newName;
   };
bool ApsimControlFile::renameParameter(const std::string& sectionName,
                                       const std::string& moduleName,
                                       const std::string& oldParameterName,
                                       const std::string& newParameterName)
   {
   bool modified = false;
   RenameParameter rename(oldParameterName, newParameterName, modified);
   enumerateParameters(sectionName, moduleName, false, boost::bind(&RenameParameter::callback, &rename, _1, _2));
   return modified;
   }
// ------------------------------------------------------------------
// Delete the specified parameter from all instances of module.
// ------------------------------------------------------------------
class DeleteParameter
   {
   public:
      DeleteParameter(const string& pName, vector<string>& v)
         : paramName(pName), values(v) { }

      void callback(IniFile* par, const string& section)
         {
         par->read(section, paramName, values);
         par->deleteKey(section, paramName);
         }

   private:
      const string& paramName;
      vector<string>& values;
   };
bool ApsimControlFile::deleteParameter(const std::string& sectionName,
                                       const std::string& moduleName,
                                       const std::string& parameterName)
   {
   vector<string> values;
   DeleteParameter deleteParam(parameterName, values);
   enumerateParameters(sectionName, moduleName, false, boost::bind(&DeleteParameter::callback, &deleteParam, _1, _2));
   return (values.size() > 0);
   }
// ------------------------------------------------------------------
// Moves the specified parameter from all instances of module to the
// specified destination instance.  Return true if change was made.
// ------------------------------------------------------------------
bool ApsimControlFile::moveParameter(const std::string& sectionName,
                                     const std::string& moduleName,
                                     const std::string& parameterName,
                                     const std::string& destModuleName)
   {
   // need to locate parameter so that we can get the section name where
   // the parameter is currently located.
   string instanceName = moduleToInstance(sectionName, moduleName);
   IniFile* par;
   string parameterFileName, parameterSectionName;
   if (findParameterName(sectionName, instanceName, parameterName,
                         par, parameterFileName, parameterSectionName))
      {
      vector<string> values;
      DeleteParameter deleteParam(parameterName, values);
      enumerateParameters(sectionName, moduleName, false, boost::bind(&DeleteParameter::callback, &deleteParam, _1, _2));
      if (values.size() > 0)
         {
         if (destModuleName == "")
            {
            if (Str_i_Eq(parameterName, "title"))
               setTitle(sectionName, values[0]);
            }
         else
            {
            string instanceName = moduleToInstance(sectionName,destModuleName);
            if (instanceName == "")
               instanceName = destModuleName;

            // only use the first word of the parameterSectionName
            parameterSectionName = StringTokenizer(parameterSectionName, ".").nextToken()
                                 + "." + instanceName + ".parameters";
            setParameterValues(sectionName, instanceName, parameterName,
                               parameterSectionName, values);
            }
         return true;
         }
      }
   return false;
   }
// ------------------------------------------------------------------
// Return true if the control file has parameters in it.
// ------------------------------------------------------------------
bool ApsimControlFile::hasParametersInCon(const std::string& section)
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);

   for (unsigned i = 0; i != paramFiles.size(); ++i)
      {
      if (paramFiles[i].fileName == ini->getFileName())
         return true;
      }
   return false;
   }
// ------------------------------------------------------------------
// remove all references to this control file from the list of
// parameter files for all modules.
// ------------------------------------------------------------------
void ApsimControlFile::removeSelfReferences(const std::string& section,
                                            const std::string& parFileForConParams)
   {
   // read in all 'module=' lines from control file.
   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);

   // loop through all module lines
   vector<string> lines;
   for (unsigned m = 0; m != moduleLines.size(); m++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(ini->getFileName(), moduleLines[m], paramFiles, false);
      bool badReferenceFound = false;
      for (unsigned p = 0; p != paramFiles.size(); ++p)
         {
         if (paramFiles[p].fileName == ini->getFileName())
            {
            paramFiles[p].fileName = parFileForConParams;
            badReferenceFound = true;
            }
         }
      if (badReferenceFound)
         moduleLines[m] = createModuleLine(paramFiles);
      }
   ini->write(section, "module", moduleLines);
   }
// ------------------------------------------------------------------
// move all parameters out of the control file and into a par
// file.  Return true if parameters were moved.
// ------------------------------------------------------------------
bool ApsimControlFile::moveParametersOutOfCon(const std::string& section,
                                              const std::string& parFileForConParams)
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);

   // if parFileToUse is blank then ask user for a file to put all
   // parameters into.

   if (!fileExists(parFileForConParams))
      {
      ofstream dummy(parFileForConParams.c_str());
      dummy.close();
      }
   IniFile* par = getParFile(parFileForConParams, false);
   if (par != NULL)
      {
      bool selfReferencesFound = false;
      for (unsigned i = 0; i != paramFiles.size(); ++i)
         {
         if (paramFiles[i].fileName == ini->getFileName())
            {
            selfReferencesFound = true;

            vector<string> paramFileSections;
            getParFileSectionsMatching(ini, paramFiles[i], paramFileSections);
            for (unsigned s = 0; s != paramFileSections.size(); s++)
               {
               string contents;
               ini->readSection(paramFileSections[s], contents);
               ini->deleteSection(paramFileSections[s]);
               par->writeSection(paramFileSections[s], contents);
               }
            }
         }
      // tell control file to remove the self reference for this instance.
      if (selfReferencesFound)
         removeSelfReferences(section, parFileForConParams);
      return selfReferencesFound;
      }
   return false;
   }
// ------------------------------------------------------------------
// Enumerate all parameter sections for the specified module name.
// ------------------------------------------------------------------
void ApsimControlFile::enumerateParameters(const std::string& section,
                                           const std::string& moduleName,
                                           bool includeConstants,
                                           boost::function2<void, IniFile*, const std::string&> callback)
   {
   vector<ParamFile> paramFiles;
   getParameterFilesForModule(ini, section, moduleName, paramFiles, includeConstants);
   for (unsigned p = 0; p != paramFiles.size(); p++)
      {
      if (paramFiles[p].fileName != "")
         {
         IniFile* par = getParFile(paramFiles[p].fileName);
         if (par != NULL)
            {
            vector<string> paramFileSections;
            getParFileSectionsMatching(par, paramFiles[p], paramFileSections);
            for (unsigned s = 0; s != paramFileSections.size(); s++)
               callback(par, paramFileSections[s]);
            }
         }
      }
   }
// ------------------------------------------------------------------
// Enumerate all parameter sections for the specified instance name.
// ------------------------------------------------------------------
void ApsimControlFile::enumerateParametersForInstance(const std::string& section,
                                                      const std::string& instanceName,
                                                      bool constantsOnly,
                                                      boost::function2<void, IniFile*, const std::string&> callback)
   {
   vector<ParamFile> paramFiles;
   getParameterFilesForInstance(ini, section, instanceName, paramFiles, constantsOnly);
   for (unsigned p = 0; p != paramFiles.size(); p++)
      {
      if (paramFiles[p].fileName != "")
         {
         if (!constantsOnly
             || fileExtensionEquals(paramFiles[p].fileName, "ini"))
            {
            IniFile* par = getParFile(paramFiles[p].fileName);
            if (par != NULL)
               {
               vector<string> paramFileSections;
               getParFileSectionsMatching(par, paramFiles[p], paramFileSections);
               if (paramFileSections.size() == 0)
                  throw runtime_error(string("No matching sections were found in parameter file.")
                                      + "\nSection name = " + paramFiles[p].sectionName
                                      + "\nFile name = " + par->getFileName()
                                      + "\nInstance name = " + paramFiles[p].instanceName);
               for (unsigned s = 0; s != paramFileSections.size(); s++)
                  callback(par, paramFileSections[s]);
               }
            }
         }
      }
   }
// ------------------------------------------------------------------
// Add a parameter file reference to all instances of the
// specified module. Return true if the con file was modified.
// ------------------------------------------------------------------
bool ApsimControlFile::addParameterFileReference(const std::string& section,
                                                 const std::string& moduleName,
                                                 const std::string& parameterFileName,
                                                 const std::string& parameterSectionName)
   {
   bool someFound = false;
   vector<string> instanceNames;
   getInstances(section, moduleName, instanceNames);
   for (unsigned i = 0; i != instanceNames.size(); i++)
      someFound = (addModuleLine(section, moduleName, instanceNames[i], parameterFileName, parameterSectionName) || someFound);
   return (someFound);
   }
// ------------------------------------------------------------------
// Set the module= line in the control for the specified module.
// ------------------------------------------------------------------
void setModuleLine(IniFile* ini,
                   const std::string& section,
                   const std::string& moduleName,
                   vector<ParamFile>& newParamFiles)
   {
   bool moduleLineFound = false;
   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);
   for (unsigned i = 0; i != moduleLines.size(); i++)
      {
      vector<ParamFile> oldParamFiles;
      parseModuleLine(ini->getFileName(), moduleLines[i], oldParamFiles, false);
      if (oldParamFiles[0].moduleName == moduleName)
         {
         for (unsigned p = 0; p != newParamFiles.size(); p++)
            {
            newParamFiles[p].moduleName = oldParamFiles[0].moduleName;
            newParamFiles[p].instanceName = oldParamFiles[0].instanceName;
            }
         if (newParamFiles.size() == 0)
            {
            ParamFile paramFile;
            paramFile.moduleName = oldParamFiles[0].moduleName;
            paramFile.instanceName = oldParamFiles[0].instanceName;
            newParamFiles.push_back(paramFile);
            }

         moduleLines[i] = createModuleLine(newParamFiles);
         moduleLineFound = true;
         }
      }
   if (!moduleLineFound)
      moduleLines.push_back(createModuleLine(newParamFiles));
   ini->write(section, "module", moduleLines);
   }
// ------------------------------------------------------------------
// Rename the specified module
// ------------------------------------------------------------------
bool ApsimControlFile::renameModule(const std::string& section,
                                    const std::string& oldModuleName,
                                    const std::string &newModuleName)
   {
   bool modsMade = false;

   // Get a complete list of all par/ini files for specified module.
   vector<ParamFile> paramFiles;
   getParameterFilesForModule(ini, section, oldModuleName, paramFiles, true);
   set<string> parFileNames;
   for (unsigned p = 0; p != paramFiles.size(); p++)
      parFileNames.insert(paramFiles[p].fileName);

   // rename all relevant sections.
   for (set<string>::iterator paramFile = parFileNames.begin();
                              paramFile != parFileNames.end();
                              paramFile++)
      {
      if (paramFile->find(".ini") == string::npos && paramFile->find(".xml") == string::npos)
         {
         IniFile* par = getParFile(*paramFile);
         vector<string> sectionNames;
         par->readSectionNames(sectionNames);
         for (unsigned s = 0; s != sectionNames.size(); s++)
            {
            StringTokenizer tokenizer(sectionNames[s], ".");
            string firstBit = tokenizer.nextToken();
            string secondBit = tokenizer.nextToken();
            string thirdBit = tokenizer.nextToken();
            if (Str_i_Eq(secondBit, oldModuleName))
               {
               par->renameSection(sectionNames[s],
                                  firstBit + "." + newModuleName + "." + thirdBit);
               modsMade = true;
               }
            }
         }
      }

   // change the module equals line in control file.
   modsMade = (changeModuleName(section, oldModuleName, newModuleName) || modsMade);

   // rename the .xml filename and change the control file to reflect the change.
   string oldIniFileName = oldModuleName + ".xml";
   string newIniFileName = newModuleName + ".xml";
   for (unsigned p = 0; p != paramFiles.size(); p++)
      {
      unsigned posIni = findSubString(paramFiles[p].fileName, oldIniFileName);
      if (posIni != string::npos)
         {
         paramFiles[p].fileName = "%apsim%/Model/" + newIniFileName;
         modsMade = true;
         }
      posIni = findSubString(paramFiles[p].fileName, oldModuleName+".ini");
      if (posIni != string::npos)
         {
         paramFiles[p].fileName = "%apsim%/Model/" + newIniFileName;
         modsMade = true;
         }
      }

   if (modsMade)
      {
      for (unsigned p = 0; p != paramFiles.size(); p++)
         replaceAll(paramFiles[p].fileName, getApsimDirectory(), "%apsim%");
      setModuleLine(ini, section, newModuleName, paramFiles);
      }
   return modsMade;
   }

// ------------------------------------------------------------------
// Delete the specified module
// ------------------------------------------------------------------
bool ApsimControlFile::deleteModule(const std::string& section,
                                    const std::string& moduleName)
   {
   // Get a complete list of all par/ini files for specified module.
   vector<ParamFile> paramFiles;
   getParameterFilesForModule(ini, section, moduleName, paramFiles, true);
   set<string> parFileNames;
   for (unsigned p = 0; p != paramFiles.size(); p++)
      parFileNames.insert(paramFiles[p].fileName);

   // rename all relevant sections.
   for (set<string>::iterator paramFile = parFileNames.begin();
                              paramFile != parFileNames.end();
                              paramFile++)
      {
      if (paramFile->find(".ini") == string::npos)
         {
         IniFile* par = getParFile(*paramFile);
         vector<string> sectionNames;
         par->readSectionNames(sectionNames);
         for (unsigned s = 0; s != sectionNames.size(); s++)
            {
            StringTokenizer tokenizer(sectionNames[s], ".");
            string firstBit = tokenizer.nextToken();
            string secondBit = tokenizer.nextToken();
            string thirdBit = tokenizer.nextToken();
            if (Str_i_Eq(secondBit, moduleName))
               par->deleteSection(sectionNames[s]);
            }
         }
      }

   bool changeMade = false;

   vector<string> lines;
   ini->read(section, "module", lines);
   vector<string> newLines;


   // loop through all lines in control file looking for a module = oldModuleName
   for(vector<string>::iterator line = lines.begin();
                                line != lines.end();
                                line++)
      {
      bool keepModuleLine = true;
      unsigned posStartModuleName =line->find_first_not_of(' ');
      if (posStartModuleName != string::npos)
         {
         unsigned posEndModuleName = line->find(' ', posStartModuleName);
         if (posEndModuleName == string::npos)
            posEndModuleName = line->length();
         if (posEndModuleName != string::npos)
            {
            int moduleNameLength = posEndModuleName - posStartModuleName;
            if (Str_i_Eq(line->substr(posStartModuleName, moduleNameLength),
                         moduleName))
               {
               keepModuleLine = false;
               changeMade = true;
               }
            }
         }
      if (keepModuleLine)
         newLines.push_back(*line);
      }
   if (changeMade)
      ini->write(section, "module", newLines);
   return changeMade;
   }
// ------------------------------------------------------------------
// Perform a Search and Replace on the sections of the specified module.
// ------------------------------------------------------------------
bool ApsimControlFile::searchReplace(const std::string& section,
                                     const std::string& moduleName,
                                     const std::string& stringToFind,
                                     const std::string& replacementString)
   {
   bool replacementMade = false;
   vector<ParamFile> paramFiles;
   getParameterFilesForModule(ini, section, moduleName, paramFiles, false);
   for (unsigned p = 0; p != paramFiles.size(); p++)
      {
      IniFile* par = getParFile(paramFiles[p].fileName);
      vector<string> paramFileSections;
      getParFileSectionsMatching(par, paramFiles[p], paramFileSections);

      for (unsigned s = 0; s != paramFileSections.size(); s++)
         {
         string contents;
         par->readSection(paramFileSections[s], contents);

         ostringstream out;
         ostream_iterator<char, char> outI(out);
         boost::regex e(stringToFind);
         regex_merge(outI, contents.begin(), contents.end(), e, replacementString, boost::match_any);
         if (out.str() != contents)
            {
            par->writeSection(paramFileSections[s], out.str());
            replacementMade = true;
            }
         }
      }
   return replacementMade;
   }
// ------------------------------------------------------------------
// Perform a Search and Replace on a control file section
// ------------------------------------------------------------------
bool ApsimControlFile::searchReplaceCon(const std::vector<std::string>& stringsToFind,
                                        const std::vector<std::string>& replacementStrings)
   {
   bool replacementMade = false;

   ifstream in(ini->getFileName().c_str());
   ostringstream s;
   s << in.rdbuf();
   string contents = s.str();
   in.close();

   string outContents = contents;
   for (unsigned i = 0; i != stringsToFind.size(); i++)
      {
      boost::regex e(stringsToFind[i]);
      outContents = regex_merge(outContents, e, replacementStrings[i], boost::match_any | boost::format_all);
      }

   if (outContents != contents)
      {
      ofstream outFile(ini->getFileName().c_str());
      outFile << outContents;
      outFile.close();
      ini->refresh();
      replacementMade = true;
      }

   return replacementMade;
   }

// ------------------------------------------------------------------
// Enumerate all matching manager action lines and call a callback
// for each one. The callee can then change the parameters if
// they so wish. Return's true if parameters were modified.
// ------------------------------------------------------------------
bool ApsimControlFile::enumerateManagerActionLines
                                (const std::string& section,
                                 const std::string& managerAction,
                                 boost::function2<void, ManagerActionParameters& , bool& > callback)
   {
   bool someWereModified = false;

   vector<string> actionValues;
   splitIntoValues(managerAction, " ", actionValues);
   if (actionValues.size() != 2)
      throw runtime_error("Bad manager action: " + managerAction);

   vector<ParamFile> paramFiles;
   getParameterFilesForModule(ini, section, "manager", paramFiles, false);
   for (unsigned p = 0; p != paramFiles.size(); p++)
      {
      IniFile* par = getParFile(paramFiles[p].fileName);
      vector<string> paramFileSections;
      getParFileSectionsMatching(par, paramFiles[p], paramFileSections);
      for (unsigned s = 0; s != paramFileSections.size(); s++)
         {
         bool paramSectionModified = false;
         string contents;
         par->readSection(paramFileSections[s], contents);

         // we want everything to be case-insensitive so drop everything to
         // lowercase.
         string lowerContents = contents;
         To_lower(lowerContents);
         To_lower(actionValues[0]);
         To_lower(actionValues[1]);

         // go find all occurrances of action lines and call callback for each.
         boost::regex e(actionValues[0] + "[[:space:]]+" + actionValues[1]);
         boost::match_results<std::string::const_iterator> what;
#if BOOST_VERSION > 103000
         boost::regex_constants::_match_flags flags = boost::match_default;
#else 
         unsigned int flags = boost::match_default;
#endif

         string::const_iterator startPos = lowerContents.begin();
         string::const_iterator endPos = lowerContents.end();
         while(regex_search(startPos, endPos, what, e, flags))
            {
            unsigned start = what[0].second - lowerContents.begin();
            unsigned i = start;
            while (contents[i] != '\n' && contents[i] != '\0')
               i++;
            unsigned end = i;
            string line = contents.substr(start, end-start);

            // parse manager line.
            ManagerActionParameters parameters;

            vector<string> parameterStrings;
            splitIntoValues(line, ",", parameterStrings);
            for (unsigned p = 0; p != parameterStrings.size(); p++)
               {
               StringTokenizer tokenizer(parameterStrings[p], "=");
               ManagerActionParameter parameter;
               parameter.name = tokenizer.nextToken();
               parameter.value = tokenizer.nextToken();
               stripLeadingTrailing(parameter.name, " ");
               stripLeadingTrailing(parameter.value, " ");
               if (parameter.name == "" || parameter.value == "")
                  throw runtime_error("Bad format for manager action line: " + line);
               parameter.units = splitOffBracketedValue(parameter.value, '(', ')');
               parameters.push_back(parameter);
               }

            // call callback to allow callee to modify parameters.
            bool didModify = false;
            callback(parameters, didModify);
            if (didModify)
               {
               line = " ";
               for (unsigned p = 0; p != parameters.size(); p++)
                  {
                  if (p != 0)
                     line += ", ";
                  line += parameters[p].name + "=" + parameters[p].value
                        + parameters[p].units;
                  }

               contents.replace(start, end-start, line);
               lowerContents.replace(start, end-start, line);
               paramSectionModified = true;
               }
            // Reset the start and end position for next iteration.
            // Remember that lowercontents has changed so iterators are
            // invalidated.
            startPos = lowerContents.begin() + start;
            endPos = lowerContents.end();
            }
         if (paramSectionModified)
            {
            par->writeSection(paramFileSections[s], contents);
            someWereModified = true;
            }
         }
      }

   return someWereModified;
   }
// ------------------------------------------------------------------
// Rename all standard .ini files to .xml
// ------------------------------------------------------------------
bool ApsimControlFile::iniToXml(const std::string& section)
   {
   bool found = false;

   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);
   for (unsigned i = 0; i != moduleLines.size(); i++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(ini->getFileName(), moduleLines[i], paramFiles, true);
      for (unsigned p = 0; p != paramFiles.size(); p++)
         {
         string fileDirectory = fileDirName(paramFiles[p].fileName);
         replaceAll(fileDirectory, "/", "\\");
         if (fileExtensionEquals(paramFiles[p].fileName, "ini") &&
             stristr(fileDirectory.c_str(), getApsimDirectory().c_str()) != NULL)
            {
            paramFiles[p].fileName = "%apsuite/apsim/" + paramFiles[p].instanceName
                                   + "/" + paramFiles[p].instanceName + ".xml";
            moduleLines[i] = createModuleLine(paramFiles);
            found = true;
            }
         }
      }
   if (found)
      {
      // Turn all slashes around and try to put %apsuite into the lines.
      string ApsuiteDir = getApsimDirectory();
      replaceAll(ApsuiteDir, "\\", "/");
      for (unsigned m = 0; m != moduleLines.size(); m++)
         {
         replaceAll(moduleLines[m], "\\", "/");
         replaceAll(moduleLines[m], ApsuiteDir, "%apsuite");
         ini->write(section, "module", moduleLines);
         }

      }
   return found;
   }

