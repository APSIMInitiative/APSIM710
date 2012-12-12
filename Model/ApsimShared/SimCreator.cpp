//---------------------------------------------------------------------------
#include <../General/pch.h>
#pragma hdrstop

#include "SimCreator.h"
#include <fstream>
#include <sstream>
#include <General/path.h>
#include <General/StringTokenizer.h>
#include <General/stl_functions.h>
#include <General/platform.h>
#include "ApsimDirectories.h"
#include "ApsimSettings.h"
#include "ApsimVersion.h"
#ifdef __WIN32__
   #include <direct.h>
#endif
#pragma package(smart_init)


// ------------------------------------------------------------------
// This class implements a component compare method based on the
// order of components listed in the component.ordering file.
// This class is used to sort modules before writing the .sim file.
// ------------------------------------------------------------------
class ComponentOrder
   {
   public:

      // ------------------------------------------------------------------
      // constructor - read in component order.
      // ------------------------------------------------------------------
      ComponentOrder(void)
         {
         ApsimSettings settings;
         settings.getComponentOrder(components);
         }
      // ------------------------------------------------------------------
      // Compare method used by sort.  Returns true if arg1 < arg2.
      // ------------------------------------------------------------------
      bool operator() (const ApsimControlFile::ModuleInstance& arg1,
                       const ApsimControlFile::ModuleInstance& arg2)
         {
         if (arg1.moduleName == arg2.moduleName)
            return (arg1.instanceName < arg2.instanceName);
         for (unsigned i = 0; i != components.size(); i++)
            {
            if (Str_i_Eq(components[i], arg1.moduleName))
               return true;
            if (Str_i_Eq(components[i], arg2.moduleName))
               return false;
            }
         return true; // neither are in list!!
         }

   private:
      vector<string> components;
   };

// -------------------------------------------
// Convert specified control file to a series
// of sim files.
// -------------------------------------------
SimCreator::~SimCreator()
   {
   for (unsigned i = 0; i != convertedParFiles.size(); i++)
      delete convertedParFiles[i];
   }
// -------------------------------------------
// Convert specified control file to a series
// of sim files.
// -------------------------------------------
void SimCreator::ConToSim(const std::string& controlFileName)
   {
   vector<string> emptySectionList;
   ConToSimInternal(controlFileName, emptySectionList);
   }

// -------------------------------------------
// Convert specified control file to a series
// of sim files.
// -------------------------------------------
void SimCreator::ConToSim(const std::string& controlFileName,
                          vector<string>& sectionNames)
   {
   ConToSimInternal(controlFileName, sectionNames);
   }

// -------------------------------------------
// Convert specified control file to a series
// of sim files.
// -------------------------------------------
void SimCreator::ConToSim(const std::string& controlFileName,
                          const std::string& sectionName)
   {
   vector<string> sectionNames;
   sectionNames.push_back(sectionName);
   ConToSimInternal(controlFileName, sectionNames);
   }

// -------------------------------------------
// Convert specified control file to a series
// of sim files.
// -------------------------------------------
void SimCreator::ConToSimInternal(const std::string& controlFileName,
                                  const std::vector<std::string>& conSections)
   {
   // change dir so relative paths work OK
   chdir(fileDirName(controlFileName).c_str());

   ApsimControlFile con(controlFileName);

   vector<string> sectionNames = conSections;
   if (sectionNames.size() == 0)
      con.getAllSectionNames(sectionNames);
   for (unsigned s = 0; s != sectionNames.size(); s++)
      {
      string simFileName = fileRoot(controlFileName) + "." + sectionNames[s] + ".sim";
      ofstream out(simFileName.c_str());

      out << "<?xml version=\"1.0\"?>\n";
#ifdef __WIN32__
      out << "<simulation executable=\"%apsim%/Model/ProtocolManager.dll\" version=\"" << getApsimVersion() <<  "\">\n";
#else
      out << "<simulation executable=\"%apsim%/Model/ProtocolManager.so\" version=\"" << getApsimVersion() <<  "\">\n";
#endif
      out << "   <title>";
      string Title = con.getTitle(sectionNames[s]);
      stripLeadingTrailing(Title, "\r");
      bool UseCDATA = (Title.find_first_of("<>&") != string::npos);
      if (UseCDATA)
         out << "<![CDATA[";
      out << Title;
      if (UseCDATA)
         out << "]]>";
      out << "</title>\n";

      vector<ApsimControlFile::ModuleInstance> moduleInstances;
      con.getAllModuleInstances(sectionNames[s], moduleInstances);
      stable_sort(moduleInstances.begin(), moduleInstances.end(), ComponentOrder());

      for (unsigned m = 0; m != moduleInstances.size(); m++)
         ConvertConModule(Title, moduleInstances[m], out);

      out << "</simulation>\n";
      cerr << "Written " << simFileName.c_str() << endl;
      }
   }

// -------------------------------------------
// Create a bit of .sim file (.xml format) for
// the specified module instance and output it
// to the specified output stream.
// -------------------------------------------
void SimCreator::ConvertConModule(std::string RunTitle,
                                  ApsimControlFile::ModuleInstance& moduleInstance,
                                  ostream& out)
   {
   out << "   <component name=\"" << moduleInstance.instanceName << "\"";
   out << " executable = \"" << moduleInstance.dllFileName << "\">\n";
   ApsimSettings settings;
   string st;
   settings.read("DotNetcomponents|" + moduleInstance.moduleName, st);
   out << "      <initdata>\n";

   if (Str_i_Eq(moduleInstance.moduleName, "input") ||
       Str_i_Eq(moduleInstance.moduleName, "patchinput") ||
       Str_i_Eq(moduleInstance.moduleName, "soi"))
      {
      if (moduleInstance.ParFiles.size() != 1)
         throw runtime_error("An input file must have a single filename. Instance name: " + moduleInstance.instanceName);
      if (newFormat)
         {
         string fileName = moduleInstance.ParFiles[0].first;
         ApsimSettings::addMacro(fileName);
         out << "         <filename input=\"yes\">" << fileName << "</filename>\n";
         }
      else
         {
         out << "         <parameters>\n";
         out << "            <property name=\"filename\">" << moduleInstance.ParFiles[0].first << "</property>\n";
         out << "         </parameters>\n";
         }
      }
   else
      {
      if (Str_i_Eq(moduleInstance.moduleName, "report")) 
	     {
		 out << "         <title>" << RunTitle << "</title>\n";
	     }
      std::vector<SimCreatorSection*> sectionsToOutput;
      
      bool IsDotNetModule = (stristr(moduleInstance.dllFileName.c_str(), "SoilWater.dll" ) ||
                             stristr(moduleInstance.dllFileName.c_str(), "SoilNitrogen.dll" ));      
      if (IsDotNetModule)
          out << "         <" + moduleInstance.moduleName + ">\n";
                             
      for (unsigned p = 0; p != moduleInstance.ParFiles.size(); p++)
         {
         string file = moduleInstance.ParFiles[p].first;
         if (stristr(file.c_str(), ".xml") != NULL)
            {
            ApsimSettings::addMacro(file);
           
            out << "         <include>" + file;
            if (!Str_i_Eq(moduleInstance.ParFiles[p].second, "standard"))
               out << '(' << moduleInstance.ParFiles[p].second << ')';
            out << "</include>\n";
            }
         else
            GetMatchingParFileSections(moduleInstance.instanceName,
                                       file,
                                       moduleInstance.ParFiles[p].second,
                                       sectionsToOutput);
         }

         if (newFormat)
         writeNewFormat(sectionsToOutput, out);
      else
         writeOldFormat(sectionsToOutput, out);

      if (IsDotNetModule)
          out << "         </" + moduleInstance.moduleName + ">\n";
         
      for (unsigned i = 0; i != sectionsToOutput.size(); i++)
         delete sectionsToOutput[i];
      }

   out << "      </initdata>\n";
   out << "   </component>\n";
   }

// --------------------------------------------
// for the specified instance name, return the
// matching sections in the specified file.
// --------------------------------------------
void SimCreator::GetMatchingParFileSections(const std::string& instanceName,
                                             const std::string& file,
                                             const std::string& sectionName,
                                             std::vector<SimCreatorSection*>& outputSections)
   {
   if (file != "")
      {
      string fileName = file;
      if (!fileExists(fileName))
         {
         // Hmmmmm. Not sure if this is needed..???
         char *cwd = getcwd(NULL, 1024);
         fileName = string(cwd) + "/" + file;
         free(cwd);
         }
      vector<ParFile*>::iterator i = find_if(convertedParFiles.begin(), convertedParFiles.end(),
                                            PEqualToFileName<ParFile>(fileName));
      if (i == convertedParFiles.end())
         {
         convertedParFiles.push_back(ConvertParFile(fileName));
         i = find_if(convertedParFiles.begin(), convertedParFiles.end(),
                     PEqualToFileName<ParFile>(fileName));
         }

      // Now try and match the section name passed in. For all matching sections
      // write section contents to 'out'
      for (unsigned s = 0; s != (*i)->sections.size(); s++)
         {
         SimCreatorSection* section = (*i)->sections[s];
         if (sectionIsAMatch(instanceName, sectionName, section->name))
            {
            bool done = false;
            for (unsigned j = 0; j != outputSections.size(); j++)
               if (outputSections[j]->openTag == section->openTag)
                  {
                  outputSections[j]->append(section);
                  done = true;
                  }
            if (!done)
               outputSections.push_back(section->clone());
            }
         }
      }
   }

// ---------------------------------------------
// Return true if the 2 section names match
// ---------------------------------------------
bool SimCreator::sectionIsAMatch(const std::string& instanceName, const std::string& conSectionName, const std::string& parSectionName)
   {
   if (parSectionName.find('.') == string::npos)
      return Str_i_Eq(parSectionName, conSectionName);
   else
      return Str_i_Eq(parSectionName, conSectionName + "." + instanceName);
   }

// --------------------------------------------
// Convert a whole par file to XML storing an
// entry in 'convertedparfiles' vector.
// --------------------------------------------
SimCreator::ParFile* SimCreator::ConvertParFile(const std::string& fileName)
   {
   ParFile* currentParFile = new ParFile(fileName);
   SimCreatorSection* currentSection = NULL;
   if (!fileExists(fileName))
      return currentParFile;

   try
      {
      ifstream in(fileName.c_str());
      string line;
      do
         {
         getline(in, line);
         // Don't scrub comments from manager sections.
         if (currentSection != NULL && ! currentSection->isManagerSection)
           {
           size_t posComment = line.find('!');
           if (posComment != string::npos)
              line.erase(posComment);
           }
         replaceAll(line, "\t", "   ");
         stripLeadingTrailing(line, "\r");
         string sectionName = getSectionName(line);

         // Save the current section we've been accumulating if necessary.
         if ( (!in || sectionName != "") && currentSection != NULL && !currentSection->isEmpty())
            currentParFile->sections.push_back(currentSection);
         else if (!in)
            delete currentSection;

         // Are we starting a new section? If so then create XML openning tags.
         if (in)
            {
            if (sectionName != "")
               {
               if (newFormat)
                  currentSection = new SimCreatorSectionNew;
               else
                  currentSection = new SimCreatorSectionOld;

               StringTokenizer tokenizer(sectionName, ".");
               string firstBit = tokenizer.nextToken();
               string secondBit = tokenizer.nextToken();
               string thirdBit = tokenizer.nextToken();
               currentSection->open(firstBit, secondBit, thirdBit);
               }

            // Are we in the middle of a section. If so then accumulate xml for section.
            else if (currentSection != NULL)
               currentSection->convertLine(line);
            }
         }
      while (in);
      return currentParFile;
      }
   catch (...)
      {
      delete currentSection;
      delete currentParFile;
      throw;
      }
   }

// ---------------------------------------------------
// convert a .ini file to a bit of sim xml and return.
// called by apsim.exe
// ---------------------------------------------------
std::string SimCreator::convertIniToSim(const std::string& includeFileName)
   {
   ParFile* par = ConvertParFile(includeFileName);
   ostringstream out;
   for (unsigned i = 0; i != par->sections.size(); i++)
      par->sections[i]->writeToOut(out);
   string returnString = out.str();
   delete par;
   return returnString;
   }

//---------------------------------------------------------------------------
// Treat the file passed in as an .ini file and convert it
// to a sim file format. Return the converted contents as a string.
// Called by GetComponentDescription (a .NET executable)
//---------------------------------------------------------------------------
extern "C" void EXPORT STDCALL convertIniToSim(const char* fileName, char* contents)
   {
   SimCreator simCreator(true);
   string xml = simCreator.convertIniToSim(fileName);
   strcpy(contents, xml.c_str());
   }

// ---------------------------------------------------
// write the specified sections to the specified
// output stream in OLD format
// ---------------------------------------------------
void SimCreator::writeOldFormat(vector<SimCreatorSection*>& sectionsToOutput,
                                ostream& out)
   {
   bool writingRules = false;
   for (unsigned s = 0; s != sectionsToOutput.size(); s++)
      {
      if (sectionsToOutput[s]->isManagerSection && !writingRules)
         {
         writingRules = true;
         out << "         <rules>\n";
         }
      else if (!sectionsToOutput[s]->isManagerSection && writingRules)
         {
         writingRules = false;
         out << "         </rules>\n";
         }
      sectionsToOutput[s]->writeToOut(out);
      }
   if (writingRules)
      out << "         </rules>\n";
   }

// ---------------------------------------------------
// write the specified sections to the specified
// output stream in NEW format
// ---------------------------------------------------
void SimCreator::writeNewFormat(vector<SimCreatorSection*>& sectionsToOutput,
                                ostream& out)
   {
   for (unsigned s = 0; s != sectionsToOutput.size(); s++)
      sectionsToOutput[s]->writeToOut(out);
   }

