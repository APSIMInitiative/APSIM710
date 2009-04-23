//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "SOI.h"
#include "DataContainer.h"
#include "DataProcessor.h"
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\date_functions.h>
#include <generalvcl\db_functions.h>
#include <generalvcl\vcl_functions.h>
#include <general\path.h>
#include <general\xml.h>
#include <ApsimShared\ApsimSettings.h>
#include <ApsimShared\ApsimDirectories.h>

using namespace std;

#define SOI_SECTION "soi"
#define SOI_PHASE_FIELD_NAME "SOI Phase"
#define SOI_PHASE_NUMBER_FIELD_NAME "SOI Phase number"
#define PHASE_NAMES_KEY "PhaseNames"

typedef std::map<std::string, unsigned, std::less<std::string> > Phases;

// ------------------------------------------------------------------
// Look through all the field names and go find a sow_year
// field.
// ------------------------------------------------------------------
string getSowYearFieldName(TDataSet* data)
   {
   int i = data->FieldDefs->IndexOf("sow_year");
   if (i == -1)
      {
      i = data->FieldDefs->IndexOf("year");
      if (i == -1)
         throw runtime_error("Cannot find a sow_year column for SOI analysis");
      }
   return data->FieldDefs->Items[i]->Name.c_str();
   }
// ------------------------------------------------------------------
// Read in all soi data from SOI file.  The file is assumed to be in
// the same directory as this DLL.
// ------------------------------------------------------------------
void readSoiData(const string& soiFilename,
                 vector<string>& phaseNames, Phases& phases)
   {
   phaseNames.erase(phaseNames.begin(), phaseNames.end());
   phases.erase(phases.begin(), phases.end());

   string fileName = soiFilename.c_str();
   replaceAll(fileName, "%apsuite", getApsimDirectory());
   if (!FileExists(fileName.c_str()))
      throw runtime_error("Cannot find soi data file: " + string(soiFilename.c_str()));

   // Read in all soi data.
   ifstream in (fileName.c_str());
   string line;
   static const char* defaultPhaseNamesString
      = "Unknown,Negative,Positive,Falling,Rising,Zero";
   Split_string(defaultPhaseNamesString, "," , phaseNames);

   vector<string> words;
   unsigned year,month,phase;
   while (getline(in, line))
      {
      Split_string(line, " ", words);
      if (words.size() == 4)
         {
         year = StrToInt(words[0].c_str());
         month = StrToInt(words[1].c_str());
         phase = StrToInt(words[3].c_str());
         string yearMonth = AnsiString(IntToStr(year) + "/" + IntToStr(month)).c_str();
         phases.insert(Phases::value_type(yearMonth.c_str(), phase));
         }
      }
   }
// ------------------------------------------------------------------
// Return an soi phase name for the specified
// year and month.  Throws runtime_error if not found.
// ------------------------------------------------------------------
string getPhase(unsigned year, unsigned month, vector<string>& phaseNames, Phases& phases)
   {
   string yearMonth = AnsiString(IntToStr(year) + "/" + IntToStr(month)).c_str();
   Phases::iterator phaseI = phases.find(yearMonth.c_str());
   if (phaseI == phases.end())
      throw runtime_error("Cannot find an SOI phase for year/month: " + yearMonth);

   else
      {
      unsigned phase = phaseI->second;
      if (phase < phaseNames.size())
         return phaseNames[phase];
      else
         {
         ostringstream msg;
         msg << "Invalid phase number: " << phase;
         throw runtime_error(msg.str());
         }
      }
   }
// ------------------------------------------------------------------
// Return true if we should keep the specified phase.
// ------------------------------------------------------------------
bool keepPhase(const string& phaseName, const std::vector<std::string>& phaseNamesToKeep)
   {
   return (find_if(phaseNamesToKeep.begin(), phaseNamesToKeep.end(),
                   CaseInsensitiveStringComparison(phaseName))
          != phaseNamesToKeep.end());
   }



//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void processSOI(DataContainer& parent,
                const XMLNode& properties,
                vector<TDataSet*> sources,
                TDataSet& result)
   {
   if (sources.size() == 1)
      {
      TDataSet* source = sources[0];
      if (!result.Active)
         {
         result.FieldDefs->Assign(source->FieldDefs);
         addDBField(&result, SOI_PHASE_FIELD_NAME, "xxxx");
         result.Active = true;
         }

      string soiFilename = parent.read(properties, "filename");
      string monthString = parent.read(properties, "month");
      unsigned monthToUse = longMonthToInt(monthString);
      vector<string> phaseNamesToKeep = parent.reads(properties, "Phase");
      bool allOtherYears = (find_if(phaseNamesToKeep.begin(), phaseNamesToKeep.end(),
                                    CaseInsensitiveStringComparison("AllOtherYears"))
                       != phaseNamesToKeep.end());
      bool allYears = (find_if(phaseNamesToKeep.begin(), phaseNamesToKeep.end(),
                               CaseInsensitiveStringComparison("AllYears"))
                       != phaseNamesToKeep.end());
      if (soiFilename == "")
         {
         ApsimSettings settings;
         settings.read("soi|soi file", soiFilename, true);
         }

      // read in all soi data from soi file.
      vector<string> phaseNames;
      Phases phases;
      readSoiData(soiFilename, phaseNames, phases);

      // get the sowing year field name
      string sowYearFieldName = getSowYearFieldName(source);

      if (result.FieldDefs->Count > 0)
         {
         result.Active = true;

         // loop through all records.
         source->First();
         while (!source->Eof)
            {
            string currentPhaseName;
            int year = source->FieldValues[sowYearFieldName.c_str()];
            currentPhaseName = getPhase(year, monthToUse, phaseNames, phases);

            bool keep = keepPhase(currentPhaseName, phaseNamesToKeep);
            if (keep || allOtherYears)
               {
               // add a new record that is identical to the current source record.
               copyDBRecord(source, &result);

               result.Edit();
               if (keep)
                  result.FieldValues[SOI_PHASE_FIELD_NAME] = currentPhaseName.c_str();
               else
                  result.FieldValues[SOI_PHASE_FIELD_NAME] = "AllOtherYears";

               result.Post();
               }

            source->Next();
            }

         // Add the AllYears series if necessary.
         if (allYears)
            {
            // loop through all records.
            source->First();
            while (!source->Eof)
               {
               string currentPhaseName;
               int year = source->FieldValues[sowYearFieldName.c_str()];
               currentPhaseName = getPhase(year, monthToUse, phaseNames, phases);

               // add a new record that is identical to the current source record.
               copyDBRecord(source, &result);

               result.Edit();
               result.FieldValues[SOI_PHASE_FIELD_NAME] = "AllYears";
               result.Post();

               source->Next();
               }


            }
         }
      }
   }
