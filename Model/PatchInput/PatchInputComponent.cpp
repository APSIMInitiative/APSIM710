#include <list>
#include <fstream>
#include <stdexcept>

#include <math.h>

#include <General/stristr.h>
#include <General/string_functions.h>
#include <General/date_class.h>
//#include <General/date_functions.h>

#include <ApsimShared/FStringExt.h>
#include <ApsimShared/ApsimDataFile.h>

#include <ComponentInterface/Component.h>

#include "../Input/StringVariant.h"
#include "../Input/InputComponent.h"
#include "PatchInputComponent.h"

using namespace std;

// ------------------------------------------------------------------
// Return a blank string when requested to indicate that we don't need a wrapper DLL.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }

extern "C" void STDCALL getDescriptionInternal(char* initScript,
                                                 char* description);   
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
   
extern "C" void STDCALL getDescriptionLengthInternal(char* initScript,
                                                 int* length);
// ------------------------------------------------------------------
// Return component description length.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL getDescriptionLength(char* initScript, int* length)
   {
   getDescriptionLengthInternal(initScript, length);
   }

// ------------------------------------------------------------------
// createComponent
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new PatchInputComponent;
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
PatchInputComponent::PatchInputComponent(void)
   : gPatchDate(1, 1, 1800)
   {
   }

// ------------------------------------------------------------------
// initialise the REPORT component.
// ------------------------------------------------------------------
PatchInputComponent::~PatchInputComponent(void)
   {
   }
// ------------------------------------------------------------------
// INIT1 method handler.
// ------------------------------------------------------------------
void PatchInputComponent::doInit1(const protocol::Init1Data& initData)
   {
   InputComponent::doInit1(initData);

   haveReadPatchData = false;
   preNewmetID = addRegistration(::respondToEvent, 0, "preNewmet", DDML(protocol::NewMetType()).c_str());
   ApsimDataFile::iterator i = find(data.constantsBegin(),
                                    data.constantsEnd(),
                                    "patch_all_years");
   patchAllYears = (i != data.constantsEnd() && Str_i_Eq(i->values[0], "true"));

   i = find(data.constantsBegin(), data.constantsEnd(), "patch_variables_long_term");
   if (i != data.constantsEnd())
      patchVariablesLongTerm = i->values;

   unpatchedMaxTID = addRegistration(::respondToGet, 0, "unpatched_maxt", protocol::DDML(newmet.maxt).c_str());
   unpatchedMinTID = addRegistration(::respondToGet, 0, "unpatched_mint", protocol::DDML(newmet.mint).c_str());
   unpatchedRadnID = addRegistration(::respondToGet, 0, "unpatched_radn", protocol::DDML(newmet.radn).c_str());
   unpatchedRainID = addRegistration(::respondToGet, 0, "unpatched_rain", protocol::DDML(newmet.rain).c_str());
   startDateID = addRegistration(::get, 0, "simulation_start_date", "<type kind=\"integer4\"/>");
   endDateID = addRegistration(::get, 0, "simulation_end_date", "<type kind=\"integer4\"/>");
   }
// ------------------------------------------------------------------
// Read all patch dates.
// ------------------------------------------------------------------
void PatchInputComponent::readPatchDates(void)
   {
   if (!data.eof())
      {
      try
         {
         ApsimDataFile::iterator i = find(data.constantsBegin(), data.constantsEnd(), "start_patching_from");
         if (i != data.constantsEnd())
         {
            GDate gPatchDate;
            gPatchDate.Read(i->values[0], "yyyy/m/d");
         }

         currentRecord = 1;
         minYear = data.getDate().Get_year();
         maxYear = minYear;
         while (!data.eof())
            {
                patchDates.insert(make_pair(data.getDate().Get_jday(), currentRecord));
            currentRecord++;
            unsigned int year = data.getDate().Get_year();
            if (year > maxYear) {maxYear = year;}
            data.next();
            }
         data.first();
         currentRecord = 1;
         }
      catch (const std::exception& err)
         {
         error(string(err.what()), true);
         }
      }
   haveReadPatchData = true;
   }
// ------------------------------------------------------------------
// Get matching variables from INPUT for the same dates as specified in our
// patch data file.
// ------------------------------------------------------------------
void PatchInputComponent::getDataFromInput(unsigned int fromID)
   {
   static const char* getDataDDML = "<type kind=\"string\" array=\"T\"/>";
   static const char* returnDataDDML =
      "<type name=\"newmet\" array=\"T\">"
      "   <field name=\"today\" kind=\"double\"/>"
      "   <field name=\"radn\" kind=\"single\"/>"
      "   <field name=\"maxt\" kind=\"single\"/>"
      "   <field name=\"mint\" kind=\"single\"/>"
      "   <field name=\"rain\" kind=\"single\"/>"
      "   <field name=\"vp\" kind=\"single\"/>"
      "</type>";

   if (patchVariablesLongTerm.size() > 0)
      {
      int startDate = 0;
      int endDate = 0;
      getVariable(startDateID, startDate, 0, 10000000, true);
      getVariable(endDateID, endDate, 0, 10000000, true);

      vector<string> dataDates;
      for (PatchDates::iterator i = patchDates.begin();
                                i != patchDates.end();
                                i++)
         {
         bool validPatchDate = (startDate == 0 && endDate == 0);
         if (!validPatchDate && i->first >= startDate && i->first <= endDate)
            validPatchDate = true;
            
         if (validPatchDate)
            {
            GDate d(i->first);
            dataDates.push_back(d.ToString("YYYY-MM-DD"));
            }
         }
      getDataMethodID = addRegistration(::event,
                                        fromID,
                                        "getData",
                                        getDataDDML);
      returnDataMethodID = addRegistration(::respondToEvent, 0, "returnData", returnDataDDML);
      publish(getDataMethodID, dataDates);
      }
   }
// ------------------------------------------------------------------
// Advance the file to todays date.
// NB: The patch data file may run over a year boundary eg. for a
//     summer crop - need to handle this situation.
// Returns the date the file is positioned at.
// ------------------------------------------------------------------
GDate PatchInputComponent::advanceToTodaysPatchData(unsigned int fromID)
   {
   if (!haveReadPatchData)
      {
      readPatchDates();
      getDataFromInput(fromID);
      }
   if (patchDates.size() > 0)
      {
      try
         {
             PatchDates::iterator i = patchDates.find(gTodaysDate.Get_jday());
         if (i == patchDates.end() && patchAllYears)
            {
            for (unsigned tryYear = minYear;
                          tryYear <= maxYear && i == patchDates.end();
                          tryYear++)
                          i = patchDates.find(GDate(gTodaysDate.Get_day(), gTodaysDate.Get_month(), tryYear).Get_jday());
            }
         if (i != patchDates.end())
            {
            // advance the data file to the correct record.
            unsigned recordToGoTo = i->second;
            if (currentRecord > recordToGoTo)
               {
               currentRecord = 1;
               data.first();
               }
            for (unsigned rec = currentRecord; rec != recordToGoTo; rec++)
               {
               currentRecord++;
               data.next();
               }
            return gTodaysDate;
            }
         else
            return GDate(infin);
         }
      catch (const std::exception& err) // probably caused by a leap year exception. ?? WELL WHY NOT TEST FOR IT ??
         {
         }
      }
   return GDate(infin);
   }

// ------------------------------------------------------------------
// return a variable to caller.
// ------------------------------------------------------------------
void PatchInputComponent::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
   {
   if (queryData.ID == unpatchedMaxTID)
      sendVariable(queryData, newmet.maxt);
   else if (queryData.ID == unpatchedMinTID)
      sendVariable(queryData, newmet.mint);
   else if (queryData.ID == unpatchedRadnID)
      sendVariable(queryData, newmet.radn);
   else if (queryData.ID == unpatchedRainID)
      sendVariable(queryData, newmet.rain);
   else
      InputComponent::respondToGet(fromID, queryData);
   }
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void PatchInputComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == preNewmetID)
      {
      variant.unpack(newmet);
      GDate d;                        // There should be a better way for this - FIXME!!
      unsigned int year, month, day; 
      d.Set(newmet.today);
      d.Get_dmy(day, month, year);
      gTodaysDate = GDate(day, month, year);

      gFileDate = advanceToTodaysPatchData(fromID);
      if (gTodaysDate == gFileDate && gTodaysDate >= gPatchDate)
         {
         for (Variables::iterator v = variables.begin();
                                  v != variables.end();
                                  v++)
            {
            StringVariant* var = &(v->second);
            if (stristr(var->getName().c_str(), "day") == NULL &&
                stristr(var->getName().c_str(), "month") == NULL &&
                stristr(var->getName().c_str(), "year") == NULL &&
                stristr(var->getName().c_str(), "allow_sparse_data") == NULL &&
                stristr(var->getName().c_str(), "start_patching_from") == NULL &&
                stristr(var->getName().c_str(), "patch_variables_long_term") == NULL)
               {
               string foreignName = var->getName();

               if (foreignName.find("patch_") != string::npos)
                  {
                   foreignName.erase(0, strlen("patch_"));

                   unsigned variableID = addRegistration(::set,
                                                         fromID,
                                                         foreignName,
                                                         DTsingleString);
                   setVariable(variableID, var->asFloat());
                  }
               }
            }
         if (patchDataByDayNumber.size() > 0)
            setPatchData();
         }
      }
   else if (eventID == returnDataMethodID)
      {
      vector<protocol::NewMetType> data;
      variant.unpack(data);
      for (unsigned i = 0; i != data.size(); i++)
         {
         GDate d(data[i].today);
         unsigned dayNumber = d.Get_day_of_year();
         if (d.Is_leap_year())
            dayNumber--;

         patchDataByDayNumber.insert(make_pair(dayNumber, data[i]));
         patchDataByDate.insert(make_pair(data[i].today, data[i]));
         }
      }
   else if (eventID != tickID)  // stop the tick event going to base class.
      InputComponent::respondToEvent(fromID, eventID, variant);

   }

// ------------------------------------------------------------------
// Do a bunch of setVariables back to INPUT for all patchVariablesLongTerm.
// ------------------------------------------------------------------
void PatchInputComponent::setPatchData()
   {
   unsigned dayNumber = gTodaysDate.Get_day_of_year();
   if (gTodaysDate.Is_leap_year() && dayNumber >= 61)
      dayNumber--;
   PatchData::iterator i;
   bool found;
   if (patchAllYears)
      {
      i = patchDataByDayNumber.find(dayNumber);
      found = (i != patchDataByDayNumber.end());
      }
   else
      {
      i = patchDataByDate.find(gTodaysDate.Get_jday());
      found = (i != patchDataByDate.end());
      }
        
   if (!found)
      {
      string msg = "No patching of data occurred on date: ";
      msg += gTodaysDate.ToString("YYYY-MM-DD");
      writeString(msg.c_str());
      }
   else
      {
      if (find(patchVariablesLongTerm.begin(), patchVariablesLongTerm.end(),
               "maxt") != patchVariablesLongTerm.end())
         {
         unsigned maxtID = addRegistration(::set, 0, "maxt", DTsingleString);
         setVariable(maxtID, i->second.maxt);
         }
      if (find(patchVariablesLongTerm.begin(), patchVariablesLongTerm.end(),
               "mint") != patchVariablesLongTerm.end())
         {
         unsigned mintID = addRegistration(::set, 0, "mint", DTsingleString);
         setVariable(mintID, i->second.mint);
         }
      if (find(patchVariablesLongTerm.begin(), patchVariablesLongTerm.end(),
               "radn") != patchVariablesLongTerm.end())
         {
         unsigned radnID = addRegistration(::set, 0, "radn", DTsingleString);
         setVariable(radnID, i->second.radn);
         }
      if (find(patchVariablesLongTerm.begin(), patchVariablesLongTerm.end(),
               "rain") != patchVariablesLongTerm.end())
         {
         unsigned rainID = addRegistration(::set, 0, "rain", DTsingleString);
         setVariable(rainID, i->second.rain);
         }
      }
   }
