//---------------------------------------------------------------------------
#include <numeric>
#include <General/StringTokenizer.h>
#include <General/string_functions.h>
#include <General/date_functions.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/TypeConverter.h>
#include <ApsimShared/ApsimRegistry.h>
#include <boost/date_time/gregorian/gregorian.hpp>

#include "TrackerVariable.h"

using namespace std;
using namespace boost::gregorian;

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
TrackerVariable::TrackerVariable(protocol::Component* p, const string& fullName)
  : parent(p)
   {
   count = 0;
   last = 0;
   startPeriodID = endPeriodID = nameID = eventID = 0; 
   startPeriodComponentID = endPeriodComponentID = eventComponentID = 0;
   inWindow = false;
   parse(fullName);
   sampleDate = 0;
   }
// ------------------------------------------------------------------
// Parse the name passed in.
// ------------------------------------------------------------------
void TrackerVariable::parse(const string& fullName)
   {
   StringTokenizer tokenizer(fullName, " ");

   // get stat
   parseStat(tokenizer);

   // make sure next word is 'of'
   if (!Str_i_Eq(tokenizer.nextToken(), "of"))
      throw runtime_error("Expected keyword 'of' in tracker variable: " + fullName);

   // get variable.
   if (stat == countStat || stat == dateStat)
      parseEventName(tokenizer);
   else
      variableName = tokenizer.nextToken();

   // Look for an on followed by an event name.
   if (stat != countStat && stat != dateStat)
      {
      if (!Str_i_Eq(tokenizer.nextToken(), "on"))
         throw runtime_error("Expected keyword 'on' in tracker variable: " + fullName);
      parseEventName(tokenizer);
      }

   string keyword = tokenizer.nextToken();
   if (stat != valueStat && Str_i_Eq(keyword, "from"))
      {
      parsePeriod(tokenizer);
      keyword = tokenizer.nextToken();
      }
   else
      inWindow = true;

   if (Str_i_Eq(keyword, "as"))
      parseAs(tokenizer);
   else 
      name = keyword;
   }
// ------------------------------------------------------------------
// Parse a 'stat'
// ------------------------------------------------------------------
void TrackerVariable::parseStat(StringTokenizer& tokenizer)
   {
   string statName = tokenizer.nextToken();
   if (Str_i_Eq(statName, "sum"))
      stat = sumStat;
   else if (Str_i_Eq(statName, "average"))
      stat = averageStat;
   else if (Str_i_Eq(statName, "minimum"))
      stat = minimumStat;
   else if (Str_i_Eq(statName, "maximum"))
      stat = maximumStat;
   else if (Str_i_Eq(statName, "count"))
      stat = countStat;
   else if (Str_i_Eq(statName, "date"))
      stat = dateStat;
   else if (Str_i_Eq(statName, "value"))
      stat = valueStat;
   else
      throw runtime_error("Invalid stat name: " + statName);
   }
// ------------------------------------------------------------------
// Parse a 'from' section
// ------------------------------------------------------------------
void TrackerVariable::parsePeriod(StringTokenizer& tokenizer)
   {
   string period = tokenizer.nextToken();

   ApsimRegistry::getApsimRegistry().unCrackPath(parent->getId(),
                                                    period, 
                                                    startPeriodComponentID, 
                                                    startPeriod);

   if (Str_i_Eq(startPeriod, "reported"))
      inWindow = true;

   if (startPeriod == "" || Str_i_Eq(startPeriod, "to"))
      throw runtime_error("Expected a start of period after a 'from' keyword in tracker variable");

   if (!Str_i_Eq(tokenizer.nextToken(), "to"))
      throw runtime_error("Expected a 'to' keyword in tracker variable.");

   period = tokenizer.nextToken();

   ApsimRegistry::getApsimRegistry().unCrackPath(parent->getId(),
                                                    period, 
                                                    endPeriodComponentID, 
                                                    endPeriod);

   if (endPeriod == "")
      throw runtime_error("Expected an end of period after a 'to' keyword in tracker variable");
   if (Str_i_Eq(endPeriod, "now"))
      endPeriod = "";
   }
// ------------------------------------------------------------------
// Parse a 'last' section
// ------------------------------------------------------------------
void TrackerVariable::parseLast(StringTokenizer& tokenizer)
   {
   string number = tokenizer.nextToken();
   if (!Is_numerical(number.c_str()))
      throw runtime_error("Expected a number following a 'last' keyword.");
   last = atoi(number.c_str());
   }
// ------------------------------------------------------------------
// Parse an 'as' section
// ------------------------------------------------------------------
void TrackerVariable::parseAs(StringTokenizer& tokenizer)
   {
   name = tokenizer.nextToken();
   if (name == "")
      throw runtime_error("Expected a name following an 'as' keyword in tracker variable.");
   }
// ------------------------------------------------------------------
// Parse an 'on' section
// ------------------------------------------------------------------
void TrackerVariable::parseEventName(StringTokenizer& tokenizer)
   {
   string evt = tokenizer.nextToken();
   if (Str_i_Eq(evt, "last"))
      {
      if (stat == countStat || stat == dateStat)
         throw runtime_error("A 'last' keyword cannot be used with a 'count' or a 'date' "
                             "keyword in a tracker variable");
      parseLast(tokenizer);
      evt = tokenizer.nextToken();
      }

   ApsimRegistry::getApsimRegistry().unCrackPath(parent->getId(),
                                                    evt, 
                                                    eventComponentID, 
                                                    eventName);

   if (eventName == "")
      throw runtime_error("Expected an event name.");
   else if (Str_i_Eq(eventName, "start_of_day"))
      eventName = "prepare";
   else if (Str_i_Eq(eventName, "end_of_day"))
      eventName = "post";
   }
// ------------------------------------------------------------------
// Do all necessary registrations.
// ------------------------------------------------------------------
void TrackerVariable::doRegistrations(void)
   {
   static const char* nullDDML = "<type/>";
   static const char* stringDDML = "<type kind=\"string\"/>";
   static const char* doubleDDML = "<type kind=\"double\"/>";
   static const char* singleArrayDDML = "<type kind=\"single\" array=\"T\"/>";
   string typeString = singleArrayDDML;

   eventID = parent->addRegistration(::respondToEvent,
                                     eventComponentID,
                                     eventName,
                                     nullDDML); 

   if (variableName != "")
      {
      // Find the registration entry of the module that sends this data to us.

      // Firstly, convert the name into a id/name pair.
      ApsimRegistry::getApsimRegistry().unCrackPath(parent->getId(),
                                                    variableName, 
                                                    ownerModuleID, 
                                                    ownerModuleName);

      ApsimRegistration* reg = (ApsimRegistration*)
                                  parent->addRegistration(::get,
                                                          ownerModuleID,
                                                          ownerModuleName,
                                                          singleArrayDDML);
      
      // Do a "get" to tickle the system into probing for the variable. Discard results.
      protocol::Variant *variant = NULL;
	  reg->setName(ownerModuleName);
      parent->getVariable((unsigned int)reg, &variant, true);
      
      // Now find the sending modules registration for that variable, and 
      //  extract units
      typeString = singleArrayDDML;
      vector<ApsimRegistration *> subscriptions;
      ApsimRegistry::getApsimRegistry().lookup(reg, subscriptions);
      if (subscriptions.size() > 0) 
         {
         string ddml = subscriptions[0]->getDDML();
         string unitsEq = "unit=";
         unsigned posAttr = ddml.find(unitsEq);
         if (posAttr != string::npos)
            {
            posAttr += unitsEq.size();
            string attrPlusRemainder = ddml.substr(1+posAttr);
            unsigned posEndQuote = attrPlusRemainder.find("\"");
            string units = attrPlusRemainder.substr(0, posEndQuote);
            typeString = "<type kind=\"single\" unit=\"" + units + "\" array=\"T\"/>";
            }
         }
      }

   if (startPeriod != "")
      startPeriodID = parent->addRegistration(::respondToEvent,
                                              startPeriodComponentID,
                                              startPeriod,
                                              nullDDML);
   if (endPeriod != "")
      endPeriodID = parent->addRegistration(::respondToEvent,
                                            endPeriodComponentID,
                                            endPeriod,
                                            nullDDML);
   if (stat == dateStat)
      {
      nameID = parent->addRegistration(::respondToGet,
                                       0,
                                       name,
                                       doubleDDML);
      }
   else
      {
      nameID = parent->addRegistration(::respondToGet,
                                       0,
                                       name,
                                       typeString);
      }
   }
// ------------------------------------------------------------------
// Incoming events come through here.
// ------------------------------------------------------------------
void TrackerVariable::respondToEvent(int fromID, unsigned evntID)
   {
   if (evntID == eventID)
      {
      if (eventComponentID <= 0 || fromID == eventComponentID)
         doSample();
      }
   else if (evntID == startPeriodID)
      {
      if (startPeriodComponentID <= 0 || fromID == startPeriodComponentID)
         onStartPeriod();
      }
   else if (evntID == endPeriodID)
      {
      if (endPeriodComponentID <= 0 || fromID == endPeriodComponentID)
         onEndPeriod();
      }
   }
// ------------------------------------------------------------------
// Incoming requests for values of variables come through here.
// ------------------------------------------------------------------
bool TrackerVariable::respondToGet(unsigned int& fromID,
                                   protocol::QueryValueData& queryData)
   {
   if (queryData.ID == nameID)
      {
      if (stat != dateStat)
         {
         vector<float> values;
         getCurrentValues(values);
         parent->sendVariable(queryData, values);
         }
      else
         parent->sendVariable(queryData, sampleDate);
	  return true;
      } 
      return false;
   }
// ------------------------------------------------------------------
// Perform a sample.
// ------------------------------------------------------------------
void TrackerVariable::doSample(void)
   {
   if (stat == countStat)
      {
      if (inWindow)
         count++;
      }
   else if (stat == dateStat)
      {
      double today;
      unsigned todayID = parent->addRegistration(::get,
                                                 0,
                                                 "today",
                                                 protocol::DDML(today).c_str());
      parent->getVariable(todayID, today, -1.0E12, 1.0E12);
      sampleDate = today;
      }
   else if (inWindow)
      {
      vector<float> theseValues;
      unsigned int valueID = parent->addRegistration(::get,
                                                     ownerModuleID,
                                                     ownerModuleName,
                                                     protocol::DDML(theseValues).c_str());

	  // This is a bit of a hack.
	  // We may be looking for an array element, e.g., no3(2)
	  // The registration is actually for the entire array (no3),
	  // so we need to tinker with regItem to make sure it knows
	  // which element we want.
	  ApsimRegistration *regItem = (ApsimRegistration *)valueID;
	  regItem->setName(ownerModuleName);

      bool ok = parent->getVariable(valueID,
                                    theseValues,
                                    -1.0E6,
                                    1.0E6,
                                    true);
 
      if (ok) 
         {
         if (stat == valueStat)
            values.erase(values.begin(), values.end());
         values.push_back(theseValues);
         if (last != 0)
            {
            // make sure there are no more than 'last' values in values vector.
            while (values.size() > last)
               values.erase(values.begin());
            }
         }
      }
   }
// ------------------------------------------------------------------
// we're at the start of the sampling window.
// ------------------------------------------------------------------
void TrackerVariable::onStartPeriod(void)
   {
   values.erase(values.begin(), values.end());
   count = 0;
   inWindow = true;
   }
// ------------------------------------------------------------------
// we're at the end of the sampling window.
// ------------------------------------------------------------------
void TrackerVariable::onEndPeriod(void)
   {
   inWindow = false;
   }
// ------------------------------------------------------------------
// return the current value to caller.
// ------------------------------------------------------------------
void TrackerVariable::getCurrentValues(vector<float>& currentValues)
   {
   currentValues.erase(currentValues.begin(), currentValues.end());
   if (stat == countStat)
      currentValues.push_back(count);
   else if (values.size() > 0)
      {
      for (unsigned i = 0; i != values[0].size(); ++i)
         {
         float value = 0.0;
         for (unsigned v = 0; v != values.size(); ++v)
            {
            switch (stat)
               {
               case sumStat     :
               case averageStat : value += values[v][i]; break;
               case minimumStat : value = min(value, values[v][i]); break;
               case maximumStat : value = max(value, values[v][i]); break;
               case countStat   : value++; break;
               case valueStat   : value = values[v][i]; break;
               }
            }
         if (stat == averageStat && values.size() > 0)
            value /= values.size();
         currentValues.push_back(value);
         }
      }
   }

