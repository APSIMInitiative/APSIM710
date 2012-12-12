//---------------------------------------------------------------------------
#include <../General/pch.h>
#pragma hdrstop

#include "SimCreatorSection.h"
#include <General/string_functions.h>
#include <General/stristr.h>

#pragma package(smart_init)

#include <stdexcept>
using namespace std;

// ------------------------------------------------------------------
// Append the specified section to this one.
// ------------------------------------------------------------------
void SimCreatorSection::append(const SimCreatorSection* section)
   {
   xml += section->xml;
   // check to make sure variable can be added to this section
   for (set<string>::const_iterator i = section->variableNames.begin();
                              i != section->variableNames.end();
                              i++)
      if (variableNames.find(*i) != variableNames.end())
         {
         throw runtime_error("The variable '" + *i + "' exists in multiple sections:  "
            + "'" + name + "' and '" + section->name + "'");
         }
   }

// ------------------------------------------------------------------
// Append the specified section to this one.
// ------------------------------------------------------------------
void SimCreatorSectionOld::append(const SimCreatorSection* section)
   {
   SimCreatorSection::append(section);
   const SimCreatorSectionOld* rhs = dynamic_cast<const SimCreatorSectionOld*>(section);
   if (rhs != NULL)
      variablesXml += rhs->variablesXml;
   }

// ------------------------------------------------------------------
// Open the ParFile given the 1st, 2nd and 3rd bits of an APSIM
// parameter file section e.g. sample.clock.parameters.
// ------------------------------------------------------------------
void SimCreatorSectionOld::open(const std::string& firstBit, const std::string& secondBit,
                                const std::string& thirdBit)
   {
   name = firstBit + "." + secondBit;
   openTag = thirdBit;
   closeTag = thirdBit;

   // If the xml tag begins with a number then we need to put an underscore
   // in front of it.
   char *endptr;
   strtod(openTag.c_str(), &endptr);
   if (endptr != openTag.c_str())
      {
      openTag = "_" + openTag;
      closeTag = openTag;
      }

   replaceAll(name, " ", "");
   isManagerSection = (!Str_i_Eq(secondBit, "swim2") &&
                          (
                          Str_i_Eq(openTag, "start_of_day") ||
                          Str_i_Eq(openTag, "end_of_day") ||
                          Str_i_Eq(openTag, "init") ||
                          Str_i_Eq(openTag, "prepare") ||
                          Str_i_Eq(openTag, "process") ||
                          Str_i_Eq(openTag, "post") ||
                          Str_i_Eq(openTag, "prenewmet") ||
                          Str_i_Eq(secondBit, "operations") ||
                          Str_i_Eq(secondBit, "manager")||
                          Str_i_Eq(secondBit, "tcllink")
                          ));
   if (isManagerSection)
      {
      string ruleName = firstBit + "." + thirdBit;
      openTag = "rule name=\"" + ruleName + "\" condition=\"" + openTag + "\"";
      closeTag = "rule";
      }
   xml = "";
   variablesXml = "";
   variableNames.erase(variableNames.begin(), variableNames.end());
   }

// ------------------------------------------------------------------
// Write contents of this section to 'out' in xml
// ------------------------------------------------------------------
void SimCreatorSectionOld::writeToOut(ostream& out)
   {
   if (isManagerSection)
	  {
      out << "            <" + openTag + ">\n";
      out << "               <![CDATA[\n" << xml << "               ]]>\n            ";
      }
   else
      {
      out << "         <" + openTag + ">\n";
      out << xml << "         ";
      }
   out << "</" + closeTag + ">\n";
   if (variablesXml != "")
      {
      out << "         <variables>\n";
      out << variablesXml;
      out << "         </variables>\n";
      }
   }

// ------------------------------------------------------------------
// convert the specified APSIM par file line to xml
// ------------------------------------------------------------------
void SimCreatorSectionOld::convertLine(const std::string& line)
   {
   if (line.find_first_not_of(' ') != string::npos)
      {
      if (isManagerSection)
         xml += "               " + line + "\n";
      else
         {
         string key, value;
         getKeyNameAndValue(line, key, value);
         if (Str_i_Eq(key, "variable"))
            variablesXml += "            <variable>" + value + "</variable>\n";
         else if (key != "")
            {
            xml += "            <property name=\"" + key + "\">" + value + "</property" + ">\n";
            if (variableNames.find(key) != variableNames.end())
               throw runtime_error("The variable '" + key + "' exists more than once in the section '" + name + "'");
            variableNames.insert(key);
            }
         }
      }
   }
// ------------------------------------------------------------------
// Open the ParFile given the 1st, 2nd and 3rd bits of an APSIM
// parameter file section e.g. sample.clock.parameters.
// ------------------------------------------------------------------
void SimCreatorSectionNew::open(const std::string& firstBit, const std::string& secondBit,
                                const std::string& thirdBit)
   {
   if (thirdBit != "")
      {
      name = firstBit + "." + secondBit;
      openTag = thirdBit;
      closeTag = thirdBit;
      }
   else
      {
      name = firstBit;
      if (secondBit != "")
         {
         openTag = secondBit;
         closeTag = secondBit;
         }
      }
   if (Str_i_Eq(openTag, "parameters") || Str_i_Eq(openTag, "constants"))
      {
      openTag = "";
      closeTag = "";
      }

   // If the xml tag begins with a number then we need to put an underscore
   // in front of it.
   char *endptr;
   strtod(openTag.c_str(), &endptr);
   if (endptr != openTag.c_str())
      {
      openTag = "_" + openTag;
      closeTag = openTag;
      }

   replaceAll(name, " ", "");
   isManagerSection = (!Str_i_Eq(secondBit, "swim2") &&
                          (
                          Str_i_Eq(openTag, "start_of_day") ||
                          Str_i_Eq(openTag, "end_of_day") ||
                          Str_i_Eq(openTag, "init") ||
                          Str_i_Eq(openTag, "prepare") ||
                          Str_i_Eq(openTag, "process") ||
                          Str_i_Eq(openTag, "post") ||
                          Str_i_Eq(openTag, "prenewmet") ||
                          Str_i_Eq(secondBit, "operations") ||
                          Str_i_Eq(secondBit, "manager")||
                          Str_i_Eq(secondBit, "tcllink")
						  ));
   if (isManagerSection)
      {
      string ruleName = firstBit + "." + thirdBit;
      eventName = thirdBit;
      openTag = "script name=\"" + ruleName + "\"";
      closeTag = "script";
      }
   xml = "";
   variableNames.erase(variableNames.begin(), variableNames.end());
   }

// ------------------------------------------------------------------
// Write contents of this section to 'out' in xml
// ------------------------------------------------------------------
void SimCreatorSectionNew::writeToOut(ostream& out)
   {
   if (isManagerSection)
      {
      out << "         <" + openTag + ">\n";
      out << "            <text><![CDATA[\n" << xml << "            ]]>\n            </text>\n";
      out << "            <event>" + eventName + "</event>\n";
      }
   else
      {
      if (openTag != "")
         out << "         <" + openTag + ">\n";
      out << xml;
      }
   if (closeTag != "")
      out << "         </" + closeTag + ">\n";
   }

// ------------------------------------------------------------------
// convert the specified APSIM par file line to xml
// ------------------------------------------------------------------
void SimCreatorSectionNew::convertLine(const std::string& line)
   {
   if (!isManagerSection && stristr(line.c_str(), "table ") != NULL && line.find('=') == string::npos)
      {
      inTable = true;
      vector<string> tableWords;
      splitIntoValues(line, " ", tableWords);
      if (tableWords.size() != 2)
         throw runtime_error("Invalid start of table: " + line);
      tableName = "table " + tableWords[1];
      xml += "         <table name=\"" + tableWords[1] + "\">\n";
	  waitingForTableHeader = true;
      tableHeaders.erase(tableHeaders.begin(), tableHeaders.end());
      tableUnits.erase(tableUnits.begin(), tableUnits.end());
      }
   else if (!isManagerSection && waitingForTableHeader)
      {
      if (tableHeaders.size() == 0)
         splitIntoValues(line, " ", tableHeaders);
      else
         {
         splitIntoValues(line, " ", tableUnits);
         if (tableUnits.size() != tableHeaders.size())
            throw runtime_error("Invalid number of units in table: " + tableName);
         waitingForTableHeader = false;
         }
      }
   else if (!isManagerSection && inTable && stristr(line.c_str(), "end table") == NULL)
      {
      vector<string> tableValues;
      splitIntoValues(line, " ", tableValues);
      if (tableValues.size() != tableHeaders.size())
         throw runtime_error("The number of values in '" + tableName + "' doesn't match the number of headings in the table.");
      xml += "            <" + tableHeaders[0] + " name=\"" + tableValues[0] + "\">\n";
      for (unsigned i = 1; i != tableValues.size(); i++)
         xml += "               <" + tableHeaders[i] + " units=\"" + tableUnits[i] + "\">" + tableValues[i] + "</" + tableHeaders[i] + ">\n";
      xml += "            </" + tableHeaders[0] + ">\n";
      }
   else if (!isManagerSection && inTable)
      {
      xml += "         </table>\n";
      inTable = false;
      }
   else if (line.find_first_not_of(' ') != string::npos)
      {
      if (isManagerSection)
         xml += "            " + line + "\n";
      else
         {
         string key, value;
         getKeyNameAndValue(line, key, value);
         if (key != "")
            {
            if (openTag != "")
               xml += "   ";
            xml += "         <" + key + ">" + value + "</" + key + ">\n";

            if (!Str_i_Eq(key, "variable") && !Str_i_Eq(key, "module_names") &&
                !Str_i_Eq(key, "variable_names"))
			   {
               if (variableNames.find(key) != variableNames.end())
                  throw runtime_error("The variable '" + key + "' exists more than once in the section '" + name + "'");
               variableNames.insert(key);
               }
            }
         }
      }
   }

