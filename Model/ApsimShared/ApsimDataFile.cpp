#include <map>
#include <vector>
#include <fstream>
#include <string>
#include <General/string_functions.h>
#include <General/stl_functions.h>
#include <General/path.h>
#include <General/IniFile.h>
#include <General/platform.h>
#include <boost/lexical_cast.hpp>
#include <boost/date_time/period.hpp>
#include <boost/date_time/date_parsing.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>

#include "FString.h"
#include "ApsimDataFile.h"


using namespace std;
using namespace boost;
using namespace boost::gregorian;

//---------------------------------------------------------------------------
// read in the contents of the specified file into this TSEGTable.
//---------------------------------------------------------------------------
void ApsimDataFile::open(const std::string& fileName)
   {
   if (!fileExists(fileName))
      throw runtime_error("Cannot open file: " + fileName + ". File doesn't exist");

   if (in.is_open())
      in.close();
   in.open(fileName.c_str(), ios::binary);
   readApsimHeader();
   firstRecordPos = in.tellg();
   readNextRecord();
   haveFoundDate = false;
   }
//---------------------------------------------------------------------------
// close the datafile.
//---------------------------------------------------------------------------
void ApsimDataFile::close(void)
   {
   in.close();
   }
//---------------------------------------------------------------------------
// retrieve the next line from the input stream.
//---------------------------------------------------------------------------
istream& ApsimDataFile::getline(string& line)
   {
   std::getline(in, line);
   if (line.size() > 0)
      {
      if (line[line.size()-1] == '\r')
         line.erase(line.size()-1);
      stripLeadingTrailing(line, " ");
      }   
   return in;
   }
//---------------------------------------------------------------------------
// advance to the next record.
//---------------------------------------------------------------------------
bool ApsimDataFile::next(void)
   {
   return readNextRecord();
   }
//---------------------------------------------------------------------------
// position at first record.
//---------------------------------------------------------------------------
bool ApsimDataFile::first(void)
   {
   in.clear();
   in.seekg(firstRecordPos);
   return readNextRecord();
   }
//---------------------------------------------------------------------------
// advance to the last record.
//---------------------------------------------------------------------------
bool ApsimDataFile::last(void)
   {
   in.seekg(-1, ios::end);
   in.clear();

   // skip over any blank lines.
   while (in && in.peek() == '\n' || in.peek() == '\r')
      in.seekg(-1, ios::cur);

   // skip to previous \n
   while (in && in.peek() != '\n')
      in.seekg(-1, ios::cur);

   // move forward 1 char to get past the \n
   in.seekg(1, ios::cur);
   return readNextRecord();
   }
// ------------------------------------------------------------------
// read in the header part of an apsim output file.  The header part
// consists of a title= line, followed by a line of field names
// and a line of unit names.
// ------------------------------------------------------------------
void ApsimDataFile::readApsimHeader() throw(runtime_error)
   {
   temporalData.erase(temporalData.begin(), temporalData.end());
   constants.erase(constants.begin(), constants.end());
   columnIndexes.erase(columnIndexes.begin(), columnIndexes.end());

   // loop through all lines looking for heading line.
   string line, previousLine;
   bool foundHeadings = false;
   while (!foundHeadings && getline(line))
      {
      if (line.find('=') != string::npos)
         {
         Value value;
         value.comment = splitOffAfterDelimiter(line, "!");
         stripLeadingTrailing(value.comment, " ");

         string valueString;
         getKeyNameAndValue(line, value.name, valueString);
         if (value.name != "")
            {
            stripLeadingTrailing(value.name, " ");
            value.units = "(" + splitOffBracketedValue(valueString, '(', ')') + ")";
            splitIntoValues(valueString, " ", value.values);
            constants.push_back(value);
            }
         }
      else
         {
         splitOffAfterDelimiter(line, "!");

         // If the first non-blank character on the line is a open bracket '('
         // then we have found the units line.  The previous line is then
         // assumed to be the headings line.
         unsigned int posFirstNonBlankChar = line.find_first_not_of (" ");
         if (posFirstNonBlankChar != string::npos &&
             line[posFirstNonBlankChar] == '(')
            {
            foundHeadings = true;

            vector<string> fieldNames, fieldUnits;
            splitIntoValues(line, " ", fieldUnits);
            splitIntoValues(previousLine, " ", fieldNames);

            if (fieldNames.size() != fieldUnits.size())
               throw runtime_error("Each column in an APSIM input data file must "
                                   "have a unit and a heading.\n"
                                   "Filename = " + fileName);

            // check for duplicate fields
            vector<string> temp;
            copy(fieldNames.begin(), fieldNames.end(), back_inserter(temp));
            sort(temp.begin(), temp.end());
            if (unique(temp.begin(), temp.end()) != temp.end())
               throw runtime_error("Column names in an APSIM input file must be"
                                   "unique\n"
                                   "Filename = " + fileName);
            // The problem now is that some headings may contain array
            // indexes so for each column we are going to build up a container
            // of pointers to temporal Value objects. This will make it easier
            // later to append a string value to a value object when we do a
            // getline.
            for (unsigned i = 0; i != fieldNames.size(); i++)
               {
               static unsigned previousArrayIndex = 0;
               string arrayIndexString = splitOffBracketedValue(fieldNames[i], '(', ')');
               bool newValue;
               if (arrayIndexString == "")
                  newValue = true;
               else
                  {
                  unsigned arrayIndex = lexical_cast<unsigned>(arrayIndexString);
                  if (arrayIndex <= 0 || (arrayIndex > 1 && arrayIndex != previousArrayIndex+1))
                     throw runtime_error("Invalid array index: " + arrayIndexString
                                         + " for column: " + fieldNames[i]);
                  newValue = (arrayIndex == 1);
                  previousArrayIndex = arrayIndex;
                  }
               static Value value;
               if (newValue)
                  {
                  value.name = fieldNames[i];
                  value.units = fieldUnits[i];
                  temporalData.push_back(value);
                  previousArrayIndex = 1;
                  }
               columnIndexes.push_back(temporalData.size()-1);
               }
            }
         }
      previousLine = line;
      }
   }
// ------------------------------------------------------------------
// Clear all values from the specified temporal object.
// ------------------------------------------------------------------
void ClearValues(Value& value)
   {
   value.values.erase(value.values.begin(), value.values.end());
   }
// ------------------------------------------------------------------
// Read in the next record.  Return true if values are returned.
// ------------------------------------------------------------------
bool ApsimDataFile::readNextRecord() throw(runtime_error)
   {
   // clear all values from all temporal objects.
   for_each(temporalData.begin(), temporalData.end(), ClearValues);

   string line;
   if (getline(line))
      {
      vector<string> fieldValues;

      // remove any comment characters.
      unsigned posComment = line.find('!');
      if (posComment != string::npos)
         line.erase(posComment);
      if (line.length() == 0)
         endOfFile = true;
      else
         {
         splitIntoValues(line, " ", fieldValues);
         if (fieldValues.size() != columnIndexes.size())
            throw runtime_error("Not enough values on line: " + line + " in file: " + fileName);

         for (unsigned i = 0; i != columnIndexes.size(); i++)
            temporalData[columnIndexes[i]].values.push_back(fieldValues[i]);
         endOfFile = false;
         }
      }
   else
      endOfFile = true;
   return endOfFile;
   }
// ------------------------------------------------------------------
// Look at the columns names to see if we sufficient columns to
// build a date.
// ------------------------------------------------------------------
void ApsimDataFile::lookForDateField(void)
   {
   yearI = temporalData.end();
   monthI = temporalData.end();
   dayI = temporalData.end();
   dateI = temporalData.end();
   domI = temporalData.end();
   for (iterator i = temporalData.begin();
                 i != temporalData.end();
                 i++)
      {
      if (Str_i_Eq(i->name, "year"))
         yearI = i;
      else if (Str_i_Eq(i->name, "month"))
         monthI = i;
      else if (Str_i_Eq(i->name, "day"))
         dayI = i;
      else if (Str_i_Eq(i->name, "date"))
         dateI = i;
      else if (Str_i_Eq(i->name, "dom"))
         domI = i;
      }
   bool ok = (dateI != temporalData.end());
   if (!ok)
      ok = (yearI != temporalData.end() && dayI != temporalData.end());
   if (!ok)
      ok = (yearI != temporalData.end() && domI != temporalData.end() && monthI != temporalData.end());
   if (!ok)
      throw runtime_error("Cannot find date columns in file: " + fileName + "\n"
                          "The file must have one of the following combinations:\n"
                          "   a date column\n"
                          "   a year and day column\n"
                          "   a year, month and dom column");
   }
// ------------------------------------------------------------------
// return the date on the current record.
// ------------------------------------------------------------------
gregorian::date ApsimDataFile::getDate(void)
   {
   if (!haveFoundDate)
      lookForDateField();

   if (dateI != temporalData.end())
      return date(from_string(dateI->values[0]));
   else
      {
      int year = lexical_cast<int>(yearI->values[0]);
      if (dayI != temporalData.end())
         {
         int day  = lexical_cast<int>(dayI->values[0]);
         return date(year, 1, 1) + date_duration(day-1);
         }
      else if (monthI != temporalData.end() && domI != temporalData.end())
         {
         int month  = lexical_cast<int>(monthI->values[0]);
         int dom    = lexical_cast<int>(domI->values[0]);
         return date(year, month, dom);
         }
      return date(pos_infin);
      }
   }
//---------------------------------------------------------------------------
// advance the met file to a specified date. Throws is can't find date.
//---------------------------------------------------------------------------
void ApsimDataFile::gotoDate(date dateToFind)
   {
   while (!eof() && getDate() != dateToFind)
      next();
   if (eof() || getDate() != dateToFind)
      throw runtime_error("Cannot find date " + to_simple_string(dateToFind)
                          + " in file " + fileName);
   }
//---------------------------------------------------------------------------
// return true if at end of file.
//---------------------------------------------------------------------------
bool ApsimDataFile::eof(void)
   {
   return endOfFile;
   }


// ------------------------------------------------------------------
// ------------------------------------------------------------------
extern "C" unsigned EXPORT STDCALL newApsimDataFile
   (const char* filename, unsigned filenameLength)
   {
   string fileName(filename, filenameLength);
   ApsimDataFile* dataFile = new ApsimDataFile;
   dataFile->open(fileName);
   return (unsigned) dataFile;
   }
extern "C" void EXPORT STDCALL deleteApsimDataFile
   (ApsimDataFile** dataFile)
   {
   delete (*dataFile);
   }
extern "C" unsigned EXPORT STDCALL ApsimDataFile_getFieldValue
   (ApsimDataFile** dataFile, unsigned* fieldIndex, char* value, unsigned valueLength)
   {
   try
      {
      ApsimDataFile::iterator i = (*dataFile)->fieldsBegin();
      i += *fieldIndex;
      string valueString = i->values[0];
      FString(value, valueLength, FORString) = valueString.c_str();
      return true;
      }
   catch (...)
      {
      return false;
      }
   }
extern "C" unsigned EXPORT STDCALL ApsimDataFile_next
   (ApsimDataFile** dataFile)
   {
   try
      {
      return (*dataFile)->next();
      }
   catch (const runtime_error& err)
      {
      //::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      return false;
      }
   }

