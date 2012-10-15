#include <../General/pch.h>
#include <stdlib.h>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <stdexcept>

#include <General/string_functions.h>
#include <General/stl_functions.h>
#include <General/path.h>

#include <boost/filesystem/operations.hpp>

#include "IniFile.h"

using namespace std;
typedef vector<pair<unsigned, unsigned> > Indexes;
// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
IniFile::IniFile(void)
   {
   haveDoneBackup = false;
   }
// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
IniFile::IniFile(const string& fileName, bool backups)
   : createBackups(backups)
   {
   haveDoneBackup = false;
   setFileName(fileName);
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
IniFile::~IniFile(void)
   {
   }
// ------------------------------------------------------------------
// Set the file name of the ini file. If the filename is relative
// then it is assumed to be relative to the current working directory.
// ------------------------------------------------------------------
void IniFile::setFileName(const string& file)
   {
   fileName = file;
   parse();
   }
// ------------------------------------------------------------------
// Re-read the .ini file.
// ------------------------------------------------------------------
void IniFile::refresh(void)
   {
   parse();
   }
// ------------------------------------------------------------------
// Parse the .ini file to get positions of sections.
// ------------------------------------------------------------------
void IniFile::parse(void)
   {
   sectionNames.erase(sectionNames.begin(), sectionNames.end());
   sectionIndexes.erase(sectionIndexes.begin(), sectionIndexes.end());
   if (!fileExists(fileName)) {throw std::runtime_error("Cannot open " + fileName);}

   ifstream in(fileName.c_str());
   ostringstream s;
   s << in.rdbuf();
   contents = s.str();

   size_t posStartLine = 0;
   size_t posEol = contents.find('\n');
   while (posEol != string::npos)
      {
      size_t posOpen = contents.find_first_not_of (" \t", posStartLine);
      if (posOpen != string::npos && contents[posOpen] == '[')
         {
         size_t posClose = contents.find(']', posOpen);
         if (posClose != string::npos)
            {
            string sectionName = contents.substr(posOpen+1, posClose-posOpen-1);
            stripLeadingTrailing(sectionName, " ");
            sectionNames.push_back(sectionName);
            sectionIndexes.push_back(posOpen);
            }
         posOpen = posClose;
         }
      posEol = contents.find('\n', posOpen);
      posStartLine = posEol + 1;
      }
   }
// ------------------------------------------------------------------
// Get the next line (tabs removed) starting from pos.
// Return true on success and updates pos to reflect the start of
// next line.
// ------------------------------------------------------------------
bool getIniLine(const string& contents, size_t& pos, string& line)
   {
   if (pos == string::npos || pos == contents.length())
      return false;

   size_t posEol = contents.find('\n', pos);
   if (posEol == string::npos)
      {
      line = contents.substr(pos);
      pos = string::npos;
      }
   else
      {
      line = contents.substr(pos, posEol - pos);
      pos = posEol + 1;
      }

   return true;
   }
// ------------------------------------------------------------------
// Strip all comments and tab characters from specified line.
// ------------------------------------------------------------------
void stripComments(std::string& line)
   {
   // remove tabs.
   replaceAll(line, "\t", "   ");

   size_t posComment = line.find_first_of("!");
   if (posComment != string::npos)
      line.erase(posComment);
   }
// ------------------------------------------------------------------
// Find lines in the specified section that match the specified key.
// Return each lines start and end pos in the section.
// Return true if keys were found.
// ------------------------------------------------------------------
bool findLinePosForKeys(const string& contents,
                        const string& key,
                        Indexes& indexes,
                        bool allowMultiple)
   {
   string line;
   size_t startPos = 0;
   size_t endPos = 0;
   while(getIniLine(contents, endPos, line))
      {
      stripComments(line);
      string iniValue = getKeyValue(line, key);
      if (iniValue != "")
         {
         indexes.push_back(make_pair(startPos, endPos-startPos-1));
         if (!allowMultiple)
            return true;
         }
      startPos = endPos;
      }
   return (indexes.size() > 0);
   }
// ------------------------------------------------------------------
// delete the lines from conents as specified in indexes.
// ------------------------------------------------------------------
void deleteLinePos(string& contents, Indexes& indexes)
   {
   // remove all key lines in reverse order.  Make sure the carraige
   // return is also deleted.  It is not included in the
   // indexes[i-1].second variable hence the +1 below.
   for (unsigned i = indexes.size(); i != 0; i--)
      contents.erase(indexes[i-1].first, indexes[i-1].second+1);
   }
// ------------------------------------------------------------------
// Read and return a string from the .ini file.
// ------------------------------------------------------------------
bool IniFile::read(const string& sectionName, const string& key, string& value) const
   {
   vector<string> values;
   if (findMatchingKeys(sectionName, key, values, true))
      {
      value = values[0];
      return true;
      }
   else
      {
      value = "";
      return false;
      }
   }
// ------------------------------------------------------------------
// Read and return a list of strings
// ------------------------------------------------------------------
bool IniFile::read(const string& sectionName, const string& key,
                   vector<string>& values) const
   {
   return findMatchingKeys(sectionName, key, values, true);
   }
// ------------------------------------------------------------------
// Read and return a list of values matching the specified key.
// Returns true if values were found.
// ------------------------------------------------------------------
bool IniFile::findMatchingKeys(const string& sectionName, const string& key,
                               vector<string>& values, bool allowMultiple) const
        {
   string line;
   string sectionContents;
   readSection(sectionName, sectionContents);

   Indexes indexes;
   findLinePosForKeys(sectionContents, key, indexes, allowMultiple);
   for (Indexes::iterator i = indexes.begin();
                          i != indexes.end();
                          i++)
      {
      string line = sectionContents.substr(i->first, i->second);
      stripComments(line);
      string value = getKeyValue(line, key);

      values.push_back(value);
      stripComments(values[values.size()-1]);
      }
   return (values.size() > 0);
   }
// ------------------------------------------------------------------
// Read and return a list of section names.
// ------------------------------------------------------------------
void IniFile::readSectionNames(vector<string>& sections) const
        {
   sections = sectionNames;
   }
// ------------------------------------------------------------------
// Read and return the contents of the specified section.
// ------------------------------------------------------------------
void IniFile::readSection(const string& section, string& contentsString) const
        {
   size_t posStartSection;
   size_t posEndSection;
   if (getSectionPosition(section, posStartSection, posEndSection))
      contentsString = contents.substr(posStartSection, posEndSection - posStartSection + 1);
   }
// ------------------------------------------------------------------
// Return the start and end positions of a section.
// ------------------------------------------------------------------
bool IniFile::getSectionPosition(const string& section,
                                 size_t& posStartSection,
                                 size_t& posEndSection) const
        {
   vector<string>::const_iterator i = find_if(sectionNames.begin(),
                                              sectionNames.end(),
                                              CaseInsensitiveStringComparison(section));
   if (i != sectionNames.end())
      {
      size_t posSectionName = sectionIndexes[i-sectionNames.begin()];
      posStartSection = contents.find('\n', posSectionName);
      if (posStartSection != string::npos)
         {
         posStartSection++;
         i++;
         if (i == sectionNames.end())
            posEndSection = contents.length();
         else
            posEndSection = sectionIndexes[i-sectionNames.begin()] - 1;
         }
      return true;
      }
   return false;
   }
// ------------------------------------------------------------------
// update all section indexes by the specified number after the specified section.
// ------------------------------------------------------------------
void IniFile::updateIndexesAfter(const string& section, unsigned numChars)
   {
   vector<string>::const_iterator i = find(sectionNames.begin(),
                                           sectionNames.end(),
                                           section);
   if (i != sectionNames.end())
      {
      i++;
      while (i != sectionNames.end())
         {
         sectionIndexes[i-sectionNames.begin()] += numChars;
         i++;
         }
      }
   }
// ------------------------------------------------------------------
// Write contents to a section in file.
// ------------------------------------------------------------------
void IniFile::writeSection(const string& section, const string& newContents)
        {
   doBackup();
   size_t posStartSection;
   size_t posEndSection;
   if (getSectionPosition(section, posStartSection, posEndSection))
      {
      // make sure we have a carriage return before the start of the next
      // section.
      if (newContents.length() > 0 && newContents[newContents.length()-1] != '\n')
         posEndSection--;
      size_t numCharsReplaced = posEndSection-posStartSection+1;
      contents.replace(posStartSection, numCharsReplaced, newContents);
      updateIndexesAfter(section, newContents.length() - numCharsReplaced);
      }
   else
      {
      // make sure contents ends with a \n\n
      if (contents.length() > 2)
         {
         char ch1 = contents[contents.length()-1];
         char ch2 = contents[contents.length()-2];
         if (ch1 == '\n')
            {
            if (ch2 != '\n')
               contents += "\n";
            }
         else
            contents += "\n\n";
         }
      sectionNames.push_back(section);
      sectionIndexes.push_back(contents.length());

      contents += "[" + section + "]\n";
      contents += newContents;
      }
   ofstream out(fileName.c_str());
   out << contents;
   }
// ------------------------------------------------------------------
// Write a string to ini file.
// ------------------------------------------------------------------
void IniFile::write(const string& section, const string& key, const string& value)
        {
   doBackup();
   if (value == "")
      deleteKey(section, key);
   else
      {
      vector<string> values;
      values.push_back(value);
      write(section, key, values);
      }
   }
// ------------------------------------------------------------------
// Write a string list to ini file.
// ------------------------------------------------------------------
void IniFile::write(const string& section, const string& key,
                    const vector<string>& values)
        {
   doBackup();
   size_t insertPos;
   string contents;
   readSection(section, contents);
   Indexes indexes;
   if (findLinePosForKeys(contents, key, indexes, true))
      {
      insertPos = indexes[0].first;
      deleteLinePos(contents, indexes);
      }
   else
      insertPos = contents.length();

   // insert new key lines.
   string newContents;
   for (unsigned i = 0; i != values.size(); i++)
      newContents += key + " = " + values[i] + "\n";
   contents.insert(insertPos, newContents);
   writeSection(section, contents);
   }
// ------------------------------------------------------------------
// Delete all keys that match key from the specified section.  This
// method handles the situation where keys may exist multiple times
// in a section.
// ------------------------------------------------------------------
void IniFile::deleteKey(const string& section, const string& key)
        {
   doBackup();
   string contents;
   readSection(section, contents);
   Indexes indexes;
   if (findLinePosForKeys(contents, key, indexes, true))
      {
      deleteLinePos(contents, indexes);
      writeSection(section, contents);
      }
   }
// ------------------------------------------------------------------
// Delete the section from the .ini file.  Return true if section was
// deleted.
// ------------------------------------------------------------------
void IniFile::deleteSection(const string& section)
        {
   doBackup();
   vector<string>::iterator i = find(sectionNames.begin(),
                                     sectionNames.end(),
                                     section);
   if (i != sectionNames.end())
      {
      vector<string>::iterator nextI = i + 1;
      size_t posStart = sectionIndexes[i - sectionNames.begin()];
      size_t numCharsToDelete;
      if (nextI == sectionNames.end())
         numCharsToDelete = string::npos;

      else
         {
         numCharsToDelete = sectionIndexes[nextI - sectionNames.begin()] - posStart;
         int delta = numCharsToDelete;
         updateIndexesAfter(*i, -delta);
         }
      int Indx = std::distance(sectionNames.begin(), i);
      sectionNames.erase(i);
      sectionIndexes.erase(sectionIndexes.begin() + Indx);

      contents.erase(posStart, numCharsToDelete);
      ofstream out(fileName.c_str());
      out << contents;
      }

   }
// ------------------------------------------------------------------
// Return a complete list of all keys in the specified section.
// ------------------------------------------------------------------
void IniFile::getKeysInSection(const string& section,
                               vector<string>& keys) const
        {
   string sectionContents;
   readSection(section, sectionContents);

   // loop through all lines in contents and store in key_names and key_values.
   istringstream in(sectionContents);
   string line;
   while (getline(in, line, '\n'))
      {
      if (line != "")
         {
         stripComments(line);
         string keyFromSection;
         string value;
         getKeyNameAndValue(line, keyFromSection, value);
         if (keyFromSection != "")
            keys.push_back(keyFromSection);
         }
      }
   }
// ------------------------------------------------------------------
// rename the specified section
// ------------------------------------------------------------------
void IniFile::renameSection(const string& oldSection,
                            const string& newSection)
        {
   doBackup();
   vector<string>::iterator i = find(sectionNames.begin(),
                                     sectionNames.end(),
                                     oldSection);
   if (i != sectionNames.end())
      {
      updateIndexesAfter(oldSection, newSection.length() - oldSection.length());
      *i = newSection;

      //unsigned z = i-sectionNames.begin();
      size_t posSectionName = contents.find(oldSection,
                                              sectionIndexes[i-sectionNames.begin()]);
      contents.replace(posSectionName, oldSection.length(), newSection);
      ofstream out(fileName.c_str());
      out << contents;
      }
   }
// ------------------------------------------------------------------
// rename the specified key.  Return true if key was modified.
// ------------------------------------------------------------------
bool IniFile::renameKey(const std::string& section,
                        const std::string& oldKey,
                        const std::string& newKey)
   {
   doBackup();
   string sectionContents;
   readSection(section, sectionContents);
   Indexes indexes;
   if (findLinePosForKeys(sectionContents, oldKey, indexes, true))
      {
      for (unsigned i = indexes.size(); i > 0; i--)
         {
         string line = sectionContents.substr(indexes[i-1].first, indexes[i-1].second);
         string key, value;
         getKeyNameAndValue(line, key, value);
         line = newKey + " = " + value;
         sectionContents.replace(indexes[i-1].first, indexes[i-1].second, line);
         }
      writeSection(section, sectionContents);
      return true;
      }
   return false;
   }
// ------------------------------------------------------------------
// Perform a backup if we haven't already done so.
// ------------------------------------------------------------------

void IniFile::doBackup()
   {
   if (!haveDoneBackup)
      {
      std::string backupFileName = fileName + ".old";
      if (boost::filesystem::exists(backupFileName))
         boost::filesystem::remove(backupFileName);
         	
      boost::filesystem::copy_file(fileName, backupFileName);
      haveDoneBackup = true;
      }
   }

