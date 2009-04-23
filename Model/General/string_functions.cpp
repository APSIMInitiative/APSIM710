#include <stdlib.h>

#include <string>
#include <sstream>
#include <iomanip>

#include <General/string_functions.h>
#include "stristr.h"

#include <cctype>
#include <stdexcept>
#include <stdlib.h>
using namespace std;
// ------------------------------------------------------------------
// removes leading and trailing characters.
// ------------------------------------------------------------------
void stripLeadingTrailing(string& text, const string& separators)
   {
   size_t Pos;

   // remove leading spaces
   Pos = text.find_first_not_of(separators);
   if (Pos > 0)
      text.replace (0, Pos, "");

   // remove trailing spaces  strrchr
   text = "`" + text;
   Pos = text.find_last_not_of(separators);
   if (Pos < text.length())
      text.replace (Pos+1, string::npos, "");
   text.replace (0, 1, "");
   }

// ------------------------------------------------------------------
//  Short description:
//    removes leading and trailing characters.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Strip (char* text, const char* separators)
   {
   string s;
   s = text;
   stripLeadingTrailing(s, separators);
   strcpy(text, s.c_str());
   }

// ------------------------------------------------------------------
// Split off a substring delimited with the specified character and
// return substring.
// ------------------------------------------------------------------
std::string splitOffAfterDelimiter(std::string& value, const std::string& delimiter)
   {
   string returnString;

   unsigned pos = value.find(delimiter);
   if (pos != string::npos)
      {
      returnString = value.substr(pos + delimiter.length());
      value.erase(pos);
      }
   return returnString;
   }
// ------------------------------------------------------------------
// Split off a bracketed value from the end of the specified string.
// The bracketed value is then returned, without the brackets,
// or blank if not found.
// ------------------------------------------------------------------
string splitOffBracketedValue(string& value,
                              char openBracket, char closeBracket)
   {
   string returnString;

   unsigned posCloseBracket = value.find_last_not_of(" ");
   if (posCloseBracket != string::npos && value[posCloseBracket] == closeBracket)
      {
      unsigned posOpenBracket = value.find_last_of(openBracket, posCloseBracket-1);
      if (posOpenBracket != string::npos)
         {
         returnString = value.substr(posOpenBracket+1, posCloseBracket-posOpenBracket-1);
         value.erase(posOpenBracket);
         stripLeadingTrailing(returnString, " ");
         }
      }
   return returnString;
   }
// ------------------------------------------------------------------
//  Short description:
//    Return true if string passed in is numerical.  False otherwise.

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
bool Is_numerical (const char* Text)
   {
   if (*Text=='\0') throw std::runtime_error("Unexpected empty string.");

   char *endptr;
   strtod(Text, &endptr);
   return (*endptr == '\0');
   }

// ------------------------------------------------------------------
//  Short description:
//    converts a string to lowercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
void To_lower (string& St)
   {
   std::transform(St.begin(), St.end(), St.begin(), ::tolower);
   }

std::string ToLower(const std::string& st)
   {
   std::string lowerSt = st;
   To_lower(lowerSt);
   return lowerSt;
   }


// ------------------------------------------------------------------
//  Short description:
//    converts a string to lowercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
void To_upper (string& St)
   {
   std::transform(St.begin(), St.end(), St.begin(), ::toupper);
   }

// ------------------------------------------------------------------
//  Short description:
//     function that takes a string and replaces all occurrances of
//     the substring with the replacement string.  Return true if
//     a replacement was made.
//  Notes:

//  Changes:
//    DPH 17/3/97
//    dph 27/3/98 changed string::npos to string::string::npos in line with standard.

// ------------------------------------------------------------------
bool Replace_all (string& St, const char* Sub_string, const char* Replacement_string)
   {
   if(*Sub_string=='\0')
       throw std::runtime_error("Empty search string.");

   bool replacementMade = false;
   size_t Pos = St.find(Sub_string);
   while (Pos != string::npos)
      {
      St.replace(Pos, strlen(Sub_string), Replacement_string);
      replacementMade = true;
      Pos = St.find(Sub_string);
      }
   return replacementMade;
   }
// ------------------------------------------------------------------
// function that takes a string and replaces all occurrances of
// the substring with the replacement string after the given position.
// Return true if a replacement was made.
// ------------------------------------------------------------------
bool replaceAll(string& St, unsigned position, const string& subString, const string& replacementString)
   {
   if (subString=="")
       throw std::runtime_error("Empty search string.");
   bool replacementMade = false;
   char* pos = (char*)St.c_str();
   pos += position;
   pos = stristr(pos, subString.c_str());
   while (pos != NULL)
      {
      unsigned charPos = pos - St.c_str();
      St.replace(charPos, subString.length(), replacementString);
      replacementMade = true;
      pos = stristr(pos, subString.c_str());
      }
   return replacementMade;
   }

// ------------------------------------------------------------------
// function that takes a string and replaces all occurrances of
// the substring with the replacement string.  Case insensitive.
// Return true if a replacement was made.
// ------------------------------------------------------------------
bool replaceAll(string& St, const string& subString, const string& replacementString)
   {
   if (subString=="")
       throw std::runtime_error("Empty search string.");
   bool replacementMade = false;
   char* pos = stristr(St.c_str(), subString.c_str());
   while (pos != NULL)
      {
      unsigned Pos = pos - St.c_str();
      St.replace(Pos, subString.length(), replacementString);
      replacementMade = true;
      pos = stristr(St.c_str(), subString.c_str());
      }
   return replacementMade;
   }
// ------------------------------------------------------------------
//  Short description:
//     convert a double to a string.

//  Notes:

//  Changes:
//    DPH 17/3/97
//    dph 27/3/98 changed npos to string::npos in line with standard.

// ------------------------------------------------------------------
string ftoa(double Float, int Num_decplaces)
   {
   ostringstream buf;
   buf.setf(std::ios::fixed, std::ios::floatfield);
   buf << std::setprecision(Num_decplaces) << Float;
   return buf.str();
   }
// ------------------------------------------------------------------
//  Short description:
//     convert a Integer to a string.
// ------------------------------------------------------------------
string itoa(int intValue)
   {
   char buffer[100];
   sprintf(buffer, "%d", intValue);
   return buffer;
   }

// ------------------------------------------------------------------
//  Short description:
//     case insensitive string comparison routine.

//  Notes:

//  Changes:
//    SB ????

// ------------------------------------------------------------------
int Str_i_Cmp(const string &a, const string &b)
   {
   #ifdef __WIN32__
      return stricmp(a.c_str(),b.c_str());
   #else
      return strcasecmp(a.c_str(),b.c_str());
   #endif
   }


// ------------------------------------------------------------------
//  Short description:
//     replace all chars in a give string with a replacement.  Can
//     handle a NULL char as a replacement char.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void Replace_all_chars (char* St, char Char_to_replace, char Replacement_char)
   {
   char* ptr = St;
   ptr = strchr(ptr, Char_to_replace);
   while (ptr != NULL)
      {
      *ptr = Replacement_char;
      ptr = strchr(ptr + 1, Char_to_replace);
      }
   }

// ------------------------------------------------------------------
// Helper function - Get a section name from the specified line.
// ie look for [section] on the line passed in.
// Returns name if found.  Blank otherwise.
// ------------------------------------------------------------------
   string getSectionName(const std::string& line)
      {
      string section;
      unsigned int posOpen = line.find_first_not_of (" \t");
      if (posOpen != string::npos && line[posOpen] == '[')
         {
         int posClose = line.find(']');
         if (posClose != string::npos)
            section = line.substr(posOpen+1, posClose-posOpen-1);
         }
      stripLeadingTrailing(section, " ");
      return section;
      }
// ------------------------------------------------------------------
// Get a value from an .ini line. ie look for keyname = keyvalue
// on the line passed in.  Returns the value (keyvalue) if found or blank otherwise.
// ------------------------------------------------------------------
   string getKeyValue(const string& line, const string& key)
      {
      string keyFromLine;
      string valueFromLine;
      getKeyNameAndValue(line, keyFromLine, valueFromLine);
      if (Str_i_Eq(keyFromLine, key))
         return valueFromLine;
      else
         return "";
      }
// ------------------------------------------------------------------
// Return the key name and value on the line.
// ------------------------------------------------------------------
   void getKeyNameAndValue(const string& line, string& key, string& value)
      {
      int posEquals = line.find('=');
      if (posEquals != string::npos)
         {
         key = line.substr(0, posEquals);
         To_lower(key);
         stripLeadingTrailing(key, " ");
         value = line.substr(posEquals+1);
         stripLeadingTrailing(value, " ");
         }
      else
         {
         key = "";
         value = "";
         }
      }

   void getKeyNameValueUnits(const string& line, string& key, string& value, string& units)
      {
      int posEquals = line.find('=');
      if (posEquals != string::npos)
         {
         key = line.substr(0, posEquals);
         To_lower(key);
         stripLeadingTrailing(key, " ");
         value = line.substr(posEquals+1);
         int posBracket = value.find('(');
         if (posBracket != string::npos)
            {
            units = value.substr(posBracket+1);
            stripLeadingTrailing(units, " ");
            value = value.substr(0,posBracket);
            }
         else
            {
            units = "";
            }
         stripLeadingTrailing(value, " ");
         }
      else
         {
         key = "";
         value = "";
         units = "";
         }
      }
// ------------------------------------------------------------------
// Locate a substring within a string - case insensitive.
// ------------------------------------------------------------------
unsigned findSubString(const std::string& st, const std::string& subString)
   {
   if (subString=="")
       return string::npos;
   char* posChar = stristr(st.c_str(), subString.c_str());
   if (posChar == NULL)
      return string::npos;
   else
      return posChar - st.c_str();
   }

// -----------------------------------------------------------
// Find a matching bracket, allowing for nested brackets.
// -----------------------------------------------------------
unsigned matchBracket(const std::string& st, char openBracket, char closeBracket,
                      unsigned startPos)
   {
   unsigned pos = startPos + 1;
   int matchCount = 1;
   while (pos < st.length())
      {
      if (st[pos] == openBracket)
         matchCount++;
      else if (st[pos] == closeBracket)
         matchCount--;
      if (matchCount == 0)
         return pos;
      pos++;
      }
   return string::npos;
   }

// ------------------------------------------------------------
// return a string of spaces for the specified level of indent.
// ------------------------------------------------------------
std::string indentString(int level)
   {
   return string(level*3, ' ');
   }

void addAttributeToXML(std::string& XML, const std::string& Attribute)
   // -----------------------------------------------------------------------
   // Add an attribute (e.g. unit="g/m2") to the end of an
   // xml string (e.g. "<type name="biomass"/>)
   // -----------------------------------------------------------------------
   {
   unsigned PosEnd = XML.rfind("/>");
   if (PosEnd == string::npos)
      throw runtime_error("Invalid XML string: " + XML);

   XML.insert(PosEnd, " " + Attribute);
   }

// -----------------------------------------------------------------------
// Get an attribute from an xml string (e.g. unit="g/m2")
// -----------------------------------------------------------------------
std::string getAttributeFromXML(const std::string& XML, const std::string& attributeName)
   {
   unsigned posEnd = XML.find(">");
   unsigned posAttribute = XML.find(attributeName);
   if (posAttribute != string::npos && posAttribute < posEnd)
      {
      unsigned posOpenQuote = XML.find('\"', posAttribute);
      if (posOpenQuote != string::npos )
         {
         unsigned posCloseQuote = XML.find('\"', posOpenQuote+1);
         if (posCloseQuote != string::npos)
            {
            return XML.substr(posOpenQuote+1, posCloseQuote-posOpenQuote-1);
            }
         }
      }
   else
      return "";
   return "";
   }

// ------------------------------------------------------------------
// single quote the string passed in.
// ------------------------------------------------------------------
std::string singleQuoted(const std::string& st)
   {
   return "'" + st + "'";
   }

// ------------------------------------------------------------------
// double quote the string passed in.
// ------------------------------------------------------------------
std::string doubleQuoted(const std::string& st)
   {
   return "\"" + st + "\"";
   }
