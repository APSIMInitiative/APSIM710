#ifndef STRING_FUNCTIONSH
#define STRING_FUNCTIONSH

#include <string>
#include <list>
#include <vector>
#include <algorithm>
#include <functional>
#include <sstream>
#include <General/platform.h>

#include <iterator>

// ------------------------------------------------------------------
//  Short description:
//    splits a string into words.

//  Notes:
       
//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class container>
void Split_string (const std::string& text, const char* separators, container& words)
   {
   words.erase(words.begin(), words.end());

        int n = text.length();
   int start, stop;

   start = text.find_first_not_of(separators);

   while ((start >= 0) && (start < n))
      {
                stop = text.find_first_of(separators, start);
                if ((stop < 0) || (stop > n)) stop = n;
                words.push_back(text.substr(start, stop - start));
                start = text.find_first_not_of(separators, stop+1);
                }
   }

// ------------------------------------------------------------------
//  Short description:
//     This method splits values on a comma but also honours double quotes
//     ensuring something in double quotes is never split.

//  Notes:
//     eg: if text = value1, "value 2, 2a", value3
//     then: words[0] = value1
//           words[1] = value2, 2a
//           words[2] = value3

//  Changes:
//    DPH 21/7/2000

// ------------------------------------------------------------------
template <class container>
void SplitStringHonouringQuotes(const std::string& text,
                                const char* separators,
                                container& words) 
   {
   words.erase(words.begin(), words.end());

   std::string separatorsAndQuote = separators + std::string("\"");
   std::string separatorsAndSpace = separators + std::string(" ");
        unsigned int n = text.length();
   unsigned int start, stop;

   start = text.find_first_not_of(separatorsAndSpace);

   while (start < n)
      {
      if (text[start] == '\"')
         {
         stop = text.find("\"", start+1);
         if (stop == std::string::npos)
            throw std::string("Mismatched quotes in string: " + text);
         stop++;
         }
      else
         stop = text.find_first_of(separatorsAndQuote, start);
                if (stop > n) stop = n;
                words.push_back(text.substr(start, stop - start));
                start = text.find_first_not_of(separatorsAndSpace, stop+1);
                }
   }

// ------------------------------------------------------------------
//  Short description:
//    builds up a string from a list of words.

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    dph 15/5/98 fixed bug to remove last occurrence of separator.

// ------------------------------------------------------------------
template <class container>
void Build_string (container& words, const char* separators, std::string& text)
   {
   text = "";
   for (typename container::iterator Iter = words.begin();
                               Iter != words.end();
                               Iter++)
      {
      if (Iter != words.begin())
         text += separators;
      text += *Iter;
      }
   }

// ------------------------------------------------------------------
// builds up a string from a container of items.
// ------------------------------------------------------------------
template <class T>
std::string buildString(const std::vector<T>& values, const char* separators)
   {
   std::ostringstream st;
   std::ostream_iterator<T, char> out(st, separators);
   std::copy(values.begin(), values.end(), out);
   return st.str();
   }
// ------------------------------------------------------------------
// splits a string into words.
// ------------------------------------------------------------------
template <class container>
void split(const std::string& text, const std::string& separators, container& words)
   {
   unsigned n = text.length();
   unsigned start, stop;

   start = text.find_first_not_of(separators);

   while ((start >= 0) && (start < n))
      {
      stop = text.find_first_of(separators, start);
      if ((stop < 0) || (stop > n)) stop = n;
      words.push_back(text.substr(start, stop - start));
      start = text.find_first_not_of(separators, stop+1);
      }
   }

// ------------------------------------------------------------------
// splits a string into words.
// ------------------------------------------------------------------
template <class container>
void splitIntoValues(const std::string& text, const std::string& separators, container& words)
   {
   words.erase(words.begin(), words.end());
   split<container>(text, separators, words);
   }

// ------------------------------------------------------------------
// Split off a substring delimited with the specified character and
// return substring. Returns blank if not found.
// ------------------------------------------------------------------
std::string EXPORT splitOffAfterDelimiter(std::string& value, const std::string& delimiter);

// ------------------------------------------------------------------
// Split off a bracketed value from the end of the specified string.
// The bracketed value is then returned, without the brackets,
// or blank if not found.
// ------------------------------------------------------------------
std::string EXPORT splitOffBracketedValue(std::string& valueString,
                                          char openBracket, char closeBracket);

// ------------------------------------------------------------------
// removes leading and trailing characters.
// ------------------------------------------------------------------
void EXPORT stripLeadingTrailing(std::string& text, const std::string& separators);

// ------------------------------------------------------------------
//  Short description:
//    Return true if string passed in is numerical.  False otherwise.

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
bool EXPORT Is_numerical (const char* Text);

// ------------------------------------------------------------------
//  Short description:
//    converts a string to uppercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
void EXPORT To_upper (std::string& St);

// ------------------------------------------------------------------
//  Short description:
//    converts a string to lowercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
void EXPORT To_lower (std::string& St);

std::string EXPORT ToLower(const std::string& st);

// ------------------------------------------------------------------
//  Short description:
//    converts all strings in container to lowercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
template <class StringContainer>
void Strings_to_lower (StringContainer& words)
   {
   typename StringContainer::iterator Iter;
   for (Iter = words.begin();
                                   Iter != words.end();
                                   Iter++)
      {
      To_lower (*Iter);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    locate a string in a container.  All comparisons are case
//    insensitive.

//  Notes:
//    returns -1 if string not found

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class StringContainer>
int Locate_string (const char* Search_string, StringContainer& words)
   {
   bool Found = false;
   int indx = -1;
   typename StringContainer::iterator Iter;
   for (Iter = words.begin();
                                   Iter != words.end() && !Found;
                                   Iter++)
      {
      Found = (strcmpi(Search_string, (*Iter).c_str()) == 0);
      indx++;
      }
   if (!Found)
      indx = -1;
   return indx;
   }

#ifdef __WIN32__
// ------------------------------------------------------------------
//  Short description:
//     generic "for_each" function for trimming from the start of the
//     occurrance of substring to the end in a given stl container.

//  Usage: (JW)
//     use in conjunction with for_each(), end result = copy container
//     A to B with certain substrings in B removed, A remains the same.

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template <class Container >
class trim_from_substring_and_copy : private std::unary_function < std::string, void >
   {
   private:
      Container& container;
      std::string Substring;
   public:
      trim_from_substring_and_copy( const char* sub, Container& c )
         : Substring ( sub ), container(c) { }
      void operator(  ) ( const std::string& x )
         {
         std::string new_st(x);
         size_t pos = new_st.find(Substring);
         if (pos != std::string::npos)
            new_st.erase(pos);
         container.push_back (new_st);
         }
    };

#endif

// ------------------------------------------------------------------
//  Short description:
//     function that takes a string of numbers and returns a
//     stl container of doubles.

//  Notes:
//     string entries delimited by " " (space)

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container_type>
void String_2_double_container (const char* Numbers,
                                container_type& container)
   {
   container.erase (container.begin(), container.end());
   std::list<std::string> string_container;
   std::string Number_string(Numbers);
   Split_string (Number_string, " ", string_container);
   for (std::list<std::string>::iterator Iter = string_container.begin();
                               Iter != string_container.end();
                               Iter++)
      {
      container.push_back (atof ( (*Iter).c_str() ));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     function that takes a container of numbers and converts to a string.

//  Notes: (JW)
//     delimiter = " " (space)
//     'precision' indiciates number of decimal places

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container_type>
void Double_container_2_string (container_type& container,
                                std::string& Numbers,
                                int precision)
   {
   std::ostringstream number_stream;  
   number_stream.setf(std::ios::fixed, std::ios::floatfield);
   number_stream.precision (precision);

   std::list<std::string> string_container;
   typename container_type::iterator Iter;
   for (Iter = container.begin();
        Iter != container.end();
        Iter++)
      number_stream << *Iter << ' ';

   number_stream << std::ends;
   Numbers = number_stream.str();
   }
// ------------------------------------------------------------------
// Function that takes a string of numbers and returns a
// stl container of integers.
// NB: string entries delimited by " " (space)
// ------------------------------------------------------------------
template <class container_type>
void String_2_integer_container(const std::string& St, container_type& values)
   {
   values.erase (values.begin(), values.end());
   std::vector<std::string> string_container;
   splitIntoValues(St, " ", string_container);
   for (unsigned i = 0; i != string_container.size(); i++)
     values.push_back (atoi(string_container[i].c_str()));
   }
// ------------------------------------------------------------------
//  Short description:
//     function that takes a string and replaces all occurrances of
//     the substring with the replacement string.

//  Notes:

//  Changes:
//    DPH 17/3/98

// ------------------------------------------------------------------
bool EXPORT Replace_all (std::string& St, const char* Sub_string, const char* Replacement_string);

// ------------------------------------------------------------------
// function that takes a string and replaces all occurrances of
// the substring with the replacement string after the given position.
// Return true if a replacement was made.
// ------------------------------------------------------------------
bool EXPORT replaceAll(std::string& St, unsigned pos, const std::string& subString, const std::string& replacementString);

// ------------------------------------------------------------------
// function that takes a string and replaces all occurrances of
// the substring with the replacement string.  Case insensitive.
// Return true if a replacement was made.
// ------------------------------------------------------------------
bool EXPORT replaceAll(std::string& St, const std::string& subString, const std::string& replacementString);

// ------------------------------------------------------------------
//  Short description:
//     convert a double to a string.

//  Notes:

//  Changes:
//    DPH 17/3/97
//    dph 27/3/98 changed NPOS to string::npos in line with standard.

// ------------------------------------------------------------------
std::string EXPORT ftoa(double Float, int Num_decplaces);
std::string EXPORT itoa(int Int);

// ------------------------------------------------------------------
//  Short description:
//     case insensitive string comparison routines.

//  Notes:

//  Changes:
//    SB ???

// ------------------------------------------------------------------
int EXPORT Str_i_Cmp(const std::string &a, const std::string &b);
#define Str_i_Eq(a,b)  (!Str_i_Cmp((a),(b)))

// ------------------------------------------------------------------
//  Short description:
//     replace all chars in a give string with a replacement.  Can
//     handle a NULL char as a replacement char.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void EXPORT Replace_all_chars (char* St, char Char_to_replace, char Replacement_char);

// ------------------------------------------------------------------
// Helper function - Get a section name from the specified line.
// ie look for [section] on the line passed in.
// Returns name if found.  Blank otherwise.
// ------------------------------------------------------------------
std::string EXPORT getSectionName(const std::string& line);

// ------------------------------------------------------------------
// Get a value from an .ini line. ie look for keyname = keyvalue
// on the line passed in.  Returns the value if found or blank otherwise.
// ------------------------------------------------------------------
std::string EXPORT getKeyValue(const std::string& line, const std::string& key);

// ------------------------------------------------------------------
// Return the key name and value on the line.
// ------------------------------------------------------------------
void EXPORT getKeyNameAndValue(const std::string& line,
                               std::string& key,
                               std::string& value);

void EXPORT getKeyNameValueUnits(const std::string& line,
                                 std::string& key,
                                 std::string& value,
                                 std::string& units);

// ------------------------------------------------------------------
// single quote the string passed in.
// ------------------------------------------------------------------
std::string EXPORT singleQuoted(const std::string& st);

// ------------------------------------------------------------------
// double quote the string passed in.
// ------------------------------------------------------------------
std::string EXPORT doubleQuoted(const std::string& st);

// ------------------------------------------------------------------
// Locate a substring within a string - case insensitive.
// ------------------------------------------------------------------
unsigned EXPORT findSubString(const std::string& st, const std::string& substring);

// -----------------------------------------------------------
// Find a matching bracket, allowing for nested brackets.
// -----------------------------------------------------------
unsigned EXPORT matchBracket(const std::string& st, char openBracket, char closeBracket,
                             unsigned startPos);

// ------------------------------------------------------------
// return a string of spaces for the specified level of indent.
// ------------------------------------------------------------
std::string EXPORT indentString(int level);

// -----------------------------------------------------------------------
// Add an attribute (e.g. unit="g/m2") to the end of an
// xml string (e.g. "<type name="biomass"/>)
// -----------------------------------------------------------------------
void EXPORT addAttributeToXML(std::string& XML, const std::string& Attribute);

// -----------------------------------------------------------------------
// Get an attribute from an xml string (e.g. unit="g/m2")
// -----------------------------------------------------------------------
std::string EXPORT getAttributeFromXML(const std::string& XML, const std::string& AttributeName);

#endif

