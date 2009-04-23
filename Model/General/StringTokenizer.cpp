//---------------------------------------------------------------------------
#include <stdlib.h>
#include "StringTokenizer.h"
using namespace std;
// ------------------------------------------------------------------
//  Short description:
    /*
     * Constructs a string tokenizer for the specified string. The
     * characters in the delim argument are the delimiters
     * for separating tokens.
     *
     * If the returnTokens flag is true, then
     * the delimiter characters are also returned as tokens. Each
     * delimiter is returned as a string of length one. If the flag is
     * false, the delimiter characters are skipped and only
     * serve as separators between tokens.
     *
     * str            a string to be parsed.
     * delim          the delimiters.
     * returnTokens   flag indicating whether to return the delimiters
     *                as tokens.
     */

//  Notes:

//  Changes:
//    DPH 25/8/2000

// ------------------------------------------------------------------
StringTokenizer::StringTokenizer(const string& st,
                                 const string& delim,
                                 bool returnTokens)
   {
   currentPosition = 0;
   str = st;
   maxPosition = str.length();
   delimiters = delim;
   retTokens = returnTokens;
   }
// ------------------------------------------------------------------
    /*
     * Constructs a string tokenizer for the specified string but
     * only after the specified position.  The
     * characters in the delim argument are the delimiters
     * for separating tokens.
     *
     * If the returnTokens flag is true, then
     * the delimiter characters are also returned as tokens. Each
     * delimiter is returned as a string of length one. If the flag is
     * false, the delimiter characters are skipped and only
     * serve as separators between tokens.
     *
     * str            a string to be parsed.
     * delim          the delimiters.
     * returnTokens   flag indicating whether to return the delimiters
     *                as tokens.
     */
// ------------------------------------------------------------------
StringTokenizer::StringTokenizer(const string& st,
                                 unsigned pos,
                                 const string& delim,
                                 bool returnTokens)
   {
   currentPosition = pos;
   str = st;
   maxPosition = str.length();
   delimiters = delim;
   retTokens = returnTokens;
   }

// ------------------------------------------------------------------
//  Short description:
    /*
     * Tests if there are more tokens available from this tokenizer's string.
     *
     * @return  true if there are more tokens available from this
     *          tokenizer's string; false otherwise.
     */

//  Notes:

//  Changes:
//    DPH 25/8/2000

// ------------------------------------------------------------------
bool StringTokenizer::hasMoreTokens()
   {
//   skipDelimiters();
   return (currentPosition < maxPosition);
   }

// ------------------------------------------------------------------
//  Short description:
    /*
     * Returns the next token from this string tokenizer.
     *
     * @return     the next token from this string tokenizer.
     * @exception  NoSuchElementException  if there are no more tokens in this
     *               tokenizer's string.
     */

//  Notes:

//  Changes:
//    DPH 25/8/2000

// ------------------------------------------------------------------
string StringTokenizer::nextToken()
   {
   unsigned int start = currentPosition;
   calcPosNextToken(start, currentPosition);
   if (start != string::npos)
      return str.substr(start, currentPosition-start);
   else
      return "";
   }

// ------------------------------------------------------------------
//  Short description:
    /*
     * Returns the next token in this string tokenizer's string. The new
     * delimiter set remains the default after this call.
     *
     * @param      delim   the new delimiters.
     * @return     the next token, after switching to the new delimiter set.
     * @exception  NoSuchElementException  if there are no more tokens in this
     *               tokenizer's string.
     */

//  Notes:

//  Changes:
//    DPH 25/8/2000

// ------------------------------------------------------------------
string StringTokenizer::nextToken(const string& delim)
   {
   delimiters = delim;
   return nextToken();
   }

// ------------------------------------------------------------------
//  Short description:
    /*
     * Returns the same value as the hasMoreTokens
     * method. It exists so that this class can implement the
     * Enumeration interface.
     *
     * @return  true if there are more tokens;
     *          false otherwise.
     * @see     java.util.Enumeration
     * @see     java.util.StringTokenizer#hasMoreTokens()
     */

//  Notes:

//  Changes:
//    DPH 25/8/2000

// ------------------------------------------------------------------
bool StringTokenizer::hasMoreElements()
   {
   return hasMoreTokens();
   }

// ------------------------------------------------------------------
//  Short description:
    /*
     * Calculates the number of times that this tokenizer's
     * nextToken method can be called before it generates an
     * exception.
     *
     * @return  the number of tokens remaining in the string using the current
     *          delimiter set.
     * @see     java.util.StringTokenizer#nextToken()
     */

//  Notes:

//  Changes:
//    DPH 25/8/2000

// ------------------------------------------------------------------
int StringTokenizer::countTokens()
   {
   int count = 0;
   unsigned start = currentPosition;
   unsigned end = 0;
   while (start != string::npos)
      {
      calcPosNextToken(start, end);
      if (start != string::npos)
         {
         start = end;
         count++;
         }
      }
   return count;
   }

// ------------------------------------------------------------------
//  Short description:
    /*
     * Return the start and end position of the next token.
     * end will actually point to one past the end of the token.
     * Will return a start position of string::npos when
     * no more tokens can be read
     */

//  Notes:

//  Changes:
//    DPH 25/8/2000

// ------------------------------------------------------------------
void StringTokenizer::calcPosNextToken(unsigned& start,
                                       unsigned& end)
   {
   // check for past end of string
   if (start == str.length())
      start = string::npos;

   // skip leading delimiters.
   if (!retTokens)
      start = str.find_first_not_of(delimiters, start);

   // find end delimiter.
   end = str.find_first_of(delimiters, start);

   if (retTokens && start == end)
      end++;
   }

