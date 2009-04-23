//---------------------------------------------------------------------------

#ifndef StringTokenizerH
#define StringTokenizerH
#include <string>
#include <General/platform.h>

// ------------------------------------------------------------------
//  Short description:
//   The string tokenizer class allows an application to break a
//   string into tokens. The tokenization method is much simpler than
//   the one used by the StreamTokenizer class. The
//   StringTokenizer methods do not distinguish among
//   identifiers, numbers, and quoted strings, nor do they recognize
//   and skip comments.
//
//   The set of delimiters (the characters that separate tokens) may
//   be specified either at creation time or on a per-token basis.

//  Notes:
//   An instance of StringTokenizer behaves in one of two
//   ways, depending on whether it was created with the
//   returnTokens flag having the value true or false:
//
//   If the flag is false, delimiter characters serve to
//     separate tokens. A token is a maximal sequence of consecutive
//     characters that are not delimiters.
//   If the flag is true, delimiter characters are considered to
//     be tokens. A token is either one delimiter character, or a maximal
//     sequence of consecutive characters that are not delimiters.
//
//   The following is one example of the use of the tokenizer. The code:
//     StringTokenizer st = new StringTokenizer("this is a test");
//     while (st.hasMoreTokens()) {
//         cout << st.nextToken() << endl;
//     }
//
//   prints the following output:
//     this
//     is
//     a
//     test

//  Changes:
//    DPH 25/8/2000

// ------------------------------------------------------------------
class EXPORT StringTokenizer
   {
   private:
      unsigned int currentPosition;
      unsigned int maxPosition;
      std::string str;
      std::string delimiters;
      bool retTokens;

    /*
     * Skips delimiters.
     */
      void skipDelimiters();

    /*
     * Return the start and end position of the next token
     * starting from the specified currpos.
     * Will return a start position of string::npos when
     * no more tokens can be read
     */
      void calcPosNextToken(unsigned& start, unsigned& end);

   public:
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
     StringTokenizer(const std::string& str,
                     const std::string& delim,
                     bool returnTokens = false);

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
     StringTokenizer(const std::string& str,
                     unsigned pos,
                     const std::string& delim,
                     bool returnTokens = false);

    /*
     * Tests if there are more tokens available from this tokenizer's string.
     *
     * @return  true if there are more tokens available from this
     *          tokenizer's string; false otherwise.
     */
     bool hasMoreTokens();

    /*
     * Returns the next token from this string tokenizer.
     *
     * @return     the next token from this string tokenizer.
     * @exception  NoSuchElementException  if there are no more tokens in this
     *               tokenizer's string.
     */
    std::string nextToken();

    /*
     * Returns the next token in this string tokenizer's string. The new
     * delimiter set remains the default after this call.
     *
     * @param      delim   the new delimiters.
     * @return     the next token, after switching to the new delimiter set.
     * @exception  NoSuchElementException  if there are no more tokens in this
     *               tokenizer's string.
     */
    std::string nextToken(const std::string& delim);

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
     bool hasMoreElements();

    /*
     * Calculates the number of times that this tokenizer's
     * nextToken method can be called before it generates an
     * exception.
     *
     * @return  the number of tokens remaining in the string using the current
     *          delimiter set.
     * @see     java.util.StringTokenizer#nextToken()
     */
     int countTokens();

    /*
     * Return the current position in string.
    */
     unsigned currentPos(void) const {return currentPosition;}


   };
#endif
