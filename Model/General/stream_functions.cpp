#include <stdlib.h>
#include <string.h>
#include <General/stream_functions.h>
#include <sstream>
using namespace std;
// ------------------------------------------------------------------
//  Short description:
//    Read in a token.  Skip all leading skip characters and stop when
//    a delimiter character is found.  Returns the character that caused
//    the parsing to stop.

//  Notes:

//  Changes:
//    DPH 21/11/94
//    DPH 17/4/1997 - moved to C++ builder.

// ------------------------------------------------------------------
char Read_token(istream& In_stream,
                const char* Init_skip_chars,
                const char* Delimiter_chars,
                string& Token)
   {
   // Clear the strings contents
   Token = "";

   // Skip all initial characters.
   bool Still_skipping_init = true;
   char Ch[2];
   while (!In_stream.eof() && Still_skipping_init)
      {
      // get next character from stream.
      In_stream.get((char*) &Ch, 2, 0);

      // only continue skipping characters if the char. just extracted
      // is in the Init_skip_chars string.
      if (strlen(Ch) == 0)
         Still_skipping_init = false;

      else if (strchr(Init_skip_chars, Ch[0]) != NULL)
         Still_skipping_init = true;

      else
         Still_skipping_init = false;
      }

   // Append characters to string until a delimiter is found.
   while (strlen(Ch) != 0 &&
          strchr(Delimiter_chars, Ch[0]) == NULL &&
          In_stream)
      {
      Token += Ch[0];
      In_stream.get((char*) &Ch, 2, 0);
      }

   // If reached end of stream then return 0;

   if (!In_stream)
      return 0;

   return Ch[0];
   }

// ------------------------------------------------------------------
//  Short description:
//     Read in entire contents of a stream and return to caller.

//  Notes:
//     I have used the non-standard read_file method of the string
//     class. If we ever move to the STL version of string then
//     this routine will have to be re-worked.

//  Changes:
//    DPH 8/10/97

// ------------------------------------------------------------------
void Read_stream(istream& In_stream, string& Contents)
   {
   getline (In_stream, Contents, '\0');
   }


// ------------------------------------------------------------------
//  Short description:
//     write contents of input stream to output stream converting
//     to CSV (comma separated values)

//  Notes:
//     assumes that input and output streams are both open.

//  Changes:
//    DPH 8/10/97

// ------------------------------------------------------------------
void Convert_2_CSV(istream& In_stream, ostream& Out_stream)
   {
   string Variable;
   string Line;
   getline(In_stream, Line);
   while (In_stream)
      {
      istringstream Line_stream((char*) Line.c_str());
      Line_stream >> Variable;
      while (Line_stream)
         {
         Out_stream << Variable << ',';
         Line_stream >> Variable;
         }
      Out_stream << endl;
      getline(In_stream, Line);
      }
   }

