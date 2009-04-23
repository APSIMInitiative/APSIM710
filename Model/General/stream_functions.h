#if !defined (STREAM_FUNCTIONS_H)
#define STREAM_FUNCTIONS_H

#include <string>
#include <iostream.h>
#include "platform.h"
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
char Read_token(std::istream& In_stream,
                const char* Init_skip_chars,
                const char* Delimiter_chars,
                std::string& Token);

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
void Read_stream(std::istream& In_stream, std::string& Contents);

// ------------------------------------------------------------------
//  Short description:
//     write contents of input stream to output stream converting
//     to CSV (comma separated values).

//  Notes:
//     assumes that input and output streams are both open.

//  Changes:
//    DPH 8/10/97

// ------------------------------------------------------------------
void EXPORT Convert_2_CSV(std::istream& In_stream, std::ostream& Out_stream);


#endif


